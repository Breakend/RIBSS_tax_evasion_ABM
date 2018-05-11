rm(list = ls())

library(ggplot2)
library(Hmisc)
library(igraph)
library(reshape2)
library(Rcpp)
library(parallel)
library(doParallel)
library(dplyr)
library(TTR)

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

source("Library/library.R")

config.file = "Inputs/model.config.csv"
config = NULL
seed = 55279
population.data.file = "Inputs/PN1_population_data.csv"
pop.data = NULL
network.data.file = "Inputs/PN1_network_data.csv"
network.data.manipulation.file = "Inputs/ALP.network.degree.manipulations.Rdata"
deterrence.strategy.file = "Inputs/IRS Examination coverage in FY2015.csv"
tax.refund.proportions.file = "Inputs/Tax Refund Proportions.csv"
network.data = NULL
initial.state = NULL
final.year = NULL
return.final.state.not.outputs = FALSE
parallelize.tax.model = FALSE
cluster = NULL
display.plots = TRUE
save.plots = FALSE


set.seed(seed)
set.seed(55333)

if(!is.null(cluster)) #If cluster is given
{
  cl <- cluster
} else if(parallelize.tax.model) #If flag is set for parallelization 
{
  num.of.cores <- detectCores()
  cl <- makeCluster(num.of.cores, outfile="")
  registerDoParallel(cl)
} else { #No parallelization
  cl <- NULL
}

data.dir      <- "Tax data/"
figures.dir   <- "Writeup/Figures/"
survey.data.dir <- "Survey data/"


# Create model configurations from file if 'config' was not specified.
if(is.null(config))
  config <- read.csv(config.file, stringsAsFactors = F)

if(is.null(pop.data))
  pop.data <- get.population.data(population.data.file)

### RV
net.degree.info<-readRDS(file=network.data.manipulation.file)


### RV temporary hard coding need to be removed: 
# config[config$config.vars%in%"beta.network", "value"] <- 0.25
# config[config$config.vars%in%"beta.media", "value"] <- 0.25
# beta.personal <- 0.5
# tax.gap.reporting.media.threshold <- 0.1
# media.deterrence.above.avg.threshold.factor <- 1.25
# m.qP<- 0.25
# s.qP<- 0.72122228
# prob.additional.to.refund <- 0.1
# prob.refund.to.additional<- 0.05
# full.tendency.factor <- 1 ## vary this between 1 and 2 with peak at 1. 
# gamblers.fallacy.grad<- 0.07966
# gamblers.fallacy.intercept<- 0.15876
# bomb.crater.factor <- 0.58
# ave.degree.tTaxes <- 2.62

#beta.network <- (1-config[config$config.vars%in%"beta.personal", "value"])/2
#beta.media <- beta.network 

### need pop.data$actor

#Size of the population
N <- nrow(pop.data) 
#Overwrite the config population.size
config[config$config.vars == 'population.size', 'value'] <- N
ave.degree.tTaxes <- as.numeric(config[config$config.vars == 'ave.degree.tTaxes', 'value']) 

#Initializations
pop.data <- initialize.attributes(pop.data)
pop.data <- initialize.risk.perceptions(pop.data, config)

#Get IRS 
table.audit.rates <- read.csv(deterrence.strategy.file)
table.audit.rates<- table.audit.rates[-1,]
#pop.data$IRS.audit.target <- get.IRS.prob.of.audit(pop.data,NULL,table.audit.rates, additional.criteria=NULL)

#Get Refund tables 
table.refund <- read.csv(tax.refund.proportions.file)
pop.data$refund.group<- findInterval(pop.data$income,c(table.refund$Lower.Income,Inf))



  
### RV: this needs to be done by sampling the right c1 distribition.
c1.dist.weight<-get.config.param(config, 'c1.dist.weight')
pop.data$c1 <- pop.data$c1.tri.dist.dist*(1-c1.dist.weight)+
  pop.data$c1.alp.majority.fit*c1.dist.weight


if(is.null(network.data))
  network.data <- get.graph.from.network.data(network.data.file)

### RV set up the network and edgelist 
edges <- as.matrix(network.data[, c("id1", "id2")])
g <- graph_from_edgelist(el = edges)

g.info<-create.interacting.network.on.taxes(g,net.degree.info, pop.data, ave.degree.tTaxes)
#Overwriting g
g <- g.info$g
g.info$net.degree.info <- net.degree.info
nn<- g.info$nn

### Temporary declarations
final.year<- 100
v.PP <-0.9
phi <- (s.discount/(1-s.discount))*(v.PP/(1-v.PP))
V.max <- 0

t.media.range <-round(10*as.numeric(config[config$config.vars%in%"morale.half.life","value"]),0)
q.num.of.audits<- 0
q.recovered.revenue.plus.penalties <-0
seq.num.of.audits<- NULL
seq.recovered.revenue.plus.penalties<- NULL



mild.tendency.to.full.evasion<- TRUE
network.effect <- FALSE
tax.refund.effect <- FALSE
media.effect <- FALSE
final.year<- 100
#pop.data$IRS.audit.target[idxx] <- 0.005 
s.discount<-exp(-log(2)/2)
media.deterrence.above.avg.threshold.factor<-1.5
V.max <- 0

v.PP <-0.5
phi <- (s.discount/(1-s.discount))*(v.PP/(1-v.PP))

targetted.auditing<- T

audit.rate<- 0.008


### reset
ave.recovered.revenue.plus.penalties <- 311116.1
ave.num.of.audits <-10
track.recovered.revenue.plus.penalties<-NULL

t.media.range <-10*2
q.num.of.audits<- 0
q.recovered.revenue.plus.penalties <-0
seq.num.of.audits<- NULL
seq.recovered.revenue.plus.penalties<- NULL


set.seed(40600)

all.data <- run.dynamics(pop.data, initial.state = NULL, config, g.info, final.year, cl=cl)

summary(all.data$track.dyn[[1]]$per.audit.rate)
summary(all.data$track.dyn[[50]]$per.audit.rate)

summary(all.data$track.dyn[[50]]$per.audit.rate*all.data$track.dyn[[50]]$per.penalty.rate)/as.numeric(config[19,2])

if(return.final.state.not.outputs)
{
  retval <- all.data[['final.state']]
} else 
{
  #############################################################
  ###                                                       ###
  ###       Arrange Output Data           
  ###                                                       ###
  #############################################################
  track.id <- as.data.frame(bind_rows(all.data$track.dyn))
  aggregated.dyn <- as.data.frame(all.data[['aggregated.dyn']])  
  final.state <- all.data[['final.state']]
  sim.data <- with(final.state, data.frame(tax.ids=pop.data$tax.ids,
                                           c1=round(100*pop.data$c1,0),
                                           c1.tilde=round(100*c1.tilde,0),
                                           per.audit.rate=pop.data$per.audit.rate,
                                           per.penalty.rate=pop.data$per.penalty.rate,
                                           Delta.Morale = round(100*Delta.Morale ,0),
                                           w = round(100*w,0),
                                           freq.audits=freq.audits,
                                           freq.penalty=freq.penalty,
                                           years.since.last.compliant =years.since.last.compliant,
                                           years.since.last.audit=years.since.last.audit,
                                           years.since.last.penalty=years.since.last.penalty,
                                           amount.under.reporting.history,
                                           perc.reporting.history,
                                           compliant.history,
                                           audit.history,
                                           penalty.history))
  
  outputs <- generate.outputs(config, aggregated.dyn, sim.data, track.id, nn, cl=cl)
  
  final.year <- dim(aggregated.dyn)[1]
  R0<- rev(all.data$R0)[1:(final.year/2)]
  R1<- rev(all.data$R1)[1:(final.year/2)]
  R3<- rev(all.data$R3)[1:(final.year/2)]
  Rannoyed <- rev(all.data$Rannoyed)[1:(final.year/2)]
  mean.report.by.comp.aud.3yrs.ago <- all.data$mean.report.by.comp.aud.3yrs.ago
  
  prop.reported.by.audit0.in.50yrs <- ifelse(is.null(R0), NA, round(mean(R0[!is.nan(R0)]),2))
  prop.reported.by.audit1.in.50yrs <- ifelse(is.null(R1), NA, round(mean(R1[!is.nan(R1)]),2))
  prop.reported.by.audit3.in.50yrs <- ifelse(is.null(R3), NA, round(mean(R3[!is.nan(R3)]),2))
  Rannoyed <- ifelse(is.null(Rannoyed), NA, 100.00*round(mean(Rannoyed[!is.nan(Rannoyed)]),2))
  mean.report.by.comp.aud.3yrs.ago <- ifelse(is.null(mean.report.by.comp.aud.3yrs.ago), NA, mean.report.by.comp.aud.3yrs.ago)
  
  outputs <- cbind(outputs, prop.reported.by.audit0.in.50yrs, prop.reported.by.audit1.in.50yrs, 
                   prop.reported.by.audit3.in.50yrs, Rannoyed, mean.report.by.comp.aud.3yrs.ago)
  
  if(display.plots || save.plots)
  {
    plots <- create.plots(sim.data, track.id, aggregated.dyn, config, g)
    print(plots$c1.plot)
    print(plots$w.plot)
    print(plots$report.plot)
    print(plots$per.audit.rate.plot)
    print(plots$years.since.last.compliant.plot)
    print(plots$report.compliance.prob.plot)
    print(plots$sample.report.trajectory.plot)
    print(plots$iterative.map.plot)
    print(plots$aggregated.dyn.plot)
    
    #display.or.save.plots(plots, figures.dir, display.plots, save.plots)
    track.id.list <- split(track.id,track.id$tax.ids)
    
    print(paste("R0=",round(mean(R0[!is.nan(R0)]),2),sep=""))
    print(paste("target R0=",0.286*round(mean(R1[!is.nan(R1)]),2),sep=""))
    print(paste("R1=",round(mean(R1[!is.nan(R1)]),2),sep=""))
    print(paste("R3=",round(mean(R3[!is.nan(R3)]),2),sep=""))
    print(paste("target R3=",0.63*round(mean(R1[!is.nan(R1)]),2),sep=""))
    print(paste("Rannoyed=",100*round(mean(Rannoyed[!is.nan(Rannoyed)]),2),"% decrease in reported income",sep=""))
    print(paste("target Rannoyed=",35,"% decrease in reported income",sep=""))
    
    see.nn.info.at.time.t(track.id.list,i=626,nn,64)
  }
  
  retval <- as.data.frame(outputs)
  #     retval <- cbind.data.frame(t(config$value), retval)
  #     names(retval) <- c(config$config.vars, colnames(outputs))
  rownames(retval) <- NULL
}

if(parallelize.tax.model & is.null(cluster))
{
  #Stop the created cluster
  stopCluster(cl)
}
