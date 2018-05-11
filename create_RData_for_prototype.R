

#############################################
###                 Inputs                ###
#############################################
rm(list = ls())
model.type <- '1k' #or 10k


source("Library/library.R")

config.file = "Inputs/best.case.model.config.csv"
config = NULL
seed = 55279

if(model.type == '10k') {
  # population.data.file = "Inputs/PN10_population_data.csv"
  # network.data.file = "Inputs/PN10_network_data.csv"
  # load("Inputs/g_info_10.RData")
  load("SensitivityAnalysis/figures/BestCase_10k/best_case_10k_pre_eq.RData")
  retval <- retval.10k
} else {
  # population.data.file = "Inputs/PN1_population_data.csv"
  # network.data.file = "Inputs/PN1_network_data.csv"
  # g.info <- NULL
  load("SensitivityAnalysis/figures/BestCase_1k/best_case_1k_pre_eq.RData")
}

population.data = retval$population.data
network.data.manipulation.file = "Inputs/ALP.network.degree.manipulations.Rdata"
deterrence.strategy.file = "Inputs/IRS Examination coverage in FY2015.csv"
tax.refund.proportions.file = "Inputs/Tax Refund Proportions.csv"
network.data = retval$network.data
g.info <- retval$g.info
initial.state = retval$final.state
final.year = NULL
return.final.state.not.outputs = FALSE
parallelize.tax.model = FALSE
cluster = NULL
display.plots = FALSE
save.plots = FALSE
last.t.years = 40
figures.dir = "Writeup/Figures/"
allow.tax.rate.changes = F


#set.seed(55333)

if(!is.null(cluster)) #If cluster is given
{
  cl <- cluster
} else if(parallelize.tax.model) #If flag is set for parallelization 
{
  num.of.cores <- min(4, detectCores())
  cl <- makeCluster(num.of.cores, outfile="")
  registerDoParallel(cl)
} else { #No parallelization
  cl <- NULL
}

#data.dir      <- "Tax data/"


# Create model configurations from file if 'config' was not specified.
if(is.null(config))
  config <- read.csv(config.file, stringsAsFactors = F)

if(is.null(population.data))
  population.data <- get.population.data(population.data.file)

net.degree.info<-readRDS(file=network.data.manipulation.file)

#Size of the population
N <- nrow(population.data) 
#Overwrite the config population.size
config[config$config.vars == 'population.size', 'value'] <- N
ave.degree.tTaxes <- as.numeric(config[config$config.vars == 'ave.degree.tTaxes', 'value']) 

if(allow.tax.rate.changes == TRUE) {
  #Change the tax rates 
  #First, identify what is the change in tax rate and then add that change to individual tax rates
  id <- which(config$config.vars == 'tax.rate.delta')
  tax.rate.delta <- as.numeric(config[id, 'value'])
  population.data$tax.rate <- population.data$tax.rate + tax.rate.delta
}

#Initializations
#population.data <- initialize.tax.payers.attributes(population.data, config)
population.data <- initialize.risk.perceptions(population.data, config, perceps = initial.state)

#Get Refund tables 
table.refund <- read.csv(tax.refund.proportions.file)
population.data$refund.group<- findInterval(population.data$income,c(table.refund$Lower.Income,Inf))

#Get IRS 
table.audit.rates <- read.csv(deterrence.strategy.file)
table.audit.rates<- table.audit.rates[-1,]

### RV: this needs to be done by sampling the right c1 distribition.
population.data$c1 <- population.data$c1.tri.dist.dist
# PK Remove this
#browser()
#population.data$c1 <- population.data$c1 + 0.1
#population.data$prop.hideable.income <- 1

if(is.null(network.data))
  network.data <- get.graph.from.network.data(network.data.file)

### RV set up the network and edgelist 
edges <- as.matrix(network.data[, c("id1", "id2")])
g <- graph_from_edgelist(el = edges,directed = FALSE)

if(is.null(g.info)) g.info<-create.interacting.network.on.taxes(g,net.degree.info, population.data, ave.degree.tTaxes)
#Overwriting gn

g <- g.info$g
g.info$net.degree.info <- net.degree.info
nn<- g.info$nn

pop.data <- population.data

track.dyn <- list()
aggregated.dyn <- NULL
gov.dyn.at.t <- NULL
gov.dyn <- NULL
tax.gap.grad <- NULL
create.config.vars(config)
#rm(tax.rate)
V.max <- NULL

N <- nrow(pop.data)

num.audited = 0
num.penalized = 0
num.compliant = N
eff.vector <- rep(0, N)
tab.audit.costs <- read.csv("Inputs/Audit.Costs.csv")

tax.ids <- pop.data$tax.ids
pop.data[, 'perc.hideable.income'] <- 100*pop.data$prop.hideable.income

init.transformed.per.audit.rate <- pop.data$transformed.per.audit.rate.0
#t.minus.1 is for holding the transformed.per.audit.rate for the
#previous loop iteration to inform the next loop iteration
transformed.per.audit.rate.t.minus.1 <- NULL


#Network information
edgelist.g<- g.info$edgelist.g
nn<- g.info$nn
nn.int <- g.info$nn.int
net.degree.info <- g.info$net.degree.info
effective.beta.media <- 0

g <- g.info$g
assort.measure.names <- c('nn.over.nc', 'cc.over.cn', 'assortativity')

hist <- initialize.history(N)
compliant.history <- hist[['compliant.history']]
penalty.history <- hist[['penalty.history']]
audit.history <- hist[['audit.history']]
amount.under.reporting.history <- hist[['amount.under.reporting.history']]
perc.reporting.history <- hist[['perc.reporting.history']]
perc.hideable.reporting.history <- hist[['perc.hideable.reporting.history']]

##############################################
## Carry over data from one complete run of
## the model to the next.
##############################################
if(is.null(initial.state)) {
  state.at.time.t <- initialize.track.dynamics(pop.data)
  transformed.per.audit.rate.t.minus.1 <- init.transformed.per.audit.rate
  hist.df <- bind_cols(hist)
  state.at.time.t <- cbind.data.frame(state.at.time.t, hist.df)
  initial.media.feeback <- get.media.tax.morale.effect(tax.gap = 0,
                                                       media.mid.effect,
                                                       media.steepness,
                                                       media.stochastic.offset)
  
  assort.measure <- get.compliance.assortativity.measure(g, state.at.time.t,
                                                         field ="hideable.reported",
                                                         threshold=0.02)
  am.list <- assort.measure$assortativity.measures[, 2]
  names(am.list) <- assort.measure.names
  aggregated.dyn <-  rbind(aggregated.dyn, c(t=1,
                                             tax.gap=0,
                                             media.feedback=(1 - initial.media.feeback),
                                             mean.per.audit.rate=mean(state.at.time.t$per.audit.rate),
                                             sd.per.audit.rate=sd(state.at.time.t$per.audit.rate),
                                             mean.per.penalty.rate = mean(state.at.time.t$per.penalty.rate),
                                             sd.per.penalty.rate = sd(state.at.time.t$per.penalty.rate),
                                             num.audited = num.audited,
                                             num.penalized = num.penalized,
                                             num.compliant = num.compliant,
                                             total.penalty.and.past.tax.to.pay=sum(state.at.time.t$penalty.and.past.tax.to.pay),
                                             total.past.tax.to.pay=sum(state.at.time.t$past.tax.to.pay),
                                             am.list))
  V.max <- 0
  
} else {
  state.at.time.t <- initial.state
  state.at.time.t[, 't'] <- rep(1, N)
  transformed.per.audit.rate.t.minus.1 <- state.at.time.t$transformed.per.audit.rate
  
  #For easy handling of history we temporarily assign them to dataframes outside state.at.time.t
  #which will later be assigned back
  compliant.history <- state.at.time.t[, names(compliant.history)]
  penalty.history <- state.at.time.t[, names(penalty.history)]
  audit.history <- state.at.time.t[, names(audit.history)]
  amount.under.reporting.history <- state.at.time.t[, names(amount.under.reporting.history)]
  perc.reporting.history <- state.at.time.t[, names(perc.reporting.history)]
  perc.hideable.reporting.history <- state.at.time.t[, names(perc.hideable.reporting.history)]
  
  tax.gap <- compute.tax.gap(pop.data, state.at.time.t)
  
  initial.media.feeback <- get.media.tax.morale.effect(tax.gap,
                                                       media.mid.effect,
                                                       media.steepness,
                                                       media.stochastic.offset)
  num.audited <- length(which(state.at.time.t$audited))
  num.penalized <- length(which(state.at.time.t$penalized))
  num.compliant <- length(which(state.at.time.t$compliant))
  
  assort.measure <- get.compliance.assortativity.measure(g, state.at.time.t,
                                                         field ="hideable.reported",
                                                         threshold=0.02)
  am.list <- assort.measure$assortativity.measures[, 2]
  names(am.list) <- assort.measure.names
  
  aggregated.dyn <-  rbind(aggregated.dyn, c(t=1,
                                             tax.gap=tax.gap,
                                             media.feedback=(1 - initial.media.feeback),
                                             mean.per.audit.rate=mean(state.at.time.t$per.audit.rate),
                                             sd.per.audit.rate=sd(state.at.time.t$per.audit.rate),
                                             mean.per.penalty.rate = mean(state.at.time.t$per.penalty.rate),
                                             sd.per.penalty.rate = sd(state.at.time.t$per.penalty.rate),
                                             num.audited = num.audited,
                                             num.penalized = num.penalized,
                                             num.compliant = num.compliant,
                                             total.penalty.and.past.tax.to.pay=sum(state.at.time.t$penalty.and.past.tax.to.pay),
                                             total.past.tax.to.pay=sum(state.at.time.t$past.tax.to.pay),
                                             am.list))
  
  V.max <- max(state.at.time.t$V.new, na.rm = T)
}
## End of carrying over data across runs


################################################
## Initialize all required variables
################################################
track.dyn[[1]] <- state.at.time.t

R0<-R1<-R3<-Rannoyed <- NULL
hideable.R0 <- hideable.R1 <- hideable.R3 <- hideable.Rannoyed <- NULL
mean.perc.report.by.comp.aud.3yrs.ago <- NULL
mean.hideable.report.by.comp.aud.3yrs.ago <- NULL

t.media.range <-round(10*morale.half.life,0)
seq.num.of.audits<- NULL
seq.recovered.revenue.plus.penalties<- NULL


# #Get audit rates based on IRS data
# pop.data$IRS.audit.target <- get.IRS.prob.of.audit(pop.data,table.audit.rates, additional.criteria=NULL)

#Create all config variables in the scope of this function.
ifelse(network.model != FALSE, network.effect <- TRUE, network.effect <- FALSE)

bomb.crater.effect <- gamblers.fallacy

### RV  Modify the beta media and network depending on whether these effects are on
if(!network.effect){
  effective.beta.network  <- 0
  Delta.Network <- 0
}
if(!media.effect){
  effective.beta.media<-0
  Delta.Media <- 0
}
Delta.Penalty <- rep(0, N)

### RV initialize the exponential moving average for media effect on deterrence
### set these large enough so that media is not active during the early dynamics. 
ave.num.of.audits<-20*audit.rate*N+ sqrt(audit.rate*N)
ave.recovered.revenue.plus.penalties<-sum(pop.data$income)*mean(pop.data$tax.rate)*
  audit.rate*(1+penalty.rate) 

if(is.null(final.year)){
  final.year <- total.years
}

Q.max <- max(state.at.time.t$Q.new)

#Step0: Get the discount rates
discount.rates <- initialize.discount.rates(config)
s.discount              <- discount.rates$s.discount[1]
s.discount.seq.default  <- discount.rates[, 's.discount.seq.default']
s.audit.discount        <- discount.rates$s.audit.discount[1]
s.generation.discount   <- discount.rates$s.generation.discount[1]
generation.half.life    <- discount.rates$generation.half.life[1]


state.at.time.t[, 'tax.ids'] <- pop.data[, c("tax.ids")]
equilibrium.reached.at <- Inf

audited.non.compliant <- rep(FALSE, N)

#for(t in 2:final.year){
t <- 1
is.in.equilibrium <- FALSE


if(model.type == "10k") {
  save.image(file = "Inputs/10k_for_prototype.RData")
} else {
  save.image(file = "Inputs/1k_for_prototype.RData")
}

