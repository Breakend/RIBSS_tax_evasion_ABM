#remove(list = ls(all = TRUE))
#gc()

### Load Libraries: please ensure you have installed these R packages.
library(ggplot2)
library(Hmisc)
library(igraph)
library(reshape2)
library(Rcpp)
library(parallel)
library(doParallel)
library(dplyr)
library(TTR)
library(scales)
#library(psych)

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

source("Library/library.R")

tax.model <- function(config.file = "Inputs/model.config.csv",
                      config = NULL,
                      seed = 55279,
                      population.data.file = "Inputs/PN1_population_data.csv",
                      population.data = NULL,
                      network.data.file = "Inputs/PN1_network_data.csv",
                      network.data.manipulation.file = "Inputs/ALP.network.degree.manipulations.Rdata",
                      deterrence.strategy.file = "Inputs/IRS Examination coverage in FY2015.csv",
                      tax.refund.proportions.file = "Inputs/Tax Refund Proportions.csv",
                      network.data = NULL,
                      initial.state = NULL,
                      g.info = NULL,
                      final.year = NULL,
                      return.final.state.not.outputs = FALSE,
                      parallelize.tax.model = FALSE,
                      cluster = NULL,
                      display.plots = FALSE,
                      save.plots = FALSE,
                      detailed.outputs = TRUE,
                      last.t.years = 40,
                      figures.dir = "Writeup/Figures/",
                      allow.tax.rate.changes = F,
                      baseline.gov.dyn = NULL)
{
  ##Set Seed 
  set.seed(seed)
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

  #browser()
  all.data <- run.dynamics(population.data, initial.state, config, g.info, 
                           table.audit.rates, table.refund, baseline.gov.dyn, final.year, cl=cl)
  
  final.state <- all.data[['final.state']]
  aggregated.dyn <- as.data.frame(all.data[['aggregated.dyn']])
  gov.dyn <- all.data[['gov.dyn']]
  track.id <- as.data.frame(bind_rows(all.data$track.dyn))
  equilibrium.reached <- all.data[['equilibrium.reached']]
  baseline.gov.dyn <- all.data[['baseline.gov.dyn']]
  sim.data <- NULL
  outputs <- NULL
  
  if(return.final.state.not.outputs)
  {
    outputs <- NULL
    
  } else 
  {
    #############################################################
    ###                                                       ###
    ###       Arrange Output Data           
    ###                                                       ###
    #############################################################
    #Get all the history data from final state. 
    #We initialize it only to create the required data frame structure with appropriate names
    hist <- initialize.history(N)
    compliant.history <- final.state[, names(hist[['compliant.history']])]
    penalty.history <- final.state[, names(hist[['penalty.history']])]
    audit.history <- final.state[, names(hist[['audit.history']])]
    amount.under.reporting.history <- final.state[, names(hist[['amount.under.reporting.history']])]
    perc.reporting.history <- final.state[, names(hist[['perc.reporting.history']])]
    perc.hideable.reporting.history <- final.state[, names(hist[['perc.hideable.reporting.history']])]
    
    tmpr <- sapply(names(perc.reporting.history), function(col.name) {
      perc.reporting.history[, col.name] <<- ifelse(perc.reporting.history[, col.name] > 100, 100, perc.reporting.history[, col.name])
    })
    rm(tmpr)
    final.state[, names(perc.reporting.history)] <- perc.reporting.history
    
    sim.data <- with(final.state, data.frame(tax.ids=population.data$tax.ids, 
                                             self.employed=population.data$self.employed,
                                                 c1=round(100*population.data$c1,0),
                                                 c1.tilde=round(100*c1.tilde,0),
                                                 per.audit.rate = per.audit.rate*100,
                                                 per.penalty.rate = per.penalty.rate*100,
                                                 perc.hideable.income = population.data$prop.hideable.income*100,
                                                 Delta.Morale = round(100*Delta.Morale ,0),
                                                 Delta.Network = round(100*Delta.Network ,0),
                                                 w = round(100*w,0),
                                                 freq.audits=freq.audits,
                                                 freq.penalty=freq.penalty,
                                                 years.since.last.compliant =years.since.last.compliant,
                                                 years.since.last.audit=years.since.last.audit,
                                                 years.since.last.penalty=years.since.last.penalty,
                                                 amount.under.reporting.history,
                                                 perc.reporting.history,
                                                 perc.hideable.reporting.history,
                                                 compliant.history,
                                                 audit.history,
                                                 penalty.history))
    
    if(detailed.outputs) {
      outputs <- generate.outputs(config, aggregated.dyn, sim.data, track.id, g.info,last.t.yrs = last.t.years, cl=cl)
    }
    
    final.year <- dim(aggregated.dyn)[1]
    R0<- rev(all.data$R0)[1:(final.year/2)]
    R1<- rev(all.data$R1)[1:(final.year/2)]
    R3<- rev(all.data$R3)[1:(final.year/2)]
    Rannoyed <- rev(all.data$Rannoyed)[1:(final.year/2)]
    mean.perc.report.by.comp.aud.3yrs.ago <- all.data$mean.perc.report.by.comp.aud.3yrs.ago
    
    #prop.reported.by.audit0.in.50yrs 
    R0 <- ifelse(is.null(R0), NA, round(mean(R0[!is.nan(R0)]),2))
    #prop.reported.by.audit1.in.50yrs 
    R1 <- ifelse(is.null(R1), NA, round(mean(R1[!is.nan(R1)]),2))
    #prop.reported.by.audit3.in.50yrs 
    R3 <- ifelse(is.null(R3), NA, round(mean(R3[!is.nan(R3)]),2))
    Rannoyed <- ifelse(is.null(Rannoyed), NA, 100.00*round(mean(Rannoyed[!is.nan(Rannoyed)]),2))
    mean.perc.report.by.comp.aud.3yrs.ago <- ifelse(is.null(mean.perc.report.by.comp.aud.3yrs.ago), NA, 
                                                    round(mean(mean.perc.report.by.comp.aud.3yrs.ago[!is.nan(mean.perc.report.by.comp.aud.3yrs.ago)]), 2))
    hideable.R0 <- rev(all.data$hideable.R0)[1:(final.year/2)]
    hideable.R0 <- ifelse(is.null(hideable.R0), NA, round(mean(hideable.R0[!is.nan(hideable.R0)]),2))
    
    hideable.R1<- rev(all.data$hideable.R1)[1:(final.year/2)]
    hideable.R1 <- ifelse(is.null(hideable.R1), NA, round(mean(hideable.R1[!is.nan(hideable.R1)]),2))
    
    hideable.R3<- rev(all.data$hideable.R3)[1:(final.year/2)]
    hideable.R3 <- ifelse(is.null(hideable.R3), NA, round(mean(hideable.R3[!is.nan(hideable.R3)]),2))
    
    hideable.Rannoyed <- rev(all.data$hideable.Rannoyed)[1:(final.year/2)]
    inds <- which(is.nan(hideable.Rannoyed) | hideable.Rannoyed == -Inf | hideable.Rannoyed == Inf)
    hideable.Rannoyed <- ifelse(is.null(hideable.Rannoyed), NA, 100.00*round(mean(hideable.Rannoyed[-inds]),2))
    
    mean.hideable.report.by.comp.aud.3yrs.ago <- all.data$mean.hideable.report.by.comp.aud.3yrs.ago
    mean.hideable.report.by.comp.aud.3yrs.ago <- ifelse(is.null(mean.hideable.report.by.comp.aud.3yrs.ago), NA, 
                                                    round(mean(mean.hideable.report.by.comp.aud.3yrs.ago[!is.nan(mean.hideable.report.by.comp.aud.3yrs.ago)]), 2))
   
    outputs <- cbind(outputs, R0, R1, R3, Rannoyed,
                     hideable.R0, hideable.R1, hideable.R3, hideable.Rannoyed,
                     mean.perc.report.by.comp.aud.3.yrs.ago = mean.perc.report.by.comp.aud.3yrs.ago, 
                     mean.hideable.report.by.comp.aud.3.yrs.ago = mean.hideable.report.by.comp.aud.3yrs.ago,
                     equilibrium.reached)
    
    if(display.plots || save.plots)
    {
      plots <- create.plots(sim.data, track.id, aggregated.dyn, config, g.info, last.t.years = last.t.years)
      display.or.save.plots(plots, figures.dir, display.plots, save.plots)
      track.id.list <- split(track.id,track.id$tax.ids)
      
      print(paste("R0=",R0,", and hideable.R0=", hideable.R0, sep=""))
      print(paste("target R0=",0.286*R1,sep=""))
      print(paste("R1=",R1,", and hideable.R1=", hideable.R1, sep=""))
      print(paste("R3=",R3,", and hideable.R3=", hideable.R3, sep=""))
      print(paste("target R3=",0.63*R1,sep=""))
      print(paste("Rannoyed=",Rannoyed,"% decrease in reported income",sep=""))
      print(paste("hideable.Rannoyed=",hideable.Rannoyed,"% decrease in reported income",sep=""))
      print(paste("target Rannoyed=",35,"% decrease in reported income",sep=""))
      print(paste("Equilibrium was reached at t:", equilibrium.reached))
      
      see.nn.info.at.time.t(track.id.list,i=161,nn,64)
    }
    
    outputs <- as.data.frame(outputs)
    #     retval <- cbind.data.frame(t(config$value), retval)
    #     names(retval) <- c(config$config.vars, colnames(outputs))
    rownames(outputs) <- NULL
  }
  
  if(parallelize.tax.model & is.null(cluster))
  {
    #Stop the created cluster
    stopCluster(cl)
  }
  
  invisible(list(outputs = outputs, final.state = final.state, 
                 agg.dyn = aggregated.dyn, gov.dyn = gov.dyn, baseline.gov.dyn = baseline.gov.dyn,
                 track.dyn = track.id, sim.data = sim.data, config = config,
                 population.data = population.data, g.info = g.info, network.data = network.data)) 
}

#Also remove the remove line from top
#x <- tax.model(display.plots = T, parallelize.tax.model = T)['outputs']
