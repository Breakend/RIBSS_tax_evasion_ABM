remove(list = ls(all = TRUE))
#gc()

### Load Libraries: please ensure you have installed these R packages.
library(ggplot2)
library(Hmisc)
library(igraph)
library(reshape2)
library(Rcpp)
#library(GGally)
#library(sna)

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

source("Library/library.R")

tax.model <- function(config = NULL,
                      config.file = "Tax data/config.csv",
                      plots.needed = FALSE)
{
  ##Set Seed 
  set.seed(55279) ### first seed given by Chris Marcum
  set.seed(55333) 
  
  data.dir<- "Tax data/"
  figures.dir   <- "../Writeup/Figures/"
  
  # Create model configurations from file if 'config' was not specified.
  if(is.null(config))
    config <- config <- read.csv(config.file, stringsAsFactors = F)
  
  N <- get.config.param(config, 'population.size')
  tax.ids <- 1:N
  
  risk.perceptions <- initialize.risk.perceptions(config)
  tax.payers.attribs <- initialize.tax.payers.attributes(config)
  population.data <- merge(risk.perceptions, tax.payers.attribs, by='tax.ids')
  
  ave.degree <- get.config.param(config, 'ave.degree')
  g <- create.network(ave.degree, N)

  ### get list of neigbours 
  ### (we do this here and not in the loop since we assume a static network)
  nn <- get.nearest.neighbours(tax.ids, g)
  #nn.int <- nn
  
  
  
  #final.year <- 3
  pop.data <- run.dynamics(population.data, config, nn)
      
  
  #############################################################
  ###                                                       ###
  ###       Arrange Output Data           
  ###                                                       ###
  #############################################################
  track.id <- do.call("rbind",track.dyn)
  aggregated.dyn <- as.data.frame(aggregated.dyn)
 
  
  sim.data <- with(pop.data, data.frame(c1=round(100*c1,0),
                                   c1.tilde=round(100*c1.tilde,0),
                                   per.audit.rate=round(100*per.audit.rate,0),
                                   per.penalty.rate=round(100*per.penalty.rate,0),
                                   Delta.Morale = round(100*Delta.Morale ,0),
                                   w = round(100*w,0),
                                   freq.audits=freq.audits,
                                   freq.penalty=freq.penalty,
                                   years.since.last.compliant =years.since.last.compliant,
                                   years.since.last.audit=years.since.last.audit,
                                   years.since.last.penalty=years.since.last.penalty,
                                   reporting.history))
  
  
  
  if(plots.needed)
  {
    create.plots(sim.data, track.id, aggregated.dyn, config, figures.dir)
    track.id.list <- split(track.id,track.id$tax.ids)
    final.year <- get.config.param(config, 'total.years')
    R0<- rev(R0)[1:(final.year/2)]
    R1<- rev(R1)[1:(final.year/2)]
    R3<- rev(R3)[1:(final.year/2)]
    Rannoyed <- rev(Rannoyed)[1:(final.year/2)]
    
    print(paste("R0=",round(mean(R0[!is.nan(R0)]),2),sep=""))
    print(paste("target R0=",0.286*round(mean(R1[!is.nan(R1)]),2),sep=""))
    print(paste("R1=",round(mean(R1[!is.nan(R1)]),2),sep=""))
    print(paste("R3=",round(mean(R3[!is.nan(R3)]),2),sep=""))
    print(paste("target R3=",0.63*round(mean(R1[!is.nan(R1)]),2),sep=""))
    print(paste("Rannoyed=",100*round(mean(Rannoyed[!is.nan(Rannoyed)]),2),"% decrease in reported income",sep=""))
    print(paste("target Rannoyed=",35,"% decrease in reported income",sep=""))
    
    see.nn.info.at.time.t(track.id.list,i=626,nn,64)
  }
 
  #print(aggregated.dyn)
  return(sim.data) 
}

