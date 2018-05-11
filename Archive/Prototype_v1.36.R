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

config.file = "Tax data/model.config.csv"
config = NULL
seed = 55279
population.data.file = "Tax data/population_data_1k.csv"
population.data = NULL
network.data.file = "Tax data/network_data_1k.csv"
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


# Create model configurations from file if 'config' was not specified.
if(is.null(config))
  config <- read.csv(config.file, stringsAsFactors = F)

if(is.null(population.data))
  population.data <- get.population.data(population.data.file)

#Size of the population
N <- nrow(population.data) 
#Overwrite the config population.size
config[config$config.vars == 'population.size', 'value'] <- N

#Initializations
population.data <- initialize.attributes(population.data)
population.data <- initialize.perceptions(population.data, config)

if(is.null(network.data))
  network.data <- get.graph.from.network.data(network.data.file)

#Create the network
edges <- as.matrix(network.data[, c("id1", "id2")])
g <- graph_from_edgelist(el = edges)

nn <- get.nearest.neighbours(population.data$tax.ids, g)

#   nn <- nearest.neighbs['nn']
#   nn.table <- nearest.neighbs['nn.table']

#all.data <- run.dynamics(population.data, initial.state = NULL, config, nn, final.year, cl=cl)

########################################
# Run Dynamics
########################################
#Renaming population.data
pop.data <- population.data

track.dyn <- list()
aggregated.dyn <- NULL
create.config.vars(config)
rm(tax.rate)

state.at.time.t <- initialize.track.dynamics(pop.data)
initial.media.feeback <- generate.media.feedback(beta.morale.media, tax.gap = 0, 
                                                 media.mid.effect, 
                                                 media.steepness, 
                                                 media.stochastic.offset)
aggregated.dyn <-  rbind(aggregated.dyn, c(t=1,
                                           tax.gap=0,
                                           media.feedback=initial.media.feeback,
                                           mean.per.audit.rate=0,
                                           sd.per.audit.rate=0,
                                           mean.per.penalty.rate = mean(pop.data$per.penalty.rate.0 + state.at.time.t$P.new),
                                           sd.per.penalty.rate = 0))

track.dyn[[1]] <- state.at.time.t

R0<-R1<-R3<-Rannoyed<- NULL
mean.report.by.comp.aud.3yrs.ago <- NULL

#Create all config variables in the scope of this function.
ifelse(network.model != FALSE, network.effect <- TRUE, network.effect <- FALSE)
penalty.behavioral.effect <- TRUE
bomb.crater.effect <- gamblers.fallacy


if(is.null(final.year))
  final.year <- total.years

beta.morale.media <- ifelse(media.effect, 0.5, 0.0)

Q.max <- max(state.at.time.t$Q.new)
P.max <- max(state.at.time.t$P.new)

#Step0: Get the discount rates
discount.rates <- initialize.discount.rates(config)
s.discount              <- discount.rates$s.discount[1]
s.discount.seq.default  <- discount.rates[, 's.discount.seq.default']
s.audit.discount        <- discount.rates$s.audit.discount[1]
s.penalty.discount      <- discount.rates$s.penalty.discount[1]

#final.year <- 5
#network.effect <- F
#media.effect <- F

for(t in 2:final.year){
  
  state.at.time.t[, 'tax.ids'] <- pop.data[, c("tax.ids")]
  state.at.time.t[, 't'] <- rep(t, N)
  #############################################################
  ###        Model a simple Markov Process for refund return              
  #############################################################
  additional.to.refund <- get.additional.to.refund(state.at.time.t$refund.return,prob.additional.to.refund=0.1)
  state.at.time.t[additional.to.refund, 'refund.return'] <- TRUE
  
  refund.to.additional <- get.refund.to.additional(state.at.time.t$refund.return,prob.refund.to.additional=0.05)
  state.at.time.t[refund.to.additional, 'refund.return'] <- FALSE
  
  #############################################################
  ###        Update Past year's variables              
  #############################################################
  past.year.compliant <- state.at.time.t$compliant
  past.year.audited.non.compliant <- state.at.time.t$audited.non.compliant
  past.year.penalized <- state.at.time.t$penalized
  V.past <- state.at.time.t$V.new
  Q.past <- state.at.time.t$Q.new
  P.past <- state.at.time.t$P.new
  
  ### Determine the subset of nn with whom they interact with that year. 
  if(network.effect){
    #nn.int <- with(state.at.time.t, getNNInt(tax.ids, nn, yearly.prop.interactions))
    nn.int <- with(state.at.time.t, get.nn.int(tax.ids,nn,yearly.prop.interactions=yearly.prop.interactions))
    prop.of.those.int <- with(state.at.time.t, get.interacting.proportion.of.nn(tax.ids,nn,nn.int))
  }
  
  #############################################################
  ###   Media Effect on Tax Morale             
  #############################################################
  
  if(media.effect){
    media.feedback <- generate.media.feedback(beta.morale.media, tax.gap, 
                                              media.mid.effect, 
                                              media.steepness, 
                                              media.stochastic.offset)
  } else {
    media.feedback <- 0
  }
  
  #############################################################
  ###        Taxpayers decsions              
  #############################################################
  
  ### Determine the c1.tilde i.e.,  the max tax rate which taxpayer feels un-necessary to risk under-reporting
  state.at.time.t[, 'c1.tilde'] <- with(pop.data, c1.tilde.fn(c1,c2,tax.rate,per.audit.rate,per.penalty.rate*penalty.asymmetry.factor,
                                                              mu,nu,avg.nu,media.feedback,tax.complexity))
  
  ### Treat those that were compliant or penalized last year differently from those
  ### non-compliant and non-detected. 
  #browser()
  compliant.or.penalized <- past.year.compliant | past.year.penalized
  non.compliant.non.penalized <- !compliant.or.penalized
  
  
  ### Delta.Per represents a taxpayers proportion of income he wishes to report
  ### considering the factors such as his/her tax rate, perceieved deterence, 
  ### media, tax complexity and percieved return on services. 
  
  ### All compliant individuals and those just penalized based their Delta.Per on the fairness fn.
  ### For these tax payers, Delta.Per does NOT include past experiences effects.
  if(any(compliant.or.penalized)){
    state.at.time.t[compliant.or.penalized, 'Delta.Per'] <- with(pop.data[compliant.or.penalized, ], 
                                                                 fairness.fn(state.at.time.t[compliant.or.penalized, 'c1.tilde'],
                                                                             c1,c2,tax.rate))
    
    if(tax.refund.effect){
      ### In addition to tax moral we now add the effect of receiving a refund return or 
      ### or additional payment.
      inds <- with(state.at.time.t,  refund.return & compliant.or.penalized)  
      state.at.time.t[inds, 'Delta.Per'] <- with(state.at.time.t, (1-return.weight)*Delta.Per[inds]+ return.weight*1)
      inds <- with(state.at.time.t, (!refund.return & compliant.or.penalized)) 
      state.at.time.t[inds, 'Delta.Per'] <- (1-return.weight)*state.at.time.t[inds, 'Delta.Per']
    }
    
  }
  
  ### Some of those non compliant and non penalized in the past year also based their Delta.Per on
  ### the fairness fn but others will be encoraged by their recent success in evading and will
  ### have a Delta.Per =0. (i.e., they tend to zero reporting)
  ### note that there is a positive feedback here: Delta.Per=0 is more likely if you evaded and not caught.
  if(any(non.compliant.non.penalized)){
    state.at.time.t[non.compliant.non.penalized, 'Delta.Per'] <- 0 
    
    if(mild.tendency.to.full.evasion){
      ### however the tendency of under-reporting for some of these individuals may not be towards
      ### full evasion
      state.at.time.t[, 'w.non'] <- state.at.time.t$propensity.prop.income.report
      state.at.time.t[compliant.or.penalized, 'w.non'] <- NA
      #with(pop.data, w.non[compliant.or.penalized]) <- NA
      random.numbers.to.compare <- runif(length(state.at.time.t$w.non),0,1)
      partial <- (state.at.time.t$w.non>=random.numbers.to.compare) 
      partial <- non.compliant.non.penalized & partial
      if(any(partial)){
        state.at.time.t[partial, 'Delta.Per'] <- fairness.fn(state.at.time.t[partial, 'c1.tilde'],
                                                             pop.data[partial, 'c1'],
                                                             pop.data[partial, 'c2'],
                                                             pop.data[partial, 'tax.rate'])
      }
    }
  }
  
  #############################################################
  ###   Compute the Network Effect on Tax Morale: Delta.Network            
  #############################################################
  if(network.effect){
    #network.values <- with(pop.data, getNetworkInteractions(Delta.Per, tax.ids, beta.morale.network,nn.int,prop.of.those.int))
    network.values <- with(state.at.time.t, get.network.interactions(Delta.Per, tax.ids, beta.morale.network,nn.int,prop.of.those.int))
    effective.beta.morale.network <- network.values$beta.network
    Delta.Network <- network.values$unweighted.neighbours.influence
  }else{
    effective.beta.morale.network  <- 0
    Delta.Network <- 0
  }
  
  
  #############################################################
  ###   Compute Overall Delta.Morale           
  #############################################################
  
  state.at.time.t[, 'Delta.Morale'] <- with(state.at.time.t, (1-effective.beta.morale.network)*Delta.Per +
                                              effective.beta.morale.network*Delta.Network)
  
  #############################################################
  ###   Sum Delta.Morale to weighted  Past Experiences        
  #############################################################
  
  ### Find proportion of income that the taxpayer wishes to report by weighing Delta.Morale
  ### against past experiences. 
  ### First step: Tally up the experiences with tax compliance and discount past experiences wrt present.
  state.at.time.t$V.new <- s.discount*V.past+state.at.time.t[, 'Delta.Morale']
  
  ### Second step: Compute the maximum value that the Tally could have been if all past experiences 
  ### complying were positive. 
  state.at.time.t$V.max <- s.discount*state.at.time.t$V.max+1
  
  ### Third step: Find proportion of income that the taxpayer wishes to report 
  state.at.time.t$propensity.prop.income.report <- with(state.at.time.t, round(V.new/V.max,2))
  state.at.time.t$propensity.prop.income.report[state.at.time.t$propensity.prop.income.report<0.10] <- 0
  #propensity.prop.income.report[propensity.prop.income.report>=0.95] <- 1
  
  ### Determine if taxpayers are fully compliant this year: those that were not compliant last year
  ### nor penalized last year will be fully compliant unless their propensity.prop.income.report is 100%
  state.at.time.t$compliant[non.compliant.non.penalized] <- FALSE
  state.at.time.t$compliant[state.at.time.t$propensity.prop.income.report==1] <- TRUE
  
  ### Those who were compliant or have been penalized last year are susceptible to initiate evasion.
  suseptibles.to.underreport.pop  <- (compliant.or.penalized & state.at.time.t$propensity.prop.income.report<1)
  
  random.numbers.to.compare <- runif(sum(suseptibles.to.underreport.pop, na.rm = T),0,1)
  initiation.prob <- 1-state.at.time.t$propensity.prop.income.report[suseptibles.to.underreport.pop]
  ### Determine which of the susceptible taxpayers initiate evasion.
  ### those that initiate have compliant set to FALSE
  state.at.time.t$compliant[suseptibles.to.underreport.pop] <- (initiation.prob<random.numbers.to.compare) 
  
  ### Interpret these as probabilities to fully-comply:
  state.at.time.t[, 'w'] <- state.at.time.t$propensity.prop.income.report
  state.at.time.t$w[non.compliant.non.penalized] <- 0
  state.at.time.t$w[state.at.time.t$propensity.prop.income.report==1] <- 1
  state.at.time.t$w[suseptibles.to.underreport.pop] <- 1-initiation.prob
  
  ### track number of years since last fully compliant.
  state.at.time.t$years.since.last.compliant <- with(state.at.time.t, years.since.last.compliant+as.numeric(!compliant))
  state.at.time.t$years.since.last.compliant[state.at.time.t$compliant] <- 0
  
  ### Determine the amount reported by those fully compliant 
  state.at.time.t[, 'report.abs'] <- pop.data$income
  ### Determine the amount reported by those non-compliant 
  compliant <- state.at.time.t$compliant
  state.at.time.t$report.abs[!(compliant)] <- pop.data$income[!(compliant)]*state.at.time.t$propensity.prop.income.report[!(compliant)]
  state.at.time.t$report.abs<- round(state.at.time.t$report.abs,0)
  state.at.time.t[, 'report'] <- round((state.at.time.t$report.abs/pop.data$income) * 100)
  
  ### Update compliance history
  state.at.time.t$compliant.history <- with(state.at.time.t, append.to.history2(compliant.history, compliant,K,label="compliant"))
  state.at.time.t$under.reporting.history <- with(state.at.time.t, append.to.history2(under.reporting.history,
                                                                                      pop.data$income-report,K,label="percent.reported"))
  
  state.at.time.t[, 'reporting.history'] <- pop.data$income - state.at.time.t$under.reporting.history
  
  
  #############################################################
  ###        Tax authority              
  #############################################################
  
  
  audit <- rep(FALSE,N)
  
  if(targetted.auditing){
    ### Target Audit. Assume we know those more likely to evade.
    #browser()
    target <- (state.at.time.t$Delta.Morale == 0)
    target.audit<- (runif(sum(target),0,1)<= audit.rate*N/sum(target))
    audit[target]<- target.audit  
  }else{
    ### Audit. For the time being we assume random audit
    audit <- (runif(N,0,1)<=audit.rate)
  }
  
  
  ### update that number of audits each tax payer experienced.
  
  state.at.time.t$audit.history <- with(state.at.time.t, append.to.history2(audit.history,audit,K,label="audited"))
  state.at.time.t$freq.audits <- state.at.time.t$freq.audits+as.numeric(audit)
  
  state.at.time.t$years.since.last.audit <- state.at.time.t$years.since.last.audit+1
  state.at.time.t$years.since.last.audit[audit]<-0
  
  ### Find those that are audited and not compliant. 
  audited.non.compliant <- audit & (!state.at.time.t$compliant)
  
  ### Find those that are penalized from those audited and not compliant.
  state.at.time.t[, 'penalized'] <- rep(FALSE,N)
  #with(pop.data, penalized[audited.non.compliant] <- (runif(sum(audited.non.compliant),0,1) <= detection.eff))
  state.at.time.t$penalized[audited.non.compliant] <- runif(sum(audited.non.compliant),0,1) <= detection.eff
  
  ### Update penalized history.
  state.at.time.t$penalty.history <- with(state.at.time.t, append.to.history2(penalty.history,penalized,K,label="penalty"))
  state.at.time.t$freq.penalty <- state.at.time.t$freq.penalty + as.numeric(state.at.time.t$penalized)
  #with(pop.data, freq.penalty<- freq.penalty+as.numeric(penalized))
  
  ### Find the Penatly to pay. 
  ### Note the multiplication by (!penalty.history). This ensures
  ### those audited and penalized in K previous years - aren't penalized again for
  ### the same under-reporting year. 
  sum.non.penalized.past.under.reporting <- with(state.at.time.t, rowSums(under.reporting.history*(!penalty.history)))
  #browser()
  past.tax.to.pay <- as.numeric(state.at.time.t$penalized)*(pop.data$tax.rate)*sum.non.penalized.past.under.reporting  
  
  penalty.to.pay <- as.numeric(state.at.time.t$penalized)*(penalty.rate)*sum.non.penalized.past.under.reporting
  
  penalty.and.past.tax.to.pay <- past.tax.to.pay + penalty.to.pay
  
  
  
  ### Correct penalized history by recording that an audited and penalized 
  ### individual has paid his/her past K years of penalties.
  
  state.at.time.t$penalty.history<- correct.past.penalties(state.at.time.t$penalty.history)
  
  state.at.time.t$years.since.last.penalty <- state.at.time.t$years.since.last.penalty+1
  state.at.time.t$years.since.last.penalty[state.at.time.t$penalized] <- 0
  
  #############################################################
  ###        Update Risk Perceptions            
  #############################################################
  
  ### Update Percieved Audit Rate
  if(network.effect){
    #browser()
    #       pop.data[, 'Delta.Q'] <- with(pop.data, doNetworkInteractions(as.numeric(audit),tax.ids, 1,
    #                                                                     nn.int,prop.of.those.int))
    state.at.time.t[, 'Delta.Q'] <- with(state.at.time.t, do.network.interactions2(as.numeric(audit),tax.ids, 1,
                                                                                   nn.int,prop.of.those.int, method="max"))
  }else{
    state.at.time.t[, 'Delta.Q'] <- as.numeric(audit)
  }
  
  state.at.time.t$Q.new <- s.audit.discount*Q.past + state.at.time.t$Delta.Q
  Q.max <- s.audit.discount*Q.max+1
  state.at.time.t[, 'w.Q'] <- state.at.time.t$Q.new/Q.max
  state.at.time.t$per.audit.rate <- with(pop.data, 
                                         (transformed.per.audit.rate.0+state.at.time.t$w.Q)/(transformed.per.audit.rate.0+1))
  
  #bomb.crater.effect <- gamblers.fallacy
  
  if(gamblers.fallacy){
    ### Find those that are not audited and not compliant. 
    non.audited.non.compliant <- (!audit) & (!state.at.time.t$compliant)
    ### Induce cold feet effect on these individuals - 
    ### by increasing their audit risk perception
    if(any(non.audited.non.compliant))
    {
      state.at.time.t$per.audit.rate[non.audited.non.compliant] <- get.per.audit.gamblers.fallacy(state.at.time.t$per.audit.rate[non.audited.non.compliant],
                                                                                                  state.at.time.t$years.since.last.audit[non.audited.non.compliant])
    }
  }
  
  if(bomb.crater.effect){
    ### Find those that are audited and not penalized. 
    audited.non.penalized <- audit & (!state.at.time.t$penalized) 
    ### Reduce percieved risk of getting audited again.
    if(any(audited.non.penalized))
    {
      state.at.time.t$per.audit.rate[audited.non.penalized] <- get.per.audit.bomb.crater(state.at.time.t$per.audit.rate[audited.non.penalized],
                                                                                         state.at.time.t$years.since.last.audit[audited.non.penalized])
    }
    
  }
  
  
  ### Update Percieved Penalty Rate
  state.at.time.t$P.new <- s.penalty.discount*P.past+as.numeric(penalty.rate-state.at.time.t$per.penalty.rate)*as.numeric(audit) 
  ### even if not penalized but audited you get a better sense of penaty rate.
  state.at.time.t$per.penalty.rate <- pop.data$per.penalty.rate.0 + state.at.time.t$P.new
  
  #############################################################
  ### Convert assigned Penalty into a change in compliance behavior             
  #############################################################
  
  if( penalty.behavioral.effect & any(state.at.time.t$penalized)){
    
    ### Express the total penalty as a fraction of the expected tax revenue owed. 
    #num.years.penalty.represents <- penalty.to.pay/(tax.rate*income)
    #browser()
    num.years.penalty.represents <- penalty.and.past.tax.to.pay/(pop.data$tax.rate*pop.data$income)
    
    Delta.Penalty <- num.years.penalty.represents[state.at.time.t$penalized]
    
    state.at.time.t$V.new[state.at.time.t$penalized] <- s.discount*state.at.time.t$V.new[state.at.time.t$penalized]+Delta.Penalty
    
    state.at.time.t$V.max[state.at.time.t$penalized] <- s.discount*state.at.time.t$V.max[state.at.time.t$penalized]+Delta.Penalty
  }
  
  
  #############################################################
  ###   Compute the Tax Gap             
  #############################################################
  
  ### Compute the tax gap 
  expected.tax.revenue<- sum(pop.data$tax.rate*pop.data$income)
  tax.revenue <- sum(pop.data$tax.rate*state.at.time.t$report.abs)
  tax.gap.amount <- expected.tax.revenue- tax.revenue
  tax.gap <- tax.gap.amount/expected.tax.revenue
  ### EU tax gap see http://www.socialistsanddemocrats.eu/sites/default/files/120229_richard_murphy_eu_tax_gap_en.pdf
  ###US tax gap see https://www.irs.gov/pub/newsroom/overview_tax_gap_2006.pdf
  ### US tax  gap is 14%, Italy & Greece 27% and Sweeden, Denamrk, Germany and UK are 
  ### 18.8,17.7,16.0 and 12.5% respectively. 
  
  #browser()
  
  #############################################################
  ###    Track Outputs            
  #############################################################
  
  if (ncol(state.at.time.t$audit.history)==4){ 
    ### The annoyed popultion are those audited that are not found to underreport 
    ### and thus were not penalized. 
    #browser()
    pop.annoyed <- with(state.at.time.t, (audit.history[, "audited.3.years.ago"] & 
                                            !(penalty.history[, "penalty.3.years.ago"]) &
                                            reporting.history[, "percent.reported.3.years.ago"]>0))
    tmp.reported.0.years.ago <- state.at.time.t$report[pop.annoyed]
    tmp.reported.3.years.ago <- state.at.time.t$reporting.history[pop.annoyed, "percent.reported.3.years.ago"]
    Rannoyed <- c(Rannoyed,mean(1-(tmp.reported.0.years.ago/tmp.reported.3.years.ago)))
    
    comp.aud.3yrs.ago <- with(state.at.time.t, (audit.history[, "audited.3.years.ago"] &
                                                  reporting.history[, "percent.reported.3.years.ago"] > 95))
    
    mean.report.by.comp.aud.3yrs.ago <- with(state.at.time.t, mean(reporting.history[, "percent.reported.0.years.ago"]))
    
  }
  
  state.at.time.t[, "audited"] <- state.at.time.t$audit.history[, "audited.0.years.ago"]
  
  ### The pop0 population is the population penalized in the current year.
  ### pop1 and pop3 those penalized 1 and 3 years ago. 
  pop0<- (state.at.time.t$years.since.last.penalty==0)
  pop1<- (state.at.time.t$years.since.last.penalty==1)
  pop3<- (state.at.time.t$years.since.last.penalty==3)
  
  
  R0<- c(R0,mean(state.at.time.t$report[pop0]))
  R1<- c(R1,mean(state.at.time.t$report[pop1]))
  R3<- c(R3,mean(state.at.time.t$report[pop3]))
  
  track.dyn[[t]] <-  state.at.time.t
  
  aggregated.dyn <-  rbind(aggregated.dyn, c(t=t,
                                             tax.gap=tax.gap,
                                             media.feedback=media.feedback,
                                             mean.per.audit.rate=mean(state.at.time.t$per.audit.rate),
                                             sd.per.audit.rate=sd(state.at.time.t$per.audit.rate),
                                             mean.per.penalty.rate = mean(state.at.time.t$per.penalty.rate),
                                             sd.per.penalty.rate = sd(state.at.time.t$per.penalty.rate)))
  
  
}

final.state <- state.at.time.t


#############################################################
###                                                       ###
###       Arrange Output Data           
###                                                       ###
#############################################################
track.id <- as.data.frame(bind_rows(track.dyn))
aggregated.dyn <- as.data.frame(aggregated.dyn)  
sim.data <- with(final.state, data.frame(tax.ids=population.data$tax.ids,
                                         c1=population.data$c1,
                                         c1.tilde=round(100*c1.tilde,0),
                                         per.audit.rate=population.data$per.audit.rate,
                                         per.penalty.rate=population.data$per.penalty.rate,
                                         Delta.Morale = round(100*Delta.Morale ,0),
                                         w = round(100*w,0),
                                         freq.audits=freq.audits,
                                         freq.penalty=freq.penalty,
                                         years.since.last.compliant =years.since.last.compliant,
                                         years.since.last.audit=years.since.last.audit,
                                         years.since.last.penalty=years.since.last.penalty,
                                         reporting.history,
                                         compliant.history,
                                         audit.history,
                                         penalty.history))

outputs <- generate.outputs(config, aggregated.dyn, sim.data, track.id, nn, cl=cl)

final.year <- dim(aggregated.dyn)[1]
R0<- rev(R0)[1:(final.year/2)]
R1<- rev(R1)[1:(final.year/2)]
R3<- rev(R3)[1:(final.year/2)]
Rannoyed <- rev(Rannoyed)[1:(final.year/2)]
mean.report.by.comp.aud.3yrs.ago <- mean.report.by.comp.aud.3yrs.ago

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
