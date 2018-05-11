#############################################################
###                                                       ###
###        Run Dynamics
###                                                       ###
#############################################################

run.dynamics.audits.at.intermitant.years.exp <- function(pop.data, initial.state=NULL, config, g.info,
                                                         table.audit.rates, table.refund, baseline.gov.dyn = NULL,
                                                         final.year = NULL, cl=NULL, period.before.next.audit.set=1)
{
  track.dyn <- list()
  aggregated.dyn <- NULL
  tax.gap.grad <- NULL
  
  create.config.vars(config)
  #rm(tax.rate)
  V.max <- NULL
  tax.gap <- 0
  
  tref <- 0
  
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
  
  #Governmet variables
  gov.dyn.at.t <- NULL
  gov.dyn <- NULL
  alpha_c <- NULL
  gd.scale.factor <- NULL
  other.rev <- NULL
  initial.deficit <- NULL
  US.marginal.deficit <- NULL
  US.marginal.Debt <- NULL
  US.Debt <- NULL
  previous.year.inflated.Debt <- NULL
  
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
                                               expected.tax.revenue = sum(pop.data$tax.rate*pop.data$income),
                                               tax.revenue = sum(pop.data$tax.rate*state.at.time.t$report.abs),
                                               mean.audit.costs = 0,
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
                                               expected.tax.revenue = sum(pop.data$tax.rate*pop.data$income),
                                               tax.revenue = sum(pop.data$tax.rate*state.at.time.t$report.abs),
                                               mean.audit.costs = NA,
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
    
    gd.scale.factor <- baseline.gov.dyn$gd.scale.factor
    alpha_c <- baseline.gov.dyn$alpha_c
    other.rev <- baseline.gov.dyn$other.rev
    initial.deficit <- baseline.gov.dyn$US.deficit
    US.marginal.deficit <- baseline.gov.dyn$US.marginal.deficit
    US.marginal.Debt <- baseline.gov.dyn$US.marginal.Debt
    US.Debt <- baseline.gov.dyn$US.Debt
    previous.year.inflated.Debt <- baseline.gov.dyn$inflated.Debt
    
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
  
  while(t < final.year || (run.till.equilibrium == TRUE && is.in.equilibrium != TRUE)) {
    t <- t + 1 #starts loop from t = 2
    state.at.time.t[, 't'] <- rep(t, N)
    
    #############################################################
    ###        Update talk taxes Network
    #############################################################
    # PK removing this section due to efficiency
    #if(network.effect){
    ### Determine the subset of nn with whom they interact with that year.
    # PK removing this section due to efficiency
    # ids.to.reset.edges <- sample(pop.data[, c("tax.ids")],
    #                              stoch.round(N/generation.half.life[1]),replace = FALSE)
    # g.info <- reconnect.edges.of.taxpayer(ids.to.reset.edges,edgelist.g,nn.int)
    #
    # edgelist.g <- g.info$edgelist.g
    # nn.int <- g.info$nn.int   ### RV I need some help here to put this in the with(state.at.time.t, ) framework.
    #}
    
    #############################################################
    ###
    ###  Capture all relevant state and decision variables of the past year
    ###
    #############################################################
    past.year.compliant <- state.at.time.t$compliant
    #past.year.audited.non.compliant <- audited.non.compliant
    past.year.penalized <- state.at.time.t$penalized
    V.past <- state.at.time.t$V.new
    Q.past <- state.at.time.t$Q.new
    
    ### Treat those that were compliant or penalized last year differently from those
    ### non-compliant and non-detected.
    compliant.or.penalized <- past.year.compliant | past.year.penalized
    non.compliant.non.penalized <- !compliant.or.penalized
    
    
    
    #############################################################
    ###
    ###        Step 1: The Behavioral Model
    ###
    #############################################################
    #Some necessary intializations
    state.at.time.t[, 'c1'] <- pop.data$c1
    state.at.time.t[, 'perc.hideable.income'] <- pop.data$perc.hideable.income
    state.at.time.t[, 'self.employed'] <- pop.data$self.employed
    
    ########################################################################
    ###  Step 1.1: Compute the Personal Evaluation on Tax Morale: Delta.Per
    ########################################################################
    
    ### Determine the c1.tilde
    state.at.time.t[, 'c1.tilde'] <- with(state.at.time.t,
                                          get.c1.tilde(pop.data$c1,pop.data$c2,pop.data$tax.rate,per.audit.rate,
                                                       per.penalty.rate*penalty.asymmetry.factor,m.qP,s.qP))
    
    ### Sanity Check Assertion.
    assertthat::assert_that(any(state.at.time.t[, 'c1.tilde'] < 1.0))
    
    ### calculating potential delta personal values with and without deterrence
    Potential.Delta.Per.without.deterrence<-
      fairness.fn(pop.data[, 'c1'],
                  pop.data[, 'c1'],
                  pop.data[, 'c2'],
                  pop.data[, 'tax.rate'])
    
    Potential.Delta.Per<-
      fairness.fn(state.at.time.t[, 'c1.tilde'],
                  pop.data[, 'c1'],
                  pop.data[, 'c2'],
                  pop.data[, 'tax.rate'])
    
    ### Determine Delta.Per for those who are either compliant
    if(any(compliant.or.penalized)){
      state.at.time.t[compliant.or.penalized, 'Delta.Per'] <- Potential.Delta.Per[compliant.or.penalized]
    }
    
    ### Determine Delta.Per for those who are either non.compliant & non.penalized
    ### Warning: This part of the code & behavioral mechanisms has very big leverage on model outcomes.
    if(any(non.compliant.non.penalized)){
      ### Non compliant have Delta.Per = 0. Thus, they strongly tending towards full evasion.
      state.at.time.t[non.compliant.non.penalized, 'Delta.Per'] <- 0
      
      ### However, if mild.tendency.to.full.evasion is ON this tendency towards full evasion
      ### is slowed and evaders will only tend increasingly towards full evasion
      ### as they increasing under-report.
      if(mild.tendency.to.full.evasion){
        state.at.time.t[, 'w.non'] <- state.at.time.t$propensity.prop.income.report
        state.at.time.t[compliant.or.penalized, 'w.non'] <- NA
        
        non.long.term.evaders <- (round(state.at.time.t$years.since.last.compliant)<generation.half.life)
        #sapply(pmax((state.at.time.t$w.non)^full.tendency.factor,0.0),stoch.round)
        spontaneously.regretful <- non.compliant.non.penalized &
          (non.long.term.evaders | Potential.Delta.Per.without.deterrence==1)
        
        if(any(spontaneously.regretful)){
          state.at.time.t[spontaneously.regretful, 'Delta.Per'] <- Potential.Delta.Per[spontaneously.regretful]
        }
      }
    }
    
    ### Apply Tax refund effect
    if(tax.refund.effect){
      taxpayers.return.weight <- get.taxpayers.return.weight(pop.data$tax.rate,
                                                             pop.data$c1,
                                                             state.at.time.t[, 'c1.tilde'],
                                                             return.weight)
      
      inds <- with(state.at.time.t,  refund.return)
      tmp.weight <- taxpayers.return.weight[inds]
      state.at.time.t[inds, 'Delta.Per'] <- with(state.at.time.t,
                                                 (1-tmp.weight)*Delta.Per[inds]+ tmp.weight*1)
      inds <- with(state.at.time.t, (!refund.return))
      tmp.weight <- taxpayers.return.weight[inds]
      state.at.time.t[inds, 'Delta.Per'] <- with(state.at.time.t,
                                                 (1-tmp.weight)*Delta.Per[inds]+0)
    }
    
    Delta.Per <- state.at.time.t[, 'Delta.Per']
    
    #############################################################
    ### Step 1.2: Compute the Network Effect on Tax Morale: Delta.Network
    #############################################################
    
    if(network.effect){
      network.values <- with(state.at.time.t,
                             get.network.interactions(Delta.Per, tax.ids,
                                                      beta.network,nn.int))
      Delta.Network <- network.values$unweighted.neighbours.influence
      state.at.time.t[, 'Delta.Network'] <- Delta.Network
      effective.beta.network <- network.values$beta.network
    }else{
      Delta.Network <- 0
      effective.beta.network <- 0
    }
    
    #############################################################
    ### Step 5: Compute the Media Feedback on Tax Morale: Delta.Media
    #############################################################
    if(media.effect){
      Delta.Media <- get.media.tax.morale.effect(tax.gap,
                                                 media.mid.effect,
                                                 media.steepness,
                                                 media.stochastic.offset)
      effective.beta.media <- beta.media*
        as.numeric(tax.gap>= tax.gap.reporting.media.threshold)*
        as.numeric(Delta.Per>=Delta.Media)
    }else{
      Delta.Media <- 0
      effective.beta.media <- 0
    }
    
    #############################################################
    ### Step 1.3: Compute Overall Delta.Morale
    #############################################################
    
    state.at.time.t[, 'Delta.Morale'] <- with(state.at.time.t,
                                              (beta.personal*Delta.Per+
                                                 effective.beta.network*Delta.Network+
                                                 effective.beta.media*Delta.Media)/
                                                (beta.personal+effective.beta.network+effective.beta.media))
    
    #############################################################
    ### Step 1.4: Compute Exponentially Weighted Moving Average (EWMA) of past evaluations
    #############################################################
    
    ### Step A: Tally up the experiences with tax compliance and discount past experiences wrt present.
    state.at.time.t$V.new[!past.year.penalized] <-
      s.discount*V.past[!past.year.penalized] +
      state.at.time.t[!past.year.penalized,'Delta.Morale']
    
    ### Step B: Compute the maximum value that the Tally could be
    V.max <- s.discount*V.max+1  ### in the large t limit this is just = 1/(1-s.discount)
    
    ### Step C: Those who were penalized have their prior experiences strongly diminished wrt to the present penalty
    phi <- (s.discount/(1-s.discount))*(v.PP/(1-v.PP))
    state.at.time.t$V.new[past.year.penalized] <-
      (s.discount*V.past[past.year.penalized]+
         phi*Delta.Penalty[past.year.penalized])*V.max/
      (s.discount*V.max+phi*Delta.Penalty[past.year.penalized])
    
    ### Step D: Find proportion of income that the taxpayer wishes to report
    state.at.time.t$propensity.prop.income.report <-  pmin(state.at.time.t$V.new/V.max,1)
    state.at.time.t$propensity.prop.income.report[state.at.time.t$propensity.prop.income.report<0.10] <- 0
    
    ### Step E: Determine if taxpayers are fully compliant this year:
    ### Those that were not compliant last year nor penalized last year will not be fully compliant
    state.at.time.t$compliant[non.compliant.non.penalized] <- FALSE
    ### Unless their propensity.prop.income.report reaches 100%
    state.at.time.t$compliant[state.at.time.t$propensity.prop.income.report>=0.95] <- TRUE
    
    ### Those who were compliant or have been penalized last year are susceptible to initiate evasion.
    suseptibles.to.underreport.pop  <- (compliant.or.penalized &
                                          state.at.time.t$propensity.prop.income.report<1)
    
    
    #############################################################
    ### Step 1.5: Taxpayers Compliance Decision and Income Amount to Report
    #############################################################
    
    ### Determine which of the susceptible taxpayers initiate in tax evasion.
    initiation.prob <- 1-state.at.time.t$propensity.prop.income.report[suseptibles.to.underreport.pop]
    initation <- as.logical(sapply(initiation.prob,stoch.round))
    ### Those that initiate have compliant set to FALSE (i.e., are no longer compliant)
    state.at.time.t$compliant[suseptibles.to.underreport.pop] <- !(initation)
    
    ### Interpret these as probabilities to fully-comply:
    state.at.time.t$w <- state.at.time.t$propensity.prop.income.report
    state.at.time.t$w[non.compliant.non.penalized] <- 0
    state.at.time.t$w[state.at.time.t$propensity.prop.income.report==1] <- 1
    state.at.time.t$w[suseptibles.to.underreport.pop] <- 1-initiation.prob
    
    ### Determine the Amount Reported by those fully compliant
    state.at.time.t[, 'report.abs'] <- pop.data$income
    ### Determine the Amount Reported by those non-compliant
    compliant <- state.at.time.t$compliant
    prop.reported <-state.at.time.t$propensity.prop.income.report[!(compliant)]
    #prop.reported[prop.reported<0.8]<- floor(10*prop.reported[prop.reported<0.8])/10
    state.at.time.t$report.abs[!(compliant)] <- pop.data$income[!(compliant)]*
      ((1-pop.data$prop.hideable.income[!(compliant)]) +
         pop.data$prop.hideable.income[!(compliant)]*prop.reported)
    rm(compliant)
    ### Round to the nearest dollar.
    state.at.time.t$report.abs<- round(state.at.time.t$report.abs,0)
    ### Report gives the percetage of income reported.
    state.at.time.t[, 'report'] <- round(100*(state.at.time.t$report.abs/pop.data$income))
    state.at.time.t[, 'hideable.reported'] <- 100 *
      (state.at.time.t$report - (100 - pop.data$perc.hideable.income))/pop.data$perc.hideable.income
    state.at.time.t$hideable.reported <- ifelse(state.at.time.t$hideable.reported < 0, 0, state.at.time.t$hideable.reported)
    
    #############################################################
    ### Step 1.6: Update Compliance History
    #############################################################
    
    ### Track number of years since taxpayer was last fully compliant.
    state.at.time.t$years.since.last.compliant <- with(state.at.time.t,
                                                       years.since.last.compliant+as.numeric(!compliant))
    state.at.time.t$years.since.last.compliant[state.at.time.t$compliant] <- 0
    
    compliant.history <- append.to.history2(compliant.history,
                                            state.at.time.t$compliant,K,label="compliant")
    
    amount.under.reporting.history <- append.to.history2(amount.under.reporting.history,
                                                         pop.data$income-state.at.time.t$report.abs,
                                                         K,label="amount.under.reported")
    
    perc.reporting.history <- append.to.history2(perc.reporting.history,
                                                 100*(state.at.time.t$report.abs/pop.data$income),
                                                 K,label="percent.reported")
    
    perc.hideable.reporting.history <- append.to.history2(perc.hideable.reporting.history,
                                                          state.at.time.t$hideable.reported,
                                                          K,label="percent.hideable.reported")
    
    
    
    
    
    #############################################################
    ###
    ###   Step 2: Tax collecting agency or IRS decides who to audit and penalize
    ###
    #############################################################
    
    #############################################################
    ### Step 2.1: Apply Deterrence Strategy and Sample taxpayers to be Audited
    #############################################################
    
    audit <- rep(FALSE,N)
    
    ### Do period.before.next.audit.set experiment
    if((t-tref)%%period.before.next.audit.set!=0){
      audit.rate.tmp <- 0
    }else{
      #print(paste(t,abs(period.before.next.audit.set), sep=" "))
      audit.rate.tmp <- abs(period.before.next.audit.set*audit.rate)
      if(period.before.next.audit.set<0){
        period.before.next.audit.set<- -round(9*runif(1)+1) 
        tref<-t
      }
    }
    
    ### Number of Audits the IRS will conduct
    num.of.audits <- round(rnorm(1, mean = N*audit.rate.tmp, 
                                 sd = coeff.dispersion.audits*N*audit.rate.tmp))
    #Intializing
    eff.back.audits <- list(eff.list=NULL,K.list=NULL)
    ids.by.audit.type<- list(ids.correpondence.audits=NULL,
                              ids.field.audits=NULL)
    audit.costs.info <- c(total=0, mean=0)
    
    if(num.of.audits > 0) {
      ### Sample the taxpayers to audit using the selection strategy probabilities
      if(targetted.auditing){
        ### Specification of the audit selection strategy
        audit.selection.strategy <- get.IRS.prob.of.audit(pop.data,state.at.time.t,
                                                          table.audit.rates,
                                                          additional.criteria=NULL)
      }else{
        ### Random audit
        audit.selection.strategy<- rep(1,N)
      }
      
      ### Sample the taxpayers to audit
      target.audit <- sample(tax.ids,num.of.audits,replace = FALSE,
                             prob=audit.selection.strategy)
      
      audit[target.audit]<- TRUE
      
      ### Classify the audits by type of audit
      ids.by.audit.type <- get.audits.by.correpondence(target.audit,
                                                       pop.data,prop.of.correpondence.audit = (detection.eff - 0.7*0.25)/0.75)
      
      eff.back.audits <- get.efficacies.and.backaudits.years(ids.by.audit.type, state.at.time.t,
                                                             backaudits=c(corresp=1,field=K))
      
      state.at.time.t[, 'audit.type'] <- NA
      state.at.time.t[ids.by.audit.type$ids.correpondence.audits, 'audit.type'] <- 'correspondence'
      state.at.time.t[ids.by.audit.type$ids.field.audits, 'audit.type'] <- 'field'
      
      audit.costs.info <- aggrgate.audit.costs(pop.data,ids.by.audit.type,tab.audit.costs)
    }
    
    #############################################################
    ### Step 2.2: Find Who gets caught for tax evasion
    #############################################################
    
    ### Find those that are audited and not compliant.
    audited.non.compliant <- audit & (!state.at.time.t$compliant)
    
    ### Find those that are penalized from those audited and not compliant.
    state.at.time.t[, 'penalized'] <- rep(FALSE,N)
    #state.at.time.t$penalized[audited.non.compliant] <- as.logical(sapply(eff.back.audits$eff.list,stoch.round))
    
    eff.vector <- rep(0.0, N)
    if(!is.null(eff.back.audits$eff.list)) {
      state.at.time.t$penalized[audited.non.compliant] <- TRUE
      eff.vector[audited.non.compliant] <- eff.back.audits$eff.list
    }
    
    
    ids.pen.by.audit.type <- get.ids.pen.by.audit.type(ids.by.audit.type,audited.non.compliant, tax.ids = pop.data$tax.ids)
    
    #############################################################
    ### Step 2.3: Update Audit & Penalty History
    #############################################################
    ### Update that number of audits each tax payer experienced.
    audit.history <- append.to.history2(audit.history, audit, K, label="audited")
    state.at.time.t$freq.audits <- state.at.time.t$freq.audits+as.numeric(audit)
    
    state.at.time.t$years.since.last.audit <- state.at.time.t$years.since.last.audit+1
    state.at.time.t$years.since.last.audit[audit]<-0
    state.at.time.t[, "audited"] <- audit.history[, "audited.0.years.ago"]
    
    ### Update penalized history.
    penalty.history <- append.to.history2(penalty.history, state.at.time.t$penalized, K,label="penalty")
    state.at.time.t$freq.penalty <- state.at.time.t$freq.penalty +
      as.numeric(state.at.time.t$penalized)
    
    #############################################################
    ### Step 2.4: Calculate Past Taxes Due and Penalties
    #############################################################
    
    years.to.be.penalized <- get.years.to.be.penalized(penalty.history, ids.pen.by.audit.type,eff.back.audits)
    sum.non.penalized.past.under.reporting <- rowSums(amount.under.reporting.history*years.to.be.penalized)
    
    past.tax.to.pay <- as.numeric(state.at.time.t$penalized)*eff.vector*
      (pop.data$tax.rate)*sum.non.penalized.past.under.reporting
    
    penalty.to.pay <- past.tax.to.pay*penalty.rate
    
    penalty.and.past.tax.to.pay <- past.tax.to.pay + penalty.to.pay
    state.at.time.t[, 'penalty.and.past.tax.to.pay'] <- penalty.and.past.tax.to.pay
    state.at.time.t[, 'past.tax.to.pay'] <- past.tax.to.pay
    
    ### Correct penalized history by recording that an audited and penalized
    ### individual has paid his/her past K years of penalties.
    penalty.history <- correct.past.penalties(penalty.history)
    
    ### Update years since last penalizes
    state.at.time.t$years.since.last.penalty <- state.at.time.t$years.since.last.penalty+1
    state.at.time.t$years.since.last.penalty[state.at.time.t$penalized] <- 0
    
    #############################################################
    ### Step 2.5: Evaluate Delta.Penalty by
    #############################################################
    
    ### Express the total penalty as a fraction of the expected tax revenue owed.
    Delta.Penalty <-penalty.and.past.tax.to.pay/
      (pop.data$tax.rate*pop.data$income)
    
    #############################################################
    ### Step 2.6: Update who potentially gets a tax refund and who doesn't
    #############################################################
    if(tax.refund.effect){
      #############################################################
      ###        Model a simple Markov Process for refund return
      #############################################################
      additional.to.refund <-  get.additional.to.refund(state.at.time.t$refund.return,
                                                        pop.data$refund.group,
                                                        rate.refund.movement*table.refund$prop)
      additional.to.refund <- (additional.to.refund ) | state.at.time.t$penalized
      
      refund.to.additional <- get.refund.to.additional(state.at.time.t$refund.return,
                                                       pop.data$refund.group,
                                                       rate.refund.movement*(1-table.refund$prop))
      
      state.at.time.t[additional.to.refund, 'refund.return'] <- TRUE
      state.at.time.t[refund.to.additional, 'refund.return'] <- FALSE
    }
    
    
    
    
    #############################################################
    ###
    ###        Step 3: Update Risk Perceptions of Audit and Penalty
    ###
    #############################################################
    
    #############################################################
    ### Step 3.1: Find components that affect the  Perceived Audit Rate
    #############################################################
    
    ### Step 3.1A: Find whether the taxpayer was audited.
    
    ### Step 3.1B: Find if any alters have been audited
    alters.audited <- 0
    if(network.effect){
      alters.audited <- any.nn.audit(audit,tax.ids,nn.int)
    }
    
    ### Step 3.1C: Find media effect on perceived audit
    taxpayer.pays.attention.to.media<- FALSE
    if(media.effect){
      
      ### find total dollar amount of recovered rev. & penalties
      recovered.revenue.plus.penalties <-  sum(penalty.and.past.tax.to.pay, na.rm = T)
      
      ### Media picks up and reports year of higher than normal IRS activity
      extraordinary.num.audits<-upper.tail.z.test(num.of.audits,
                                                  seq.num.of.audits,
                                                  alpha=0.999)
      extraordinary.tot.penalties<-upper.tail.z.test(recovered.revenue.plus.penalties,
                                                     seq.recovered.revenue.plus.penalties,
                                                     alpha=0.999)
      if( t>t.media.range &
          (extraordinary.num.audits )){ #| extraordinary.tot.penalties
        
        ### Assume that the media reports how many are audited by income breakdown.
        reported.who.audited.by.income <-data.frame(frac=c(0:10)/10,
                                                    income=quantile(pop.data$income[audit],
                                                                    prob=c(0:10)/10))
        
        position <- find.closest.entry(reported.who.audited.by.income$income,
                                       pop.data$income)
        
        prob.of.paying.attention<- pmax(reported.who.audited.by.income$frac[position],
                                        pop.data$actor.logical)
        
        taxpayer.pays.attention.to.media<-sapply(prob.of.paying.attention,stoch.round)
      }
      
      extraordinary.num.audits<-lower.tail.z.test(num.of.audits,
                                                  seq.num.of.audits,
                                                  alpha=0.001)
      
      extraordinary.tot.penalties<-lower.tail.z.test(recovered.revenue.plus.penalties,
                                                     seq.recovered.revenue.plus.penalties,
                                                     alpha=0.001)
      if( t>t.media.range &
          (extraordinary.num.audits | extraordinary.tot.penalties)){
        
        ### Assume that the media reports how many are audited by income breakdown.
        reported.who.audited.by.income <-data.frame(frac=c(0:10)/10,
                                                    income=quantile(pop.data$income[audit],
                                                                    prob=c(0:10)/10))
        
        position <- find.closest.entry(reported.who.audited.by.income$income,
                                       pop.data$income)
        
        ### if audit rates decrease as reported by media and most people audited earn more than me:
        ### I pay attention that this decrease is real and affects me
        prob.of.paying.attention<- 1-reported.who.audited.by.income$frac[position]
        
        taxpayer.pays.attention.to.media.dec<-sapply(prob.of.paying.attention,stoch.round)
      }
      
      seq.num.of.audits<- get.last.number.of.year(seq.num.of.audits,
                                                  num.of.audits,
                                                  t.media.range)
      seq.recovered.revenue.plus.penalties <- get.last.number.of.year(
        seq.recovered.revenue.plus.penalties,recovered.revenue.plus.penalties,
        t.media.range)
      
    }else{
      taxpayer.pays.attention.to.media<- FALSE
    }
    
    #############################################################
    ### Step 3.2: Obtain the Effective Weighted Audit Observation
    #############################################################
    state.at.time.t[, 'Delta.Q'] <-
      (beta.personal*as.numeric(audit)+
         beta.network*alters.audited+
         beta.media*taxpayer.pays.attention.to.media)/
      (beta.personal+ beta.network*alters.audited+
         beta.media*taxpayer.pays.attention.to.media)
    
    state.at.time.t$Q.new <- s.audit.discount*Q.past + state.at.time.t$Delta.Q
    Q.max <- s.audit.discount*Q.max+1
    state.at.time.t[, 'w.Q'] <- state.at.time.t$Q.new/Q.max
    
    #############################################################
    ### Step 3.3: Decay the baseline Perceived audit rate if the true Audit rate is below the baseline
    #############################################################
    state.at.time.t$transformed.per.audit.rate<- check.n.modify.baseline.perceptions(
      transformed.per.audit.rate.t.minus.1,
      init.transformed.per.audit.rate,
      s.audit.discount,q=audit.rate,q.base=0.008)
    ### The base audit rate (q.base) of 0.8% is set since this was the true audit prob when the ALP survey was fielded
    
    #############################################################
    ### Step 3.4: Update the Perceived Audit rate via the EWMA
    #############################################################
    state.at.time.t$per.audit.rate <- with(state.at.time.t,
                                           (transformed.per.audit.rate+w.Q)/(transformed.per.audit.rate+1))
    
    #############################################################
    ### Step 3.5: Modify the Perceived Audit rate due to the Gamblers Fallacy Effect
    #############################################################
    if(gamblers.fallacy){
      ### Find those that are not audited and not compliant.
      non.audited.non.compliant <- (!audit) & (!state.at.time.t$compliant)
      ### Induce cold feet effect on these individuals -
      ### by increasing their audit risk perception
      if(any(non.audited.non.compliant))
      {
        state.at.time.t$per.audit.rate[non.audited.non.compliant] <-
          get.per.audit.gamblers.fallacy(state.at.time.t$per.audit.rate[non.audited.non.compliant],
                                         state.at.time.t$propensity.prop.income.report[non.audited.non.compliant],
                                         state.at.time.t$years.since.last.audit[non.audited.non.compliant],
                                         s.audit.discount,
                                         gamblers.fallacy.grad,
                                         gamblers.fallacy.intercept)
      }
    }
    
    #############################################################
    ### Step 3.6: Modify the Perceived Audit rate due to the Bomb Crater Effect
    #############################################################
    if(bomb.crater.effect){
      ### Find those that are audited and not penalized.
      audited.non.penalized <- audit & (!state.at.time.t$penalized)
      ### Reduce percieved risk of getting audited again.
      if(any(audited.non.penalized))
      {
        state.at.time.t$per.audit.rate[audited.non.penalized] <-
          get.per.audit.bomb.crater(state.at.time.t$per.audit.rate[audited.non.penalized],
                                    state.at.time.t$years.since.last.audit[audited.non.penalized],
                                    s.audit.discount,
                                    bomb.crater.factor)
      }
      
    }
    
    #############################################################
    ### Step 3.7: Temporarily Increase number of alter for those who were audited.
    #############################################################
    # PK: Removing this chunk of code since it takes a lot of time.
    # if(network.effect){
    # g.info <- add.edges.due.to.an.audit(ids=tax.ids[audit],
    #                                     edgelist.g,nn.int,
    #                                     net.degree.info["audit.eff"])
    # edgelist.g <- g.info$edgelist.g
    # nn.int <- g.info$nn.int   ### RV I need some help here to put this in the with(state.at.time.t, ) framework.
    # }
    
    #############################################################
    ### Step 3.8: Modify the Perceived Penalty Rate
    #############################################################
    state.at.time.t$per.penalty.rate <- s.generation.discount*state.at.time.t$per.penalty.rate +
      (1-s.generation.discount)* pop.data$per.penalty.rate
    penalized.or.media.network.affected <- as.logical(sapply(state.at.time.t[, 'Delta.Q'],stoch.round))
    state.at.time.t$per.penalty.rate[penalized.or.media.network.affected] <- penalty.rate
    
    
    
    #############################################################
    ###
    ###   Step 4: Compute the Aggregates
    ###
    #############################################################
    
    ### Compute the tax gap
    tax.gap <- compute.tax.gap(pop.data, state.at.time.t)
    
    #############################################################
    ###
    ###    Step 6: Track Outputs  (other outputs that do not affect feedback loops)
    ###
    #############################################################
    
    targets <- calculate.important.targets(state.at.time.t, audit.history, penalty.history,
                                           perc.reporting.history, perc.hideable.reporting.history)
    Rannoyed <- c(Rannoyed, targets$Rannoyed)
    hideable.Rannoyed <- c(hideable.Rannoyed, targets$hideable.Rannoyed)
    mean.perc.report.by.comp.aud.3yrs.ago <- c(mean.perc.report.by.comp.aud.3yrs.ago, targets$mean.perc.report.by.comp.aud.3yrs.ago)
    mean.hideable.report.by.comp.aud.3yrs.ago <- c(mean.hideable.report.by.comp.aud.3yrs.ago, targets$mean.hideable.report.by.comp.aud.3yrs.ago)
    
    R0 <- c(R0, targets$R0)
    R1 <- c(R1, targets$R1)
    R3 <- c(R3, targets$R3)
    
    hideable.R0 <- c(hideable.R0, targets$hideable.R0)
    hideable.R1 <- c(hideable.R1, targets$hideable.R1)
    hideable.R3 <- c(hideable.R3, targets$hideable.R3)
    
    ## Assigning histories back to state.at.time.t
    compliant.history -> state.at.time.t[, names(compliant.history)]
    penalty.history -> state.at.time.t[, names(penalty.history)]
    audit.history -> state.at.time.t[, names(audit.history)]
    amount.under.reporting.history -> state.at.time.t[, names(amount.under.reporting.history)]
    perc.reporting.history -> state.at.time.t[, names(perc.reporting.history)]
    perc.hideable.reporting.history -> state.at.time.t[, names(perc.hideable.reporting.history)]
    
    num.audited <- length(which(state.at.time.t$audited))
    num.penalized <- length(which(state.at.time.t$penalized))
    num.compliant <- length(which(state.at.time.t$compliant))
    
    assort.measure <- get.compliance.assortativity.measure(g, state.at.time.t,
                                                           field ="hideable.reported",
                                                           threshold=0.02)
    am.list <- assort.measure$assortativity.measures[, 2]
    names(am.list) <- assort.measure.names
    
    track.dyn[[t]] <-  state.at.time.t
    aggregated.dyn <-  rbind(aggregated.dyn, c(t=t,
                                               tax.gap=tax.gap,
                                               expected.tax.revenue = sum(pop.data$tax.rate*pop.data$income),
                                               tax.revenue = sum(pop.data$tax.rate*state.at.time.t$report.abs),
                                               mean.audit.costs = audit.costs.info["mean"],
                                               media.feedback=(1-Delta.Media),
                                               mean.per.audit.rate=mean(state.at.time.t$per.audit.rate),
                                               sd.per.audit.rate=sd(state.at.time.t$per.audit.rate),
                                               mean.per.penalty.rate = mean(state.at.time.t$per.penalty.rate),
                                               sd.per.penalty.rate = sd(state.at.time.t$per.penalty.rate),
                                               num.audited = num.audited,
                                               num.penalized = num.penalized,
                                               num.compliant = num.compliant,
                                               total.penalty.and.past.tax.to.pay=sum(penalty.and.past.tax.to.pay),
                                               total.past.tax.to.pay=sum(past.tax.to.pay),
                                               am.list))
    
    
    ########################################
    ##      Government Dynamics           ##
    ########################################
    
    if(!is.null(initial.state)) { 
      #Only if an initial.state is provided, execute this
      #This ensures, we don't calculate gov.dyn for pre-equilibrium state of the model
      #Since calculating gov.dyn before reaching equilibrium does not make sense in this model
      gov.dyn.at.t <- get.gov.dynamics(pop.data, state.at.time.t, 
                                       tab.audit.costs, ids.by.audit.type, alpha_C = alpha_c,
                                       US.other.government.revenues = other.rev, US.Debt=US.Debt,
                                       US.marginal.Debt=US.marginal.Debt, scale.factor = gd.scale.factor)
      
      inflated.indicators <- get.Debt.and.GDP.with.growth(year=t,
                                                          marginal.deficit=US.marginal.deficit,
                                                          previous.year.inflated.Debt= previous.year.inflated.Debt,
                                                          initial.deficit=initial.deficit,
                                                          initial.interest.payments=241*10^9,
                                                          initial.GDP = 18.46*10^12,
                                                          GDP.growth.rate = 1.0241, 
                                                          Debt.interest.rate = (1+241*10^9/(19.59*10^12)))
      
      gov.dyn.at.t <- cbind.data.frame(gov.dyn.at.t,inflated.indicators)
      
      #Overwriting these variables so that they are ready for the next iteration of the loop
      US.marginal.deficit <- gov.dyn.at.t$US.marginal.deficit
      US.marginal.Debt <- gov.dyn.at.t$US.marginal.Debt
      US.Debt <- gov.dyn.at.t$US.Debt
      previous.year.inflated.Debt <- gov.dyn.at.t$inflated.Debt
      
      gov.dyn <- rbind.data.frame(gov.dyn, gov.dyn.at.t) 
    }
    
    
    
    #########################################################
    ##      Check if equilibrium has been reached          ##
    #########################################################
    tg.diff.vec <- diff(aggregated.dyn[, 'tax.gap'])
    
    if(t > generation.half.life*10) {
      #Only after sufficient time has passed start checking for equilibrium
      #so that there are enough observations
      eq <- check.for.equilibrium(tg.diff.vec) #Half a percentage point
      if(eq == TRUE) {
        #print(paste("Equilbrium has been reached at:", t))
        equilibrium.reached.at <- min(t, equilibrium.reached.at)
        is.in.equilibrium <- TRUE
        #### Important this part should only be run after the system has reached a steady state. 
        baseline.gov.dyn <- get.baseline.gov.dyn(pop.data, state.at.time.t, aggregated.dyn, 
                                                 tab.audit.costs, generation.half.life)
        
        if(run.till.equilibrium) break #Getting out of the while loop after equilibrium is reached
        
      } else if(t == final.year) {
        #print(paste("increasing the final year to:", final.year+10, "Current t:", t))
        if(run.till.equilibrium) final.year <- final.year + 10
        is.in.equilibrium <- FALSE
      } else if(t >= 1000) {
        print("Equilibrium was not reached at the end of 1000 years; breaking manually to avoid infintie loop!")
        break #Equilbrium was not reached
      }
      
    }
  }
  
  retval <- list(R0=R0, R1=R1, R3=R3,
                 Rannoyed=Rannoyed, hideable.Rannoyed = hideable.Rannoyed,
                 hideable.R0 = hideable.R0, hideable.R1 = hideable.R1, hideable.R3 = hideable.R3,
                 mean.perc.report.by.comp.aud.3yrs.ago=mean.perc.report.by.comp.aud.3yrs.ago,
                 mean.hideable.report.by.comp.aud.3yrs.ago = mean.hideable.report.by.comp.aud.3yrs.ago,
                 track.dyn=track.dyn,
                 aggregated.dyn=aggregated.dyn,
                 gov.dyn = gov.dyn,
                 baseline.gov.dyn = baseline.gov.dyn,
                 final.state=state.at.time.t,
                 equilibrium.reached = equilibrium.reached.at)
  
  return(retval)
}
