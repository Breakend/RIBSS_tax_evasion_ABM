#############################################################
###                                                       ###
###        Run Dynamics              
###                                                       ###
#############################################################

run.dynamics <- function(pop.data, initial.state=NULL, config, g.info, 
                         table.audit.rates, table.refund, 
                         final.year = NULL, cl=NULL)
{
  track.dyn <- list()
  aggregated.dyn <- NULL
  create.config.vars(config)
  rm(tax.rate)
  
  N <- nrow(pop.data)
  tax.ids <- pop.data$tax.ids
  
  #Network information
  edgelist.g<- g.info$edgelist.g
  nn<- g.info$nn
  nn.int <- g.info$nn.int
  net.degree.info <- g.info$net.degree.info
  
  
  ##############################################
  ## Carry over data from one complete run of
  ## the model to the next.
  ##############################################
  if(is.null(initial.state)) {
    state.at.time.t <- initialize.track.dynamics(pop.data)
    initial.media.feeback <- generate.media.feedback(beta.media, 
                                                     tax.gap = 0, 
                                                     media.mid.effect, 
                                                     media.steepness, 
                                                     media.stochastic.offset)
    aggregated.dyn <-  rbind(aggregated.dyn, c(t=1,
                                               tax.gap=0,
                                               media.feedback=initial.media.feeback,
                                               mean.per.audit.rate=mean(pop.data$per.audit.rate),
                                               sd.per.audit.rate=sd(pop.data$per.audit.rate),
                                               mean.per.penalty.rate = mean(pop.data$per.penalty.rate),
                                               sd.per.penalty.rate = sd(pop.data$per.penalty.rate)))
  }else {
    state.at.time.t <- initial.state
    state.at.time.t[, 't'] <- rep(1, N)
    
    expected.tax.revenue<- sum(pop.data$tax.rate*pop.data$income)
    tax.revenue <- sum(pop.data$tax.rate*state.at.time.t$report.abs)
    tax.gap.amount <- expected.tax.revenue- tax.revenue
    tax.gap <- tax.gap.amount/expected.tax.revenue
    
    initial.media.feeback <- get.media.tax.morale.effect(tax.gap, 
                                                         media.mid.effect, 
                                                         media.steepness, 
                                                         media.stochastic.offset)
    
    aggregated.dyn <-  rbind(aggregated.dyn, c(t=1,
                                               tax.gap=tax.gap,
                                               media.feedback=initial.media.feeback,
                                               mean.per.audit.rate=mean(state.at.time.t$per.audit.rate),
                                               sd.per.audit.rate=sd(state.at.time.t$per.audit.rate),
                                               mean.per.penalty.rate = mean(state.at.time.t$per.penalty.rate),
                                               sd.per.penalty.rate = sd(state.at.time.t$per.penalty.rate)))
  }
  ## End of carrying over data across runs
  
  
  ################################################
  ## Initialize all required variables
  ################################################
  track.dyn[[1]] <- state.at.time.t
  
  R0<-R1<-R3<-Rannoyed<- NULL
  mean.report.by.comp.aud.3yrs.ago <- NULL
  V.max <- 0
  
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
  Delta.Penalty <- 0
  
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
  equilibrium.reached <- Inf
  
  for(t in 2:final.year){
    
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
    past.year.audited.non.compliant <- state.at.time.t$audited.non.compliant
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
    
    ########################################################################
    ###  Step 1.1: Compute the Personal Evaluation on Tax Morale: Delta.Per   
    ########################################################################
    
    ### Determine the c1.tilde 
    state.at.time.t[, 'c1.tilde'] <- with(state.at.time.t, 
                                          get.c1.tilde(pop.data$c1,c2,pop.data$tax.rate,per.audit.rate,
                                                       per.penalty.rate*penalty.asymmetry.factor,m.qP,s.qP))
    
    ### Sanity Check Assertion. 
    assertthat::assert_that(any(state.at.time.t[, 'c1.tilde'] < 1.0))
    
    ### Determine Delta.Per for those who are either compliant 
    if(any(compliant.or.penalized)){
      state.at.time.t[compliant.or.penalized, 'Delta.Per'] <- 
        with(pop.data[compliant.or.penalized, ], 
             fairness.fn(state.at.time.t[compliant.or.penalized, 'c1.tilde'],
                         c1,c2,tax.rate))
      
    }
    
    ### Determine Delta.Per for those who are either . 
    if(any(non.compliant.non.penalized)){
      ### Non compliant have Delta.Per = 0 thus strongly tending towards full evasion. 
      state.at.time.t[non.compliant.non.penalized, 'Delta.Per'] <- 0 
      
      ### if mild.tendency.to.full.evasion then this tendency is slowed and
      ### evaders will only tend increasingly towards full evasion 
      ### as they increasing under-report. 
      if(mild.tendency.to.full.evasion){
        state.at.time.t[, 'w.non'] <- state.at.time.t$propensity.prop.income.report
        state.at.time.t[compliant.or.penalized, 'w.non'] <- NA
        partial <- non.compliant.non.penalized & 
          sapply((state.at.time.t$w.non)^full.tendency.factor,stoch.round)
        if(any(partial)){
          state.at.time.t[partial, 'Delta.Per'] <- 
            fairness.fn(state.at.time.t[partial, 'c1.tilde'],
                        pop.data[partial, 'c1'],pop.data[partial, 'c2'],
                        pop.data[partial, 'tax.rate'])
        }
      }
    }
    
    ### Apply Tax refund effect
    if(tax.refund.effect){
      inds <- with(state.at.time.t,  refund.return )  
      state.at.time.t[inds, 'Delta.Per'] <- with(state.at.time.t, 
                                                 (1-return.weight)*Delta.Per[inds]+ return.weight*1)
      inds <- with(state.at.time.t, (!refund.return  )) 
      state.at.time.t[inds, 'Delta.Per'] <- (1-return.weight)*state.at.time.t[inds, 'Delta.Per']
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
    state.at.time.t$compliant[state.at.time.t$propensity.prop.income.report>=0.999] <- TRUE
    
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
    
    ### Round to the nearest dollar. 
    state.at.time.t$report.abs<- round(state.at.time.t$report.abs,0)
    ### Report gives the percetage of income reported. 
    state.at.time.t[, 'report'] <- round(100*(state.at.time.t$report.abs/pop.data$income))
  
    
    #############################################################
    ### Step 1.6: Update Compliance History         
    #############################################################
    
    ### Track number of years since taxpayer was last fully compliant.
    state.at.time.t$years.since.last.compliant <- with(state.at.time.t, 
                                                       years.since.last.compliant+as.numeric(!compliant))
    state.at.time.t$years.since.last.compliant[state.at.time.t$compliant] <- 0
    
    state.at.time.t$compliant.history <- with(state.at.time.t, 
                                              append.to.history2(compliant.history, 
                                                                 compliant,K,label="compliant"))
    state.at.time.t$amount.under.reporting.history <- with(state.at.time.t, 
                                                           append.to.history2(amount.under.reporting.history,
                                                                              pop.data$income-report.abs,
                                                                              K,label="amount.under.reported"))
    
    state.at.time.t$perc.reporting.history <- with(state.at.time.t, 
                                                   append.to.history2(perc.reporting.history,
                                                                      report.abs/pop.data$income,
                                                                      K,label="percent.reported"))
    
    
    
    
    
    #############################################################
    ###
    ###   Step 2: Tax collecting agency or IRS decides who to audit and penalize             
    ###
    #############################################################
    
    #############################################################
    ### Step 2.1: Apply Deterrence Strategy and Sample taxpayers to be Audited         
    #############################################################
    
    audit <- rep(FALSE,N)
    ### Number of Audits the IRS will conduct
    num.of.audits <- round(rnorm(1, mean = N*audit.rate, sd = coeff.dispersion.audits*N*audit.rate))
    
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
    
    #############################################################
    ### Step 2.2: Find Who gets caught for tax evasion       
    #############################################################
    
    ### Find those that are audited and not compliant. 
    audited.non.compliant <- audit & (!state.at.time.t$compliant)
    
    ### Find those that are penalized from those audited and not compliant.
    state.at.time.t[, 'penalized'] <- rep(FALSE,N)
    state.at.time.t$penalized[audited.non.compliant] <- runif(sum(audited.non.compliant),0,1) <= detection.eff
    
    #############################################################
    ### Step 2.3: Update Audit & Penalty History        
    #############################################################
    
    ### Update that number of audits each tax payer experienced.
    state.at.time.t$audit.history <- with(state.at.time.t, 
                                          append.to.history2(audit.history,audit,K,label="audited"))
    state.at.time.t$freq.audits <- state.at.time.t$freq.audits+as.numeric(audit)
    
    state.at.time.t$years.since.last.audit <- state.at.time.t$years.since.last.audit+1
    state.at.time.t$years.since.last.audit[audit]<-0
    
    ### Update penalized history.
    state.at.time.t$penalty.history <- with(state.at.time.t, 
                                            append.to.history2(penalty.history,
                                                               penalized,K,
                                                               label="penalty"))
    state.at.time.t$freq.penalty <- state.at.time.t$freq.penalty + 
      as.numeric(state.at.time.t$penalized)
    
    #############################################################
    ### Step 2.4: Calculate Past Taxes Due and Penalties         
    #############################################################
    
    years.to.be.penalized <- with(state.at.time.t,get.years.to.be.penalized(penalty.history))
    sum.non.penalized.past.under.reporting <- with(state.at.time.t,
                                                   rowSums(amount.under.reporting.history*years.to.be.penalized))
    
    past.tax.to.pay <- as.numeric(state.at.time.t$penalized)*
      (pop.data$tax.rate)*sum.non.penalized.past.under.reporting  
    
    penalty.to.pay <- past.tax.to.pay*penalty.rate
    
    penalty.and.past.tax.to.pay <- past.tax.to.pay + penalty.to.pay
    
    ### Correct penalized history by recording that an audited and penalized 
    ### individual has paid his/her past K years of penalties.
    state.at.time.t$penalty.history <- correct.past.penalties(state.at.time.t$penalty.history)
    
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
      recovered.revenue.plus.penalties <-  sum(penalty.and.past.tax.to.pay)
      
      ### Media picks up and reports year of higher than normal IRS activity
      extraordinary.num.audits<-upper.tail.z.test(num.of.audits,
                                                  seq.num.of.audits,
                                                  alpha=0.99)
      extraordinary.tot.penalties<-upper.tail.z.test(recovered.revenue.plus.penalties,
                                                     seq.recovered.revenue.plus.penalties,
                                                     alpha=0.99)
      if( t>t.media.range &
          (extraordinary.num.audits | extraordinary.tot.penalties)){
        
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
    pop.data$transformed.per.audit.rate<- check.n.modify.baseline.perceptions(
      pop.data$transformed.per.audit.rate,
      pop.data$transformed.per.audit.rate.0,
      s.audit.discount,q=audit.rate,q.base=0.008) 
    ### The base audit rate (q.base) of 0.8% is set since this was the true audit prob when the ALP survey was fielded
    
    #############################################################
    ### Step 3.4: Update the Perceived Audit rate via the EWMA
    #############################################################
    state.at.time.t$per.audit.rate <- with(pop.data, 
                                           (transformed.per.audit.rate+state.at.time.t$w.Q)/
                                             (transformed.per.audit.rate+1))
    
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
    expected.tax.revenue<- sum(pop.data$tax.rate*pop.data$income)
    tax.revenue <- sum(pop.data$tax.rate*state.at.time.t$report.abs)
    tax.gap.amount <- expected.tax.revenue- tax.revenue
    tax.gap <- tax.gap.amount/expected.tax.revenue
    
    #############################################################
    ###
    ###    Step 6: Track Outputs  (other outputs that do not affect feedback loops)      
    ###
    #############################################################
    
    if (ncol(state.at.time.t$audit.history)==4){ 
      ### The annoyed popultion are those audited that are not found to underreport 
      ### and thus were not penalized. 
      #browser()
      pop.annoyed <- with(state.at.time.t, (audit.history[, "audited.3.years.ago"] & 
                                              !(penalty.history[, "penalty.3.years.ago"]) &
                                              perc.reporting.history[, "percent.reported.3.years.ago"]>0))
      tmp.reported.0.years.ago <- state.at.time.t$report[pop.annoyed]
      tmp.reported.3.years.ago <- 100*state.at.time.t$perc.reporting.history[pop.annoyed, 
                                                                         "percent.reported.3.years.ago"]
      
      Rannoyed <- c(Rannoyed,mean(1 - (tmp.reported.0.years.ago/tmp.reported.3.years.ago)))
      
      comp.aud.3yrs.ago <- with(state.at.time.t, (audit.history[, "audited.3.years.ago"] &
                                                    perc.reporting.history[, "percent.reported.3.years.ago"] > 95))
      
      mean.report.by.comp.aud.3yrs.ago <- with(state.at.time.t, 
                                               mean(perc.reporting.history[, "percent.reported.0.years.ago"]))
      
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
                                               media.feedback=Delta.Media,
                                               mean.per.audit.rate=mean(state.at.time.t$per.audit.rate),
                                               sd.per.audit.rate=sd(state.at.time.t$per.audit.rate),
                                               mean.per.penalty.rate = mean(state.at.time.t$per.penalty.rate),
                                               sd.per.penalty.rate = sd(state.at.time.t$per.penalty.rate)))
    
    #########################################################
    ##      Check if equilibrium has been reached          ##
    #########################################################
    
    tax.gap.diff <- diff(aggregated.dyn[, 'tax.gap'])
    tax.gap.grad<- 0
    if(t <= generation.half.life) {
      tax.gap.grad <- mean(tax.gap.diff[1:t-1])*100 #Because there will be t - 1 differences for t elements
    } else {
      tax.gap.grad <- mean(tax.gap.diff[(t-generation.half.life):t-1])*100
    }
    
    if(abs(tax.gap.grad) < 0.005) {
      equilibrium.reached <- min(equilibrium.reached, t)
      
      #Break from the loop if run.till.equilibrium is TRUE
      if(run.till.equilibrium) break
    }
    
  }
  
  retval <- list(R0=R0, R1=R1, R3=R3,
                 Rannoyed=Rannoyed,
                 mean.report.by.comp.aud.3yrs.ago=mean.report.by.comp.aud.3yrs.ago,
                 track.dyn=track.dyn,
                 aggregated.dyn=aggregated.dyn,
                 final.state=state.at.time.t,
                 mean.report.by.comp.aud.3yrs.ago=mean.report.by.comp.aud.3yrs.ago,
                 equilibrium.reached = equilibrium.reached)
  
  return(retval)
}