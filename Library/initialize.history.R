initialize.history <- function(pop.size) {
  
  #############################################
  ## Initialize history
  ##
  #############################################
  
  N <- pop.size
  compliant.history  <- data.frame(compliant.0.years.ago = rep(TRUE, N),
                                   compliant.1.years.ago = rep(TRUE, N),
                                   compliant.2.years.ago = rep(TRUE, N),
                                   compliant.3.years.ago = rep(TRUE, N))
  
  penalty.history <- data.frame(penalty.0.years.ago = rep(FALSE, N),
                                penalty.1.years.ago = rep(FALSE, N),
                                penalty.2.years.ago = rep(FALSE, N),
                                penalty.3.years.ago = rep(FALSE, N))
  
  audit.history <- data.frame(audited.0.years.ago = rep(FALSE, N),
                              audited.1.years.ago = rep(FALSE, N),
                              audited.2.years.ago = rep(FALSE, N),
                              audited.3.years.ago = rep(FALSE, N))
  
  amount.under.reporting.history <- data.frame(amount.under.reported.0.years.ago = rep(0,N),
                                               amount.under.reported.1.years.ago = rep(0,N),
                                               amount.under.reported.2.years.ago = rep(0,N),
                                               amount.under.reported.3.years.ago = rep(0,N))
  
  perc.reporting.history <- data.frame(percent.reported.0.years.ago = rep(0,N),
                                       percent.reported.1.years.ago = rep(0,N),
                                       percent.reported.2.years.ago = rep(0,N),
                                       percent.reported.3.years.ago = rep(0,N))
  
  perc.hideable.reporting.history <- data.frame(percent.hideable.reported.0.years.ago = rep(0,N),
                                       percent.hideable.reported.1.years.ago = rep(0,N),
                                       percent.hideable.reported.2.years.ago = rep(0,N),
                                       percent.hideable.reported.3.years.ago = rep(0,N))
  
  
  return(list(compliant.history = compliant.history,
              penalty.history = penalty.history,
              audit.history = audit.history,
              amount.under.reporting.history = amount.under.reporting.history,
              perc.reporting.history = perc.reporting.history,
              perc.hideable.reporting.history = perc.hideable.reporting.history))

  # return(cbind(compliant.history,
  #             penalty.history,
  #             audit.history,
  #             amount.under.reporting.history,
  #             perc.reporting.history))
  
}