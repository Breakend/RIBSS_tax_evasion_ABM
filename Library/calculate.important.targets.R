
calculate.important.targets <- function(state.at.time.t, audit.history, penalty.history, 
                                        perc.reporting.history, perc.hideable.reporting.history) {
  
  ### The annoyed popultion are those audited that are not found to underreport 
  ### and thus were not penalized. 
  audit.var <- paste("audited", 3, "years.ago", sep = '.')
  penalty.var <- paste("penalty", 3, "years.ago", sep = '.')
  percent.reported.var <- paste("percent.reported", 3, "years.ago", sep = '.')
  hideable.reported.var <- paste("percent.hideable.reported", 3, "years.ago", sep = '.')
  
  pop.annoyed <- (audit.history[, audit.var]) & 
    !(penalty.history[, penalty.var]) &
    (perc.reporting.history[, percent.reported.var]>0)
  tmp.reported.0.years.ago <- state.at.time.t$report[pop.annoyed]
  tmp.reported.3.years.ago <- perc.reporting.history[pop.annoyed, percent.reported.var]
  
  tmp.hideable.reported.0.years.ago <- state.at.time.t$hideable.reported[pop.annoyed]
  tmp.hideable.reported.3.years.ago <- perc.hideable.reporting.history[pop.annoyed, hideable.reported.var]
  
  Rannoyed <- mean(1 - (tmp.reported.0.years.ago/tmp.reported.3.years.ago))
  hideable.Rannoyed <- mean(1 - (tmp.hideable.reported.0.years.ago/tmp.hideable.reported.3.years.ago))
  
  comp.aud.3yrs.ago <- audit.history[, audit.var] & 
    (perc.reporting.history[, percent.reported.var] > 95)
  mean.perc.report.by.comp.aud.3yrs.ago <- mean(perc.reporting.history[comp.aud.3yrs.ago, percent.reported.var])
  
  comp.aud.hideable.3yrs.ago <- audit.history[, audit.var] & 
    (perc.hideable.reporting.history[, hideable.reported.var] > 95)
  mean.hideable.report.by.comp.aud.3yrs.ago <- mean(perc.hideable.reporting.history[comp.aud.hideable.3yrs.ago, hideable.reported.var])
  
  ### The pop0 population is the population penalized in the current year.
  ### pop1 and pop3 those penalized 1 and 3 years ago. 
  pop0<- (state.at.time.t$years.since.last.penalty==0)
  pop1<- (state.at.time.t$years.since.last.penalty==1)
  pop3<- (state.at.time.t$years.since.last.penalty==3)
  
  
  R0<- mean(state.at.time.t$report[pop0])
  R1<- mean(state.at.time.t$report[pop1])
  R3<- mean(state.at.time.t$report[pop3])
  
  hideable.R0<- mean(state.at.time.t$hideable.reported[pop0])
  hideable.R1<- mean(state.at.time.t$hideable.reported[pop1])
  hideable.R3<- mean(state.at.time.t$hideable.reported[pop3])
  
  invisible(list(R0=R0, R1=R1, R3=R3,
                 Rannoyed=Rannoyed, hideable.Rannoyed = hideable.Rannoyed,
                 hideable.R0 = hideable.R0, hideable.R1 = hideable.R1, hideable.R3 = hideable.R3, 
                 mean.perc.report.by.comp.aud.3yrs.ago=mean.perc.report.by.comp.aud.3yrs.ago,
                 mean.hideable.report.by.comp.aud.3yrs.ago = mean.hideable.report.by.comp.aud.3yrs.ago))
}