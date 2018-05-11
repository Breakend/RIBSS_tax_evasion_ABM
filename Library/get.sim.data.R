
get.sim.data <- function(final.state, population.data,
                         amount.under.reporting.history,
                         perc.reporting.history,
                         perc.hideable.reporting.history,
                         compliant.history,
                         audit.history,
                         penalty.history) {
 
  sim.data <-  with(final.state, data.frame(tax.ids=population.data$tax.ids, 
                                            self.employed=population.data$self.employed,
                                            c1=round(100*population.data$c1,0),
                                            c1.tilde=round(100*c1.tilde,0),
                                            per.audit.rate = per.audit.rate*100,
                                            per.penalty.rate = per.penalty.rate*100,
                                            perc.hideable.income = population.data$prop.hideable.income*100,
                                            Delta.Morale = round(100*Delta.Morale ,0),
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
  
  invisible(sim.data)
}