#Function to initialize track dynamics

initialize.track.dynamics <- function(pop.data) {
  
    #Set track dynamics year 1 to defaults
    N <- nrow(pop.data)
    defaults <- data.frame(t=rep(1,N),
                           tax.ids=pop.data$tax.ids,
                           per.audit.rate=round(pop.data$per.audit.rate,2),
                           per.penalty.rate=round(pop.data$per.penalty.rate,2),
                           report.abs=pop.data$income,
                           report=rep(100,N),
                           hideable.reported=rep(100,N),
                           c1=pop.data$c1,
                           perc.hideable.income=100*pop.data$prop.hideable.income,
                           self.employed=pop.data$self.employed,
                           transformed.per.audit.rate=pop.data$transformed.per.audit.rate,
                           stringsAsFactors = FALSE)
    
    defaults[, c('c1.tilde',
                 'Delta.Morale',
                 'Delta.Network',
                 'Delta.Q',
                 'w', 'w.Q', 'w.non',
                 'penalty.and.past.tax.to.pay',
                 'past.tax.to.pay',
                 'audit.type')] <- rep(NA,N)
    
    defaults[, c('compliant.t0',
                 'compliant',
                 'refund.return')] <- rep(TRUE,N)
    
    #All attributes that are to be set to FALSE
    defaults[, c( 'audited', 
                  'penalized')] <- rep(FALSE,N)

    #All attributes that are to be set to 0
    defaults[, c( 'V.new',
                  'Q.new',
                  'P.new',
                  'years.since.last.compliant',
                  'years.since.last.audit',
                  'years.since.last.penalty',
                  'freq.audits',
                  'freq.penalty')] <- rep(0,N)
    
    defaults[, 'V.max'] <- rep(0,N)
    
    #All attributes that are to be set to 1
    defaults[, c('Delta.Per',
                 'propensity.prop.income.report')] <- rep(1,N)
                           
  
  return(defaults)
}