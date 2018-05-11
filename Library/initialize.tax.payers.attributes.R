
initialize.tax.payers.attributes <- function(config, attribs = NULL)
{
  N <- get.config.param(config, 'population.size')
  tax.ids <- 1:N
  c2<- get.config.param(config, 'c2')
  compliant.t0 <- rep(TRUE,N)
  
  if(is.null(attribs))
  {
    c1<- rbeta(N, 0.85, 1.1, ncp = 0)*c2 ### median of the distribution is 0.3
    c1<- sort(c1)
    
    compliant<- compliant.t0
    compliant.history<- compliant.t0
    
    audited.non.compliant <- rep(FALSE,N)
    penalized <- rep(FALSE,N)
    
    V.new<- rep(0,N)
    V.max <- rep(max(V.new),N)
    
    Delta.Per <- rep(1,N)
    propensity.prop.income.report<- rep(1,N)
    
    refund.return <- rep(TRUE,N)
    
    Q.new<- rep(0,N)
    #Q.max <- max(Q.new)
    
    P.new<- rep(0,N)
    #P.max <- max(P.new)
    
    income<- rep(100,N)
    under.reporting.history<- rep(0,N)
    penalty.history<- rep(FALSE,N)
    audit.history<- rep(FALSE,N)
    
    years.since.last.compliant <- rep(0,N)
    years.since.last.audit <- rep(0,N)
    years.since.last.penalty <- rep(0,N)
    
    freq.audits <- rep(0,N)
    freq.penalty <- rep(0,N)
  
    tax.payers.attribs <- data.frame(tax.ids, c1, compliant.t0, compliant, compliant.history, audited.non.compliant, penalized, V.new, 
                              V.max, Delta.Per, propensity.prop.income.report, refund.return, Q.new, P.new, income, 
                              under.reporting.history, penalty.history, audit.history, years.since.last.compliant, 
                              years.since.last.audit, years.since.last.penalty, freq.audits, freq.penalty)
  #tax.payers.attribs <- as.data.frame(tax.payers.attribs)
  } else {
    tax.payers.attribs <- data.frame(tax.ids, c1=attribs$c1, compliant.t0, compliant = attribs$compliant, 
                                     compliant.history = attribs$compliant.history[, "compliant.0.years.ago"], 
                                     audited.non.compliant = attribs$audited.non.compliant, 
                                     penalized = attribs$penalized, V.new = attribs$V.new, V.max = rep(max(attribs$V.new),N), 
                                     Delta.Per=attribs$Delta.Per, propensity.prop.income.report=attribs$propensity.prop.income.report, 
                                     refund.return=attribs$refund.return, Q.new=attribs$Q.new, P.new=attribs$P.new, income=attribs$income, 
                                     under.reporting.history=attribs$under.reporting.history[, "percent.reported.0.years.ago"], 
                                     penalty.history=attribs$penalty.history[, "penalty.0.years.ago"], 
                                     audit.history=attribs$audit.history[, "audited.0.years.ago"], years.since.last.compliant=attribs$years.since.last.compliant, 
                                     years.since.last.audit=attribs$years.since.last.audit, years.since.last.penalty=attribs$years.since.last.penalty, 
                                     freq.audits=attribs$freq.audits, freq.penalty=attribs$freq.penalty)
  }
  
  return(tax.payers.attribs)
}