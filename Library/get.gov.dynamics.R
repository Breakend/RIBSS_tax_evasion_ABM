get.gov.dynamics <- function(pop.data, state.at.time.t, tab.audit.costs, 
                             perc.non.tax.rev = NULL, ids.by.audit.type = NULL,
                             alpha_C=25583, US.other.government.revenues = NULL, 
                             US.fixed.gov.expenses = NULL, #11923*324*10^6
                             US.Debt = 19.59*10^12,
                             US.marginal.Debt = 0,
                             scale.factor = 1, US.tp.N = 151*10^6,
                             audit.cost.scale=0.6257) {
  
  
  N <- dim(pop.data)[1] #Population Size
 
  retval <- get.total.tax.revenues(state.at.time.t, pop.data, scale.factor = scale.factor)
  
  US.expected.tax.revenue <- retval$US.expected.tax.revenue
  US.tax.revenue <- retval$US.tax.revenue
  US.tax.gap <- retval$tax.gap
  
  #Scaling up recovered revenues and penalties
  US.penalty.and.past.tax.to.pay <- sum(state.at.time.t$penalty.and.past.tax.to.pay, na.rm = T)*US.tp.N/N
  US.past.taxes.to.pay <- sum(state.at.time.t$past.tax.to.pay, na.rm = T)*US.tp.N/N
    
  ### Find Other government revenues - these are fixed at the status quo value.
  if(is.null(US.other.government.revenues)) {
    US.other.government.revenues<-get.other.government.revenues(US.tp.N=US.tp.N,
                          US.tax.revenue,
                          baseline.IRS.penalties.revenues=US.penalty.and.past.tax.to.pay)
  }
  
  ### sanity check the ratio below should be ~54%
  perc.non.tax.rev <- 100*US.other.government.revenues/(US.other.government.revenues+US.tax.revenue)
  
  ### Only at status quo should  US.government.revenues= US.other.government.revenues+US.tax.revenue
  US.government.revenues <- get.total.government.revenues(US.tax.revenue,US.penalty.and.past.tax.to.pay,US.other.government.revenues)
  
  if(is.null(ids.by.audit.type)){
    c.audit <- which(state.at.time.t$audit.type == 'correspondence')
    f.audit <- which(state.at.time.t$audit.type == 'field')
    ids.by.audit.type <- list(ids.correpondence.audits = c.audit, 
                              ids.field.audits = f.audit)
  }
  
  num.of.audits <-  length(ids.by.audit.type$ids.correpondence.audits) + length(ids.by.audit.type$ids.field.audits) 
  if(num.of.audits == 0) {
    audit.costs.info <- c(total=0, mean=0)
  } else {
    audit.costs.info<-aggrgate.audit.costs(pop.data,ids.by.audit.type,tab.audit.costs)
  }

  US.audit.cost <- audit.cost.scale*audit.costs.info["mean"]*US.tp.N*num.of.audits/N
  US.fixed.gov.expenses  <- alpha_C*US.tp.N
  US.government.expenses <- US.fixed.gov.expenses+US.audit.cost
  
  
  ### sanity check the deficit should be around $590 Billion.
  US.deficit <- (US.government.expenses-US.government.revenues)
  US.Debt <- US.Debt+US.deficit
  
  US.marginal.deficit <- get.marginal.deficit(US.government.revenues,US.government.expenses,initial.deficit=586.116440*10^9)
  US.marginal.Debt <- US.marginal.Debt+US.marginal.deficit
  
  ### Need to be completed!!
  # debt.and.gdp <- get.Debt.and.GDP.with.growth(year=1,marginal.deficit=0,previous.year.Debt= 19.59*10^12,
  #                     initial.deficit=590*10^9,initial.interest.payments=241*10^9,initial.GDP = 18.46*10^12,
  #                         GDP.growth.rate = 1.0241, Debt.interest.rate = (1+241*10^9/(19.59*10^12)))
  
  retval <- data.frame(total.revenue = US.government.revenues,
                       US.expected.tax.revenue=US.expected.tax.revenue,
                       scale.factor=scale.factor,
                       IT.revenue = US.tax.revenue,
                       IT.revenue.per.taxpayer = US.tax.revenue/(US.tp.N),
                       perc.non.tax.revenues = perc.non.tax.rev,
                       US.penalty.and.past.tax.to.pay = US.penalty.and.past.tax.to.pay,
                       US.past.taxes.to.pay = US.past.taxes.to.pay,
                       US.tax.gap = 100*(US.expected.tax.revenue-US.tax.revenue )/US.expected.tax.revenue, 
                       US.government.expenses = US.government.expenses,
                       US.fixed.gov.expenses =  US.government.expenses - US.audit.cost,
                       audit.costs = US.audit.cost,
                       US.Debt = US.Debt,
                       US.deficit = (US.government.expenses-US.government.revenues),
                       US.marginal.Debt =  US.marginal.Debt,
                       US.marginal.deficit = US.marginal.deficit)
  
  return(retval)
}