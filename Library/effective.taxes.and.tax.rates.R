
effective.taxes.and.tax.rates <- function(income.info, tax.schedule) {
  
  income.info$Income <- income.info$income
  income.info$Income[income.info$Income==0]<-1
  tax.rate.income.bracket <- tax.schedule
  names(tax.rate.income.bracket) <- c('filing.status', 'min', 'max', 'tax.rate')
  tax.rate.income.bracket <- split(tax.rate.income.bracket,tax.rate.income.bracket$filing.status)
  
  eff.income.tax <- 
    apply(income.info,1,get.effective.tax.rate,tax.rate.income.bracket=tax.rate.income.bracket)
  
  ind.income.taxes <- ifelse(eff.income.tax < 1, 1, eff.income.tax) #Lower bound to 1 dollar 
  
  ind.tax.rates <- ind.income.taxes/income.info$Income
  
  overall.tax.rate <- sum(ind.income.taxes)/sum(income.info$Income)
  
  return(list(ind.income.taxes = ind.income.taxes, 
              ind.tax.rates = ind.tax.rates, 
              overall.tax.rate = overall.tax.rate))
}