

get.total.tax.revenues <- function(state, pop.data, US.tp.N = 151*10^6, scale.factor=1) {
  
  US.expected.tax.revenue<- scale.factor*sum(pop.data$tax.rate*pop.data$income)*US.tp.N/nrow(pop.data)
  US.tax.revenue <- scale.factor*sum(pop.data$tax.rate*state$report.abs)*US.tp.N/nrow(state)
  
  return(list(US.expected.tax.revenue=US.expected.tax.revenue,
           US.tax.revenue=US.tax.revenue,
           tax.gap = (US.expected.tax.revenue-US.tax.revenue)/US.expected.tax.revenue))
}
