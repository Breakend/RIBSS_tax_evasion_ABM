compute.tax.gap <- function(pop.data, state.at.time.t) {
  expected.tax.revenue<- sum(pop.data$tax.rate*pop.data$income)
  tax.revenue <- sum(pop.data$tax.rate*state.at.time.t$report.abs)
  tax.gap.amount <- expected.tax.revenue - tax.revenue
  tax.gap <- tax.gap.amount/expected.tax.revenue
  
  return(tax.gap)
}