
get.baseline.recovered.tax.revenues.and.penalties <- function(state, N = 151*10^6, scale = 1){
  val <- scale * sum(state$penalty.and.past.tax.to.pay, na.rm = T)*N/(nrow(state))
  return(val)
}


                              