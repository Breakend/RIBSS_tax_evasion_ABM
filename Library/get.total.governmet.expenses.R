
get.total.governmet.expenses <- function(N,audit.costs,alpha_C=25583){
  C <- alpha_C*N+audit.costs
  return(C)
}