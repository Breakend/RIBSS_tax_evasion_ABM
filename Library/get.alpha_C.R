

get.alpha_C <- function(US.tp.N, baseline.audit.costs,baseline.total.governmet.expenses=3.863052*10^2 ){
  alpha_C<- (baseline.total.governmet.expenses-baseline.audit.costs)/US.tp.N
  return(alpha_C)
}