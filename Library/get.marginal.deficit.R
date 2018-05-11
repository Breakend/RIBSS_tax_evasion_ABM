
get.marginal.deficit <- function(total.government.revenues,get.total.governmet.expenses,initial.deficit=590*10^9){
  Delta.Debt <- (get.total.governmet.expenses-total.government.revenues)-initial.deficit
  return(Delta.Debt)
}