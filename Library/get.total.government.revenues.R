
get.total.government.revenues <- function(tax.revenues,total.penalties,other.government.revenues){
  Q<- tax.revenues+total.penalties+other.government.revenues
  return(Q)
}