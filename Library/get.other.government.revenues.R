
get.other.government.revenues <- function(US.tp.N,
                                          baseline.total_tax_revenues, 
                                          alpha_R= 21701.56, 
                                          baseline.IRS.penalties.revenues){
  Z<- alpha_R*US.tp.N-baseline.total_tax_revenues-baseline.IRS.penalties.revenues
  return(Z)
}