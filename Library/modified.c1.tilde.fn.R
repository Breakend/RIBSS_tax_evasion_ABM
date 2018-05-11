modified.c1.tilde.fn <- function(c1,c2,tax.rate,per.audit.rate,per.penalty.rate,mu,nu,avg.nu,tax.complexity){
  
  if(tax.rate>c2) return(0)
  
  c1.mod<- c1*(1-tax.complexity)*max(1-nu/avg.nu,0) 
  
  audit.penalty.factor <- cdf.lognormal(x=per.audit.rate*per.penalty.rate, log(mu),1)
  
  c1.tilde <- c1.mod+(c2-c1.mod)*audit.penalty.factor 
  
  return(c1.tilde)
}