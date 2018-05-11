get.c1.tilde <- function(c1,c2,tax.rate,per.audit.rate,per.penalty.rate,m.qP,s.qP){

  c1<- pmin(c1,c2)
  
  deterrance.factor <- cdf.lognormal(x=per.audit.rate*per.penalty.rate, log(m.qP),1/s.qP)
  
  c1.tilde <- c1+(c2-c1)*deterrance.factor

  return(c1.tilde)
}