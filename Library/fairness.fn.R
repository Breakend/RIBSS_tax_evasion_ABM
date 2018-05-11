fairness.fn <- function(c1.tilde,c1,c2,tax.rate){
  
  R<- (c2-tax.rate)/(c2-c1.tilde)
  R[tax.rate<=c1.tilde]<-1
  
  ids <- which(tax.rate>c2)
  if(length(ids) > 0) {R[ids]<-0}
  
  return(R)
}