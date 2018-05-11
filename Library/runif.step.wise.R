runif.step.wise<-function(x){
  
  a<-runif(1,0,x)
  b<-runif(1,x,1)
  c<-stoch.round(x)
  
  R<-c*b+(1-c)*a
  
  return(R)
}





