ALP.match.cor<- function(x,y,rho=0.5,verbose=F){
  
  x.old <- x
  df.tmp <- data.frame(x,y)
  
  cc <- (1:length(x))[complete.cases(df.tmp)]
  df.tmp <- df.tmp[cc,]
  
  x<- df.tmp$x
  y<- df.tmp$y
  
  if(rho>0.9) stop("ALP.ALP.match.cor can't handel such a large correlation")
  rho.root<-uniroot(ALP.match.cor.guess, rho=rho,x=x,y=y,return.diff=T, interval=c(0.0001,0.999))
  
  v<- ALP.match.cor.guess(rho.root$root,rho,x,y,return.vector=T)
  
  if(verbose){
    print(paste("Target cor = ",rho,". Outputed vector has cor = ",round(cor(x,v),4),sep=""))
  }
  
  df.tmp <- data.frame(x=x.old,y=x.old)
  df.tmp$y[cc] <- v
  return(df.tmp)
}
