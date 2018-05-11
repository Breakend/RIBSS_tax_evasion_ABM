lower.tail.z.test<-function(xbar,x.seq,alpha=0.05){
  R<- FALSE
  
  if(!is.null(x.seq) & length(x.seq)>1){
    z <- (xbar-mean(x.seq))/( sd(x.seq)/ sqrt(length(xbar))) 
    
    z.alpha <- qnorm(alpha) 
    if(!is.nan(z)){if(z<z.alpha){
      R<- TRUE ## if alpha=0.05 and R=TRUE => xbar is laower than mu0 at the 95% confidence
    }}
  }
  return(R)
}