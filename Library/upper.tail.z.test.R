upper.tail.z.test<-function(xbar,x.seq,alpha=0.95){
  R<- FALSE
  
  if(!is.null(x.seq) & length(x.seq)>1){
    z <- (xbar-mean(x.seq))/( sd(x.seq)/ sqrt(length(xbar))) 
    
    z.alpha <- qnorm(alpha) 
    if(!is.nan(z)){if(z>z.alpha){
      R<- TRUE ## if alpha=0.95 and R=TRUE => xbar is larger than mu0 at the 95% confidence
    }}
  }
  return(R)
}

### Verification 
# a<- rbinom(100,1*N,0.015)
# b<- rbinom(100,1*N,0.008)
# c<-sapply(a,FUN=function(x){upper.tail.z.test(x,x.seq=b,alpha=0.999)})
# table(c)