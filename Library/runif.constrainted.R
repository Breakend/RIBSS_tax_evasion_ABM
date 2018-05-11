runif.constrainted<-function(x){
  if(x==0) {
    R<-0
  }else{
    if(x<0.5) stop("out of bound")
    R<-runif(1,1-2*(1-x),1)
  }
  return(R)
}

