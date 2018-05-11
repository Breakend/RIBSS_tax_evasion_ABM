correct.past.penalties<- function(penalty.history){
  R<- penalty.history
  if(is.matrix(R)){
    z <- R[,ncol(R)]
    for(j in 1:ncol(R)){
      R[,j] <- z
    }
  }
  return(R)
}