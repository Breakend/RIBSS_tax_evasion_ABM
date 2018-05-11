ALP.get.rearranged.sampled.dist<- function(sampled.dist,predicted.reg){
  a<-order(predicted.reg, decreasing = T)
  b<-order(sampled.dist, decreasing = T)
  
  tmp <- sampled.dist
  tmp[a] <- tmp[b]
  
  rearranged.sampled.dist<-tmp
  
  return(rearranged.sampled.dist)
}