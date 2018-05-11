ALP.fn.get.proportion.of.alters<-function(x,levels,useNA ="ifany"){
  l <- length(levels)
  boh<- rep(0,l)
  names(boh)<-1:l
  tmp <- table(as.numeric(x),useNA =useNA)
  boh[names(tmp)]<-tmp
  names(boh) <- levels
  return(boh/sum(boh))
}