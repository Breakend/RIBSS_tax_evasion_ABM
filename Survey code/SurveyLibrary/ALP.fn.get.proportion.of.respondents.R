ALP.fn.get.proportion.of.respondents<-function(x,levels){
  l <- length(levels)
  boh<- rep(0,l)
  names(boh)<-1:l
  tmp <- table(as.numeric(x),useNA ="ifany")
  boh[names(tmp)]<-tmp
  if(any(is.na(x))){names(boh) <- c(levels,"NA")}else{names(boh) <-levels}
  return(boh/sum(boh))
}