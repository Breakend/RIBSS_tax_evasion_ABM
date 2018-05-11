get.interacting.proportion.of.nn<- function(tax.ids,nn,nn.int){
  proportion<-sapply(tax.ids,FUN=function(i){
    l<- length(nn[[i]])
    if(l>0){
      return(length(nn.int[[i]])/length(nn[[i]]))
    }
    else{return(0)}
  })
  return(proportion)
}