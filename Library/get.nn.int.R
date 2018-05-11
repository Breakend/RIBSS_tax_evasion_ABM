get.nn.int.ineff<- function(tax.ids,nn,yearly.prop.interactions=0.1){
  
  nn.int<-sapply(tax.ids,FUN=function(i){
    l<- length(nn[[i]])
    if(l>0){
      return(nn[[i]][as.logical(rbinom(l, 1, yearly.prop.interactions))])
    }
    else{return(nn[[i]])}
  })
  
  return(nn.int)
}


get.nn.int<- function(tax.ids,nn,yearly.prop.interactions=0.1){
  
  nn.int<-sapply(tax.ids,FUN=function(i){
    nn.list <- unlist(nn[i])
    l<- length(nn.list)
    if(l>0){
      return(nn.list[as.logical(rbinom(l, 1, yearly.prop.interactions))])
    }
    else{return(nn.list)}
  })
  
  return(nn.int)
  
}