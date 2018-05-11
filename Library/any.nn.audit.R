any.nn.audit<- function(a,tax.ids,nn.int){
  any.a.list<-sapply(tax.ids,FUN=function(i){
    return(any(a[nn.int[[i]] ]))
  })
  return(any.a.list)
}