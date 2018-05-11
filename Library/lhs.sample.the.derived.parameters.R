lhs.sample.the.derived.parameters<-function(x,cases){
  inv.direction<- as.logical(x["inv.direction"])
  Derived.from <- as.character(x["Derived.from"])
  denom1<-model.parameters[Derived.from,3]-model.parameters[Derived.from,2]
  denom2<- x[,3]-x[,2]
  value <- cases[,Derived.from]
  
  if(inv.direction){
    value.inv <- model.parameters[Derived.from,3]-value
    R<-denom2*(value.inv-model.parameters[Derived.from,2])/denom1+x[,2]
  }else{
    R<- denom2*(value-model.parameters[Derived.from,2])/denom1+x[,2]
  }
  R<- as.data.frame(R)
  names(R)<- row.names(x)
  
  return(R)
}
