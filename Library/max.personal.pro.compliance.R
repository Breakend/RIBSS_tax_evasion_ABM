max.personal.pro.compliance<-function(V.max, s.discount){
  V.max<- s.discount*V.max+1
  return(V.max)
}