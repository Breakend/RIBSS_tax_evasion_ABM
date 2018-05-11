personal.pro.compliance<-function(V.past, Delta, s.discount){
  V.new<- s.discount*V.past+Delta
  return(V.new)
} 