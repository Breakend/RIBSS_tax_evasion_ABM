test.monotonic.increasing.sequence<- function(x){
  x<- as.numeric(x)
  return(all(x == cummax(x)))
}