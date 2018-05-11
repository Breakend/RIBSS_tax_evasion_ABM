test.monotonic.decreasing.sequence<- function(x){
  x<- as.numeric(x)
  return(all(x == cummin(x)))
}