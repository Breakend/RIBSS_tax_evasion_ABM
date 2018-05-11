get.last.number.of.year<-function(x,a,t.window){
  if(length(x)<t.window){
    x<-c(x,a)
  }else{
    x<-c(x[-1],a)
  }
  return(x)
}

