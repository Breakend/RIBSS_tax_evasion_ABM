#Returns average over all the realizations

average.over.realizations<-function(xdat,input.fields,output.fields){
  
  x<- xdat[1,input.fields]
  y <- apply(xdat[,output.fields],2,mean,na.rm = T)
  
  return(unlist(c(x,y)))
}