get.per.audit.gamblers.fallacy<-function(x,n,l=2){
  ### In the model the gamblers.fallacy effect called for year n but modifies and acts on
  ### percived audit rate for year n+1. Thus when called n+1 represents the number of years since beginning 
  ### to under-report. 
  m<-sapply(n,FUN=function(z) if(z>l){return(max(1,2*l-z+1))}else{return(z+1)})
  return(  ((1-(1-x)^m))  )
}
