find.closest.entry <- function(x,value){
  if(length(value)==1){
    R <- which(abs(x-value)==min(abs(x-value)))
    
    if (length(R)>1){
      R<- R[1]
    }
  }
  
  if(length(value)>1){
    R<-sapply(value,FUN=function(y){
      R <- which(abs(x-y)==min(abs(x-y)))
      if (length(R)>1){
        R<- R[1]
      }
      return(R)
    })
  }
  return(R)
  
}