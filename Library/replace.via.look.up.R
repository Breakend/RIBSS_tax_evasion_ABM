replace.via.look.up<- function(x,look.up, rows=c(1,2)){
  
  ## replace enties in x that corespond with look.up[,1] with look.up[,2]
  ## if different rows to use - specify these by rows
  
  positions <- match(x,look.up[,rows[1]])
  replacement<-look.up[positions,rows[2]]
  replacement[is.na(replacement)]<- x[is.na(replacement)]
  
  return(replacement)  
}