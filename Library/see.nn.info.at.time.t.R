see.nn.info.at.time.t <- function(x,i,nn,t){
  
  out.list<- NULL
  out.list[[1]]<- x[[i]][t,]
  l.nn <- length(nn[[i]])
  if(l.nn>0){
  for(k in 1:l.nn){
    j <- nn[[i]][k]
    out.list[[k+1]] <- x[[j]][t,]
  }
  }
  out <- do.call("rbind",out.list)
  rownames(out) <- c("ego",paste("alter",1:length(nn[[i]]),sep=""))
  return(out)
}