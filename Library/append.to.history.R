append.to.history<- function(x.history,x.present,K,label=NULL){
   
  K<- K+1
   x.history <- rbind(x.history ,x.present)
   if(nrow(x.history)>K){
     l<- nrow(x.history)
     l<-((l-K+1):l)
     x.history<-x.history[l,]
   }
   
   l <- nrow(x.history)
   rownames(x.history) <- paste(label,seq((l-1),0,-1),
                        "years ago",sep=" ")
   
   return(x.history)
 }