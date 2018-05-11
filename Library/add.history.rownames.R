add.history.rownames <- function(x,label=NULL){
   
   l <- nrow(x)
   rownames(x) <- paste(label,seq((l-1),0,-1),
                                        "years ago",sep=" ")
   return(x)
 }