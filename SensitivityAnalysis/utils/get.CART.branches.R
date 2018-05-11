get.CART.branches <- function(fit, include.leaves=FALSE, rm.root.string=TRUE){
   
   require(rpart)
   R<- list()
   k<-1

   node.label <- rownames(fit$frame)
   for(i in 1:nrow(fit$frame)){
     plot.split<- fit$frame$var[i]
     if(!(plot.split%in%"<leaf>")){
       j<- as.numeric(node.label[i])
       tmp<- path.rpart(fit,j)[[1]]
       if(rm.root.string){
         R[[k]]<-tmp[-1]
         R[[k]] <- c(tmp[-1], as.character(plot.split))
       }else{
         R[[k]]<-tmp
       }
       k<-k+1
     }
   }
   
   return(R)
   
 }