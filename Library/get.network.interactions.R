get.network.interactions<- function(initial.perception, tax.ids, beta,nn.int,method="mean"){


  final.network.influenced.perception<-sapply(tax.ids,FUN=function(i){
    nn.list <- nn.int[[i]]
    if(length(nn.list)>0){
      beta.network <- beta# *prop.of.those.int[i] 
      if(method%in%"mean"){
        neighbours.influence <- mean(initial.perception[ nn.list ])
      }
      if(method%in%"max"){
        neighbours.influence <- max(initial.perception[ nn.list ])
      }
      if(method%in%"min"){
        neighbours.influence <- min(initial.perception[ nn.list ])
      }
    }else{
      beta.network<- 0
      neighbours.influence <- 0
    }
    R <- c(beta.network=beta.network,unweighted.neighbours.influence=neighbours.influence)
    return(R)
  })
  final.network.influenced.perception<-as.data.frame(t(final.network.influenced.perception))
  
  return(final.network.influenced.perception)
}