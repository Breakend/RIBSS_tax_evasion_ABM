do.network.interactions<- function(initial.perception, beta,nn.int,prop.of.those.int,method="mean"){
  
  if(method=="mean"){
    final.network.influenced.perception<-sapply(tax.ids,FUN=function(i){
        nn.list <- nn.int[[i]]
        beta.ind <- beta*prop.of.those.int[i] 
        pensonal.influence <- (1-beta.ind)*initial.perception[i]
        neighbours.influence <- 0
        
        if(prop.of.those.int[i]>0){
          neighbours.influence <- beta.ind*mean(initial.perception[ nn.list ])
        }
        return(pensonal.influence+neighbours.influence)
    })
  }
  if(method=="max"){
    final.network.influenced.perception<-sapply(tax.ids,FUN=function(i){
      nn.list <- nn.int[[i]]
      N.nn <- length(nn.list)
      pensonal.influence <- initial.perception[i]
      neighbours.influence <- 0
      
      if(prop.of.those.int[i]>0){
        prob.affect.by.at.least.one.nn <- 1-beta^N.nn
        rv.logical <- as.logical(rbinom(1, 1, prob.affect.by.at.least.one.nn ) )
        if(rv.logical){
        neighbours.influence <- max(initial.perception[ nn.list ])
        }
      }
      return(max(c(pensonal.influence,neighbours.influence)))
    })
  }
  return(final.network.influenced.perception)
}