get.network.interactions<- function(initial.perception, tax.ids, beta,nn.int,prop.of.those.int,method="mean"){

  if(method=="mean"){
    final.network.influenced.perception<-sapply(tax.ids,FUN=function(i){
      nn.list <- nn.int[[i]]
      beta.network <- beta*prop.of.those.int[i] 
      neighbours.influence <- 0
      if(prop.of.those.int[i]>0){
        neighbours.influence <- mean(initial.perception[ nn.list ])
      }
      R <- c(beta.network=beta.network,unweighted.neighbours.influence=neighbours.influence)
      return(R)
    })
    final.network.influenced.perception<-as.data.frame(t(final.network.influenced.perception))
  }
  if(method=="max"){
    final.network.influenced.perception<-sapply(tax.ids,FUN=function(i){
      nn.list <- nn.int[[i]]
      N.nn <- length(nn.list)
      pensonal.influence <- initial.perception[i]
      neighbours.influence <- 0
      
      if(prop.of.those.int[i]>0){
        ### this assumes that if you interact with nn - you preferentially interact with
        ### the ones that have the highest  initial.perception value. 
        prob.affect.by.at.least.one.nn <- 1-beta^N.nn
        rv.logical <- as.logical(rbinom(1, 1, prob.affect.by.at.least.one.nn ) )
        if(rv.logical){
          neighbours.influence <- max(initial.perception[ nn.list ])
        }
      }
      R <- (ego.alters.influence=max(c(pensonal.influence,neighbours.influence)))
      return(R)
    })
  }
  return(final.network.influenced.perception)
}