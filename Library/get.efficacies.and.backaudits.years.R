get.efficacies.and.backaudits.years<- function(rr,state.at.time.t,backaudits,eff=c(corresp=0.9,field=0.7)){
  corresp<-intersect(rr$ids.correpondence.audits,state.at.time.t$tax.ids[(!state.at.time.t$compliant)])
  field<-intersect(rr$ids.field.audits,state.at.time.t$tax.ids[!state.at.time.t$compliant])
  
  eff.list <- c(rep(eff["corresp"],length(corresp)),
                rep(eff["field"],length(field)))[order(c(corresp,field))]
  
  if(length(eff.list) > 0) {
    eff.list <-round(sapply(eff.list,runif.constrainted),2)
  } else {
    eff.list <- NULL
  }
  K.list <- c(rep(backaudits["corresp"],length(corresp)),
              rep(backaudits["field"],length(field)))[order(c(corresp,field))]
  
  ### Generate a random number of back audits from a poisson with min 1 and max K+1
  K.field <- sapply(K.list[K.list>1], FUN=function(x){pmin(rpois(1,1)+1,x)})
  K.list[K.list>1] <- K.field
  
  R<- list(eff.list=eff.list,K.list=K.list)
  
  return(R)
}





