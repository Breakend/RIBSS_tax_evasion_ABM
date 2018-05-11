get.refund.return<- function(refund.return, prob.refund.to.additional=0, prob.additional.to.refund=0){
  
    refund.to.additional<-as.logical(sapply(refund.return, FUN=function(x,prob=prob.refund.to.additional){
      if(x){
        return( rbinom(1, 1, prob = prob))
      }else{
        return(0)
      }
    }))
    
    additional.to.refund<-as.logical(sapply(!refund.return, FUN=function(x,prob=prob.additional.to.refund){
      if(x){
        return( rbinom(1, 1, prob = prob))
      }else{
        return(0)
      }
    }))
    
    refund.to.additional <- refund.return&refund.to.additional
    additional.to.refund <-  (!refund.return)&additional.to.refund
    
    refund.return[additional.to.refund] <- T
    refund.return[refund.to.additional] <- F
    
    return(refund.return)
}