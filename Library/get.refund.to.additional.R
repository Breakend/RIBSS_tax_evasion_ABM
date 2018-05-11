get.refund.to.additional <- function(refund.return,refund.group,prob.refund.to.additional){
  
  ll <- max(refund.group)
  refund.to.additional<- refund.return
  for(ii in 1:ll){
    in.this.group <- refund.group%in%ii
    tmp <- refund.to.additional & in.this.group
    refund.to.additional[tmp]<-  rbinom(sum(tmp), 1, prob = prob.refund.to.additional[ii])
  }
  
  return(as.logical(refund.to.additional)) 
}