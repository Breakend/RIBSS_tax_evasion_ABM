get.additional.to.refund <- function(refund.return,refund.group,prob.additional.to.refund){
  
  ll <- max(refund.group)
  additional.to.refund <- !refund.return
  for(ii in 1:ll){
    in.this.group <- refund.group%in%ii
    tmp <- additional.to.refund & in.this.group
    additional.to.refund[tmp]<-  rbinom(sum(tmp), 1, prob = prob.additional.to.refund[ii])
  }

  return(as.logical(additional.to.refund)) 
}