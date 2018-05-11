get.per.audit.gamblers.fallacy<-function(q,propensity.prop.income.report,n,s.audit.discount, grad=0.07966, intercept= 0.15876){
  ### In the model the gamblers.fallacy effect called for year n but modifies and acts on
  ### percived audit rate for year n+1. Thus when called n+1 represents the number of years since beginning 
  ### to under-report. 
  multip.factor<-(exp(intercept +grad*log(1-propensity.prop.income.report))-1)*exp(-(1-s.audit.discount)*n)
  multip.factor<- max(0,multip.factor)
  q.new <- (multip.factor+1)*q
  return(q.new)
}
