update.percieved.audit.rate <- function(per.audit.rate.0,audit,audit.history,s.discount.seq.default){
  
  
  ### this is correct as long as K >>1
  s.discount.seq <- rev(s.discount.seq.default[1:nrow(audit.history)])
  Delta <-(s.discount.seq%*%audit.history)/sum(s.discount.seq)
  
  Delta[audit] <- 0 
  
  ### need to transform the per.audit.rate such that 
  ### with no audit the function we apply will preserve 
  ### the percieved audit rate value
  tmp<-per.audit.rate.0/(1-per.audit.rate.0) 
  
  
  per.audit.rate <- (tmp+Delta)/(tmp+1) 
  
  
  return(per.audit.rate)
}