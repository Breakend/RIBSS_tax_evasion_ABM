check.n.modify.baseline.perceptions<- function(qq.i,qq.i0,s,q,q.base=0.008){
  
  ### This function was written to exponentially reduce the base line perceived audit
  ### rate when the audit rate goes below the baseline qudit rate where the ALP survey data was 
  ### gathered. This makes sure that if the audit rate goes to say 0.1% instead of 0.8% the
  ### baseline percieved audit risks will slowly approach values 1/8 of their original baseline.
  ### if however the audit rate is brough back to values above 0.8% then the base line perceived audit 
  ### rate increases back to at most it's orginial value. 
  
  tmp1<- qq.i0*(q/q.base)
  tmp2 <- qq.i-tmp1
  qq.i <- tmp1+ tmp2*(exp(-(1-s)))
  qq.i <- pmin(qq.i, qq.i0)
  
  
  return(qq.i)
}