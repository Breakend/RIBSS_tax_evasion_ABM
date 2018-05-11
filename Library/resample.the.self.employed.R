resample.the.self.employed<- function(proportion.of.self.employed,self.employed,self.employed.propensity){
  
  N<- length(self.employed)
  old.proportion<- sum(self.employed)/N
  
  if(proportion.of.self.employed>old.proportion){
    new.se <-stoch.round((proportion.of.self.employed-old.proportion)*N)
    new.se <-sample((1:N)[!self.employed],size=new.se,prob=self.employed.propensity[!self.employed])
    self.employed[new.se] <- TRUE
  }
  if(proportion.of.self.employed<old.proportion){
    new.nse <-stoch.round((old.proportion-proportion.of.self.employed)*N)
    new.nse <-sample((1:N)[self.employed],size=new.nse,prob=1-self.employed.propensity[self.employed])
    self.employed[new.nse] <- FALSE
  }
  
  return(self.employed)
}