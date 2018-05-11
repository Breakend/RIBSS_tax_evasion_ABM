initialize.risk.perceptions <- function(config, perceps = NULL)
{
  N <- get.config.param(config, 'population.size')
  tax.ids <- 1:N
  
  audit.rate <- get.config.param(config, 'audit.rate')
  penalty.rate <- get.config.param(config, 'penalty.rate')
  #per.prop.of.penalty.rate <- get.config.param(config, 'per.prop.of.penalty.rate')
  
  per.audit.rate.0 <-  pmin(pop.data$per.audit.rate,0.8) ### RV
  per.penalty.rate.0 <- pmin(pop.data$per.penalty.rate,0.8) ### RV rep(penalty.rate*per.prop.of.penalty.rate,N)
  
  transformed.per.audit.rate.0 <- per.audit.rate.0/(1-per.audit.rate.0)
  transformed.per.penalty.rate.0 <- per.penalty.rate.0/(1-per.penalty.rate.0)
  
  if(is.null(perceps))
  {
    per.audit.rate<- per.audit.rate.0
    per.penalty.rate <- per.penalty.rate.0
  } else {
    per.audit.rate<- perceps$per.audit.rate
    per.penalty.rate <- perceps$per.penalty.rate
  }
  
  risk.perceptions <- data.frame(tax.ids, per.audit.rate.0, per.audit.rate, transformed.per.audit.rate.0, 
                                 per.penalty.rate.0, per.penalty.rate, transformed.per.penalty.rate.0)
  
  #risk.perceptions <- as.data.frame(risk.perceptions)
  
  return (risk.perceptions)
}