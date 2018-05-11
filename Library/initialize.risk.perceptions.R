#############################################################
###        Initial Risk Perceptions     
#############################################################
initialize.risk.perceptions <- function(population.data, config, perceps = NULL) {
  N <- nrow(population.data)
  
  audit.rate <- get.config.param(config, 'audit.rate')
  penalty.rate <- get.config.param(config, 'penalty.rate')
  #per.prop.of.penalty.rate <- get.config.param(config, 'per.prop.of.penalty.rate')
  population.data[, 'c2'] <- get.config.param(config, 'c2')
  population.data[, 'per.audit.rate.0'] <- pmin(population.data$per.audit.rate,0.8) ### RV
  population.data[, 'per.penalty.rate.0'] <- population.data$per.penalty.rate ### RV
  population.data[, 'transformed.per.audit.rate.0'] <- with(population.data, 
                                                            (per.audit.rate.0)/(1-per.audit.rate.0))
  if(is.null(perceps))
  {
    population.data[, 'transformed.per.audit.rate'] <- with(population.data, 
                                                            (per.audit.rate)/(1-per.audit.rate))
  } else {
    population.data[, 'transformed.per.audit.rate'] <- with(perceps, 
                                                            (per.audit.rate)/(1-per.audit.rate))
  }

  
  return(population.data)
}
