get.media.tax.morale.effect <- function(tax.gap, 
                                    media.mid.effect, media.steepness, 
                                    media.stochastic.offset=0){
  
  ### generate the tax morale due to media 
  
  media.attention.to.tax.evasion <- cdf.lognormal(tax.gap, 
                                        log(media.mid.effect),
                                        1/media.steepness)
  
  if(media.stochastic.offset>0 & runif(1)<media.stochastic.offset){
    mm <- media.stochastic.offset+
      (1-media.stochastic.offset)*media.attention.to.tax.evasion
    media.attention.to.tax.evasion <- rpert(1, x.min=0, x.max=mm, x.mode=NULL,
                                            mu=media.attention.to.tax.evasion)
      #runif(1,min=media.attention.to.tax.evasion,max=mm)
  }
  
  compliance.morale.due.to.media.attention <- 1-media.attention.to.tax.evasion
  
  return(compliance.morale.due.to.media.attention)
}