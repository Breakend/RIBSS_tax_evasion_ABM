generate.media.feedback <- function(beta.morale.media, tax.gap, 
                                    media.mid.effect, media.steepness, 
                                    media.stochastic.offset=0){
  
  media.attention.to.tax.evasion <- cdf.lognormal(tax.gap, 
                                        log(media.mid.effect),
                                        1/media.steepness)
  
  if(media.stochastic.offset>0){
    mm <- media.stochastic.offset+
      (1-media.stochastic.offset)*media.attention.to.tax.evasion
    media.attention.to.tax.evasion <- rpert(1, x.min=0, x.max=mm, x.mode=NULL,
                                            mu=media.attention.to.tax.evasion)
      #runif(1,min=media.attention.to.tax.evasion,max=mm)
  }
  
  public.response.to.media.attention <- beta.morale.media*tax.gap*media.attention.to.tax.evasion
  
  return(public.response.to.media.attention)
}