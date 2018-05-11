initialize.discount.rates <- function(config)
{
  morale.half.life <- get.config.param(config, 'morale.half.life')
  K <- get.config.param(config, 'K')
  
  s.discount <- exp(-log(2)/morale.half.life) ## half life is ~2 years
  s.discount.seq.default <- s.discount^seq(0, K, by=1)
  s.audit.discount <- s.discount
  
  generation.half.life <- get.config.param(config, 'generation.half.life')
  s.generation.discount <- exp(-log(2)/generation.half.life)  # half life is 40 years.
  s.penalty.discount <- s.generation.discount
  
  disc.rates <- data.frame(s.discount, s.discount.seq.default, 
                           s.audit.discount, s.generation.discount, s.penalty.discount,generation.half.life=generation.half.life)
  
  return (disc.rates)
}