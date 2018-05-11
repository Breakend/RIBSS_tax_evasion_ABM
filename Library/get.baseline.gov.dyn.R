#### Important this part should only be run after the system has reached a steady state. 

get.baseline.gov.dyn <- function(pop.data, state.at.time.t, aggregated.dyn, 
                                 tab.audit.costs, generation.half.life,
                                 baseline.rev = 1.4470 * 10^12,
                                 US.Debt = 19.59*10^12,
                                 US.marginal.Debt = 0) {
  
  #Calibrate Government Dynamics variables
  gov.dyn <- get.gov.dynamics(pop.data, state.at.time.t,
                              tab.audit.costs)
  
  scale.factor <- baseline.rev / gov.dyn$IT.revenue
  gov.dyn$IT.revenue <- gov.dyn$IT.revenue * scale.factor

  ### The number of taxpayers is low (e.g 10^3 or 10^4) in our model and only a few get caught
  ### and penalized per year. Due to different income levels this prduces get year-to-year variability
  ### in amount of unpaid taxes and penalties recovered. This varability is not realistic when scaled 
  ### up to the US population. We thus need some smoothing. We thus contrstuct a new scale.factor.rev.pen
  ### that smoothes the variability of recoved mount of unpaid taxes and penalties over a ceratin number 
  ### of years (in this case = generation.half.life years).
  scale.factor.rev.pen <- mean(last(aggregated.dyn[,"total.penalty.and.past.tax.to.pay"],generation.half.life))
  scale.factor.rev.pen <- scale.factor.rev.pen/last(aggregated.dyn[,"total.penalty.and.past.tax.to.pay"]) 
  rev.pen <- get.baseline.recovered.tax.revenues.and.penalties(state.at.time.t, 
                                                               scale = scale.factor*scale.factor.rev.pen)
  
  
  other.rev <- get.other.government.revenues(US.tp.N = 151*10^6, 
                                             baseline.total_tax_revenues = gov.dyn$IT.revenue, 
                                             baseline.IRS.penalties.revenues = rev.pen)
  alpha_c <- get.alpha_C(US.tp.N=151*10^6, 
                         gov.dyn$audit.costs,
                         baseline.total.governmet.expenses=3.863052*10^12 )
  
  #Find first Calibrate Government Dynamics variables
  gov.dyn <- get.gov.dynamics(pop.data, state.at.time.t, 
                              tab.audit.costs, ids.by.audit.type, alpha_C = alpha_c,
                              US.other.government.revenues = other.rev, US.Debt=US.Debt,
                              US.marginal.Debt=US.marginal.Debt, scale.factor = scale.factor)
  
  gov.dyn$US.Debt <- US.Debt
  
  #Setting these to 0 explicitly at baseline
  gov.dyn$US.marginal.deficit <- 0
  gov.dyn$US.marginal.Debt <- 0

  inflated.indicators <- get.Debt.and.GDP.with.growth(year= 0, #Year 0 is for equilibrium
                                                      marginal.deficit=gov.dyn$US.marginal.deficit,
                                                      previous.year.inflated.Debt= gov.dyn$US.Debt-gov.dyn$US.deficit,
                                                      initial.deficit=gov.dyn$US.deficit,
                                                      initial.interest.payments=241*10^9,initial.GDP = 18.46*10^12,
                                                      GDP.growth.rate = 1.0241, Debt.interest.rate = (1+241*10^9/US.Debt))
  gd.scale.factor <- scale.factor
  baseline.gov.dyn <- cbind.data.frame(gov.dyn,inflated.indicators, other.rev, gd.scale.factor, alpha_c)
  
  return(baseline.gov.dyn)
  
}