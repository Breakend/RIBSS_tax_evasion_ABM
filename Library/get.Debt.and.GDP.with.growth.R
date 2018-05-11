
get.Debt.and.GDP.with.growth <- function(year=1,
                                         marginal.deficit=0,
                                         previous.year.inflated.Debt= 19.59*10^12,
                                         initial.deficit=590*10^9,
                                         initial.interest.payments=241*10^9,
                                         initial.GDP = 18.46*10^12,
                                         GDP.growth.rate = 1.0241, 
                                         Debt.interest.rate = (1+241*10^9/(19.59*10^12))){
  
  initial.deficit.non.interest <- initial.deficit- initial.interest.payments
  
  inflated.Debt <- as.numeric(previous.year.inflated.Debt*Debt.interest.rate) + 
    as.numeric((initial.deficit.non.interest+marginal.deficit)*GDP.growth.rate^year)
  
  GDP <- as.numeric(initial.GDP*GDP.growth.rate^year)
  
  
  return(data.frame(inflated.Debt=inflated.Debt,GDP=GDP,ratio=inflated.Debt/GDP))
  
}