#Calculate the compliance rate score for the dataset
#Basically, calcuates the mean squared error for the histogram values of proportion of
#income reported from 0-5% to 95-100%
#Lower the score the better it is...
#Values for comparison were given by Kim Bloomquist

calc.comp.rate.rmse <- function(dataset, method = "last-two-bins")
{
  #crate is short for comp.rate
  crate.model <- dataset[, grep("perc.hideable.income.reported.q.", names(dataset))]
  crate.obs <- read.csv("Tax data/c_rate_sensitive.csv", stringsAsFactors = F) #From Kim Bloomquist
  crate.obs <- crate.obs$percent
  
  #Converting 12 bins into 10 bins by removing 90-95% and 0-5% bins and adding them to the one above them
  crate.model[, 2] <- crate.model[, 1] + crate.model[, 2] #0-5% bin added to 0-10% bin
  crate.model[, 12] <- crate.model[, 11] + crate.model[, 12] #90-95% bin added to 90-100% bin
  crate.obs[2] <- crate.obs[1] + crate.obs[2] #0-5% bin added to 0-10% bin
  crate.obs[12] <- crate.obs[11] + crate.obs[12] #90-95% bin added to 90-100% bin
  #Now, dropping those columns since they are no longer needed
  crate.model <- crate.model[, -c(1,11)]
  crate.obs <- crate.obs[-c(1,11)]
  
  dist.measure <- NULL
  if(method == "all-four-bins") {
    dist.measure <- apply(crate.model, 1, function(x){ 
      100*sum((x[c(1,2,9,10)] - crate.obs[c(1,2,9,10)])^2)/100 #Dividing by 100 because theoretical max is 4 * 10^4
      #But Target range is differences of 5 percent. Hence, 4 bins times 5 squared
    })
  } else if (method == "last-two-bins") {
    dist.measure <- apply(crate.model, 1, function(x){ 
      100*sum((x[c(9,10)] - crate.obs[c(9,10)])^2)/100 #Dividing by 100 because theoretical max is 4 * 10^4
      #But Target range is differences of 5 percent. Hence, 4 bins times 5 squared
    })
  }
  
  return (dist.measure)
}