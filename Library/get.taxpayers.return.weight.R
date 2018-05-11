get.taxpayers.return.weight<- function(tax.rate, c1,c1.tilde,return.weight=return.weight){
  
  taxpayers.return.weight <- rep(return.weight,length(tax.rate))
  tmp <- ((tax.rate-c1)>0 & (c1.tilde-tax.rate)>0)
  taxpayers.return.weight[tmp] <- return.weight*(tax.rate[tmp]-c1[tmp])/(c1.tilde[tmp]-c1[tmp])
  taxpayers.return.weight[(tax.rate-c1)<=0]<-0
  
  return(taxpayers.return.weight)
}


### Verification: The weight place on the return received should be increasing with income since the bigger
### the income the bigger the tax return. Hence increasing +ve correlation. 
### However, for the supper rich ($180k) the weight should be smaller as they are surely nor relying on the
### tax return to buy their next sports car. 


# summary(pop.data$c1)
# summary(pop.data$tax.rate)
# 
# summary(pop.data$tax.rate-pop.data$c1)
# 
# 
# mean(pop.data$tax.rate-pop.data$c1)
# 
# tax.rate <- pop.data$tax.rate
# c1 <- pop.data$c1
# c1.tilde<-  state.at.time.t[, 'c1.tilde']
# 
# taxpayers.return.weight <- get.taxpayers.return.weight(tax.rate, c1,c1.tilde,return.weight=return.weight)
# 
# hist(taxpayers.return.weight)
#  
# 
# tmp <- pop.data$income<15000
# cor(taxpayers.return.weight[tmp],pop.data$income[tmp])
# 
# tmp <- pop.data$income<50000 & pop.data$income>=15000
# cor(taxpayers.return.weight[tmp],pop.data$income[tmp])
# 
# tmp <- pop.data$income<100000 & pop.data$income>=50000
# cor(taxpayers.return.weight[tmp],pop.data$income[tmp])
# 
# tmp <- pop.data$income<150000 & pop.data$income>=100000
# cor(taxpayers.return.weight[tmp],pop.data$income[tmp])
# 
# tmp <- pop.data$income<180000 & pop.data$income>=150000
# cor(taxpayers.return.weight[tmp],pop.data$income[tmp])
# 
# tmp <- pop.data$income>180000
# cor(taxpayers.return.weight[tmp],pop.data$income[tmp])

