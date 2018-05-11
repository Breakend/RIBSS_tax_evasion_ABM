
n<- 10^7

pa <- perceivedauditrate
r<- do.ALP.per.rate.analysis(per.rate=pa ,lab=lab,scale.up=5, n.bins=50)
log.per.audit.dist <- r$fit.log.rate
per.audit.dist<- log.per.audit.dist
per.audit.dist$x <- 10^(log.per.audit.dist$x)
sampled.per.audit <-sample(per.audit.dist$x,n, replace = T, prob=per.audit.dist$y)
rescale.to.match.mean <- mean(pa,na.rm=T)/mean(sampled.per.audit)
sampled.per.audit<- rescale.to.match.mean*sampled.per.audit
sampled.per.audit[sampled.per.audit>100] <- 100
summary(sampled.per.audit)
sampled.per.audit<- sampled.per.audit[order(sampled.per.audit)]

sampled.per.audit.full<- sampled.per.audit

pa <- perceivedauditrate[subsets$nonaudited.alters.nonaudited]
r<- do.ALP.per.rate.analysis(per.rate=pa ,lab=lab,scale.up=5, n.bins=50)
log.per.audit.dist <- r$fit.log.rate
per.audit.dist<- log.per.audit.dist
per.audit.dist$x <- 10^(log.per.audit.dist$x)

sampled.per.audit <-sample(per.audit.dist$x,n, replace = T, prob=per.audit.dist$y)
rescale.to.match.mean <- mean(pa,na.rm=T)/mean(sampled.per.audit)
sampled.per.audit<- rescale.to.match.mean*sampled.per.audit
sampled.per.audit[sampled.per.audit>100] <- 100
summary(sampled.per.audit)
sampled.per.audit<- sampled.per.audit[order(sampled.per.audit)]


print(summary(sampled.per.audit.full/sampled.per.audit))



pp <- perceivedpenaltyrate
r<- do.ALP.per.rate.analysis(per.rate=pp ,lab=lab,scale.up=5, n.bins=50)
log.per.penalty.dist <- r$fit.log.rate
per.penalty.dist<- log.per.penalty.dist
per.penalty.dist$x <- 10^(log.per.penalty.dist$x)
sampled.per.penalty <-sample(per.penalty.dist$x,n, replace = T, prob=per.penalty.dist$y)
rescale.to.match.mean <- mean(pp,na.rm=T)/mean(sampled.per.penalty)
sampled.per.penalty<- rescale.to.match.mean*sampled.per.penalty
summary(sampled.per.penalty)
sampled.per.penalty<- sampled.per.penalty[order(sampled.per.penalty)]


sampled.per.penalty.full<- sampled.per.penalty

pp <- perceivedpenaltyrate[subsets$nonaudited.alters.nonaudited]
r<- do.ALP.per.rate.analysis(per.rate=pp ,lab=lab,scale.up=5, n.bins=50)
log.per.penalty.dist <- r$fit.log.rate
per.penalty.dist<- log.per.penalty.dist
per.penalty.dist$x <- 10^(log.per.penalty.dist$x)
sampled.per.penalty <-sample(per.penalty.dist$x,n, replace = T, prob=per.penalty.dist$y)
rescale.to.match.mean <- mean(pp,na.rm=T)/mean(sampled.per.penalty)
sampled.per.penalty<- rescale.to.match.mean*sampled.per.penalty
summary(sampled.per.penalty)
sampled.per.penalty<- sampled.per.penalty[order(sampled.per.penalty)]

print(summary(sampled.per.penalty.full/sampled.per.penalty))
