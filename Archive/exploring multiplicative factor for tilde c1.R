remove(list = ls(all = TRUE))
gc()

##Set Seed 
set.seed(55279) ### first seed given by Chris Marcum
set.seed(55333) 

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

### Find on which platform and macchine we are running this on
if(.Platform$OS.type == "windows"){
  if (Sys.info()["nodename"]=="" ){
    OSdir <- paste("" ,sep="")}
  memory.limit(4000)
}else{ 
  if (Sys.info()["nodename"]=="vardavas-r.rand.org" | 
      Sys.info()["nodename"]=="vardavas-r.local"){
    OSdir <- paste("/Users/rvardava/Documents/Projects_2015/Tax_Evasion/R code/" ,sep="")}
  else {
    OSdir <- paste("/Users/rvardava/Documents/Projects_2015/Tax_Evasion/R code/" ,sep="")
  }
}

library(ggplot2)
library(Hmisc)

library.dir   <- "Library/"

library <- file.path(library.dir, "library.R")
source(library)

### set Working Directory
setwd(OSdir)


erf <- function(x) {2 * pnorm(x * sqrt(2)) - 1}
erf.inv <- function(x) {qnorm((x + 1)/2)/sqrt(2)}
logit <- function(x) {log(x/(1-x))}
cdf.logitnormal <- function(x,mu=0,sigma=1) {0.5* (1+erf((logit(x)-mu)/sqrt(2*sigma^2)))}
cdf.lognormal <- function(x,mu=0,sigma=1) {0.5* (1+erf((log(x)-mu)/(sqrt(2)*sigma)))}
#median e^mu


c1<-0.1
c2<-0.7

qP <- seq(0,1,0.01)

tmp <- as.data.frame(cbind(x=qP,y=cdf.lognormal(x=qP, log(0.06),1)))
plot(tmp$x,tmp$y,type="l",xlab="qP",ylab="Phi",lwd = 2, col = "dark red",main = "multiplicative factor in the tilde c1 equation")

tmp <- as.data.frame(cbind(x=qP,y=sqrt(qP)))
plot(tmp$x,tmp$y,type="l",xlab="qP",ylab="Phi",lwd = 2, col = "dark red",main = "multiplicative factor in the tilde c1 equation")





