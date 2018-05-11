remove(list = ls(all = TRUE))

### Load Libraries: please ensure you have installed these R packages.
library(ggplot2)
library(Hmisc)
library(igraph)
library(reshape2)
library(tgp)

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

library.dir   <- "Library/"

library <- file.path(library.dir, "library.R")
source(library)

##Set Seed 
set.seed(55279) ### first seed given by Chris Marcum
set.seed(55333) 
 


#############################################################
###                                                       ###
###        Set Directories and load Input/Output files              
###                                                       ###
#############################################################

data.dir<- "Tax data/"
util.dir<- "Utilities/"

model.parameters <- read.csv(paste(data.dir,"model.parameters_lit_review",".csv",sep=""))
rownames(model.parameters) <- model.parameters$parameter
model.parameters <- model.parameters[,c("Mode","Lower","Upper","pdf", "integer")]

source(paste(util.dir,"Plot_c1_range_of_distributions",".R",sep=""))

#############################################################
###                                                       ###
###        Settings              
###                                                       ###
#############################################################

## 500 cases and 10 realizations per case.
### Expected number of days 128*200*10*5/(60*60*24)

### for the expected # cases need in LHS see \cite{Giunta2003} 
### According to Giunta et al., he presented empirical estimations to get the number of experiments in Latin Hypercube Sampling only: 1.5*V runs for 5-10 variables (V), 3*V runs for 10-20 variables (V), and 4.5*V runs for a 20- 30 variables(V).


n.cases<- 100

model.options <- expand.grid( network.model = c("FALSE", "ER"), 
                              mild.tendency.to.full.evasion = c("TRUE", "FALSE"),
                              tax.refund.effect = c("TRUE", "FALSE"),
                              targetted.auditing = c("TRUE", "FALSE"),
                              bomb.crater.effect = c("TRUE", "FALSE"),
                              gamblers.fallacy = c("TRUE", "FALSE"),
                              media.effect = c("TRUE", "FALSE"),
                              population.size = 1000,
                              total.years = 100,
                              tax.gap = 0)

indices <- with(model.options, which(bomb.crater.effect == FALSE & gamblers.fallacy==TRUE))
model.options <- model.options[-indices, ]
model.options <- cbind(model.option=c(1:nrow(model.options )),model.options)


#############################################################
###                                                       ###
###        Generate the Latin Hyper Cube Samples for 
###         Experimental Design
###                                                       ###
#############################################################

#find point estimates.

point.estimates <- model.parameters$pdf%in%"Point"

model.parameters.to.sample <- model.parameters[!point.estimates,]
model.parameters.point.est <- model.parameters[point.estimates,]

lhs.tab <- t(apply(model.parameters.to.sample,1,UniTri.Inputs))
lhs.tab[model.parameters.to.sample$integer,"Mode"] <- round(lhs.tab[
  model.parameters.to.sample$integer,"Mode"],0)

mode.cases <-t(as.data.frame(lhs.tab[,"Mode"]))
mode.cases <- cbind(case=0,mode.cases)
mode.cases <- merge(model.options,mode.cases)


cases <- lhs(n.cases, lhs.tab[,c("Lower","Upper")], lhs.tab[,"Shape"], lhs.tab[,"Mode"]) 
colnames(cases) <- rownames(model.parameters.to.sample)
cases <- as.data.frame(cases)
cases[,model.parameters.to.sample$integer] <- round(cases[,model.parameters.to.sample$integer],0)


tmp <- as.data.frame(t(model.parameters.point.est$Mode))
colnames(tmp) <- rownames(model.parameters.point.est)
point.estimate.values <- tmp

cases <- cbind(case=c(1:nrow(cases)),cases,point.estimate.values)
cases<-merge(model.options,cases)

case.labeles<- c("model.option","case")
non.case.labeles <- colnames(cases)[!(colnames(cases)%in% case.labeles)]
cases<-cases[,c(case.labeles,non.case.labeles)]
mode.cases <- cbind(mode.cases,point.estimate.values)
mode.cases<-mode.cases[,c(case.labeles,non.case.labeles)]

cases<- rbind(mode.cases,cases)

estimated.run.time.in.sec.per.case.per.realization<- 5
num.realizations <- 10

estimated.run.time.whole.ED <-  round(num.realizations*nrow(cases)*estimated.run.time.in.sec.per.case.per.realization/(60*60*24),2)

print(paste("Est. running time",estimated.run.time.whole.ED,"days",sep=" "))

write.csv(cases,file=paste(data.dir,"test.cases",".csv",sep=""),row.names = FALSE)


### create seed
seed.list<-55279
for(i.seed in 2:num.realizations){
  seed.list<-c(seed.list,seed.list[i.seed-1]+1000)
}
seed.list<- cbind(realization= 1:num.realizations, random.seed= seed.list)

write.csv(seed.list,file=paste(data.dir,"random.seed.table",".csv",sep=""),row.names = FALSE)





