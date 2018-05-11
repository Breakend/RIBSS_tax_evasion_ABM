remove(list = ls(all = TRUE))

### Load Libraries: please ensure you have installed these R packages.
library(ggplot2)
library(Hmisc)
library(igraph)
library(reshape2)
library(tgp)
library(xtable)


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

## Specify directories 
model.inputs.dir   <- "Inputs/"
data.dir<- "Tax data/"
util.dir<- "Utilities/"
network.data.dir <- "Network data/"
survey.data.dir <- "Survey data/"

### Load Parameter values from the literature and expert opinion.
model.parameters <- read.csv(paste(data.dir,"model.parameters_lit_review",".csv",sep=""),stringsAsFactors =F)
rownames(model.parameters) <- model.parameters$parameter
model.parameters <- model.parameters[,c("Mode","Lower","Upper","pdf", "integer")]
model.parameters$Derived.from <- NA
model.parameters$inv.direction <- FALSE 

model.parameters$type <-NA
  

### Load Survey data analysis output from ALP_Survey_Analysis.R
ALP.model.param.tab <- readRDS(file=paste(survey.data.dir,"model.param.tab.Rdata",sep=""))

### Government & IRS Controls 

controls.parameters <- c("tax.rate","audit.rate","detection.eff","penalty.rate","K")
model.parameters[controls.parameters, "type"] <- "Controls"

model.parameters["audit.rate",c(1,2,3)]<- c(0.01,0.005,0.05)
model.parameters["audit.rate","pdf"] <- "PERT"

model.parameters["penalty.rate",c(1,2,3)]<- c(0.50,0.25,0.8)
model.parameters["penalty.rate","pdf"] <- "PERT"

model.parameters["K",c(1,2,3)]<- c(1,1,5)
model.parameters["penalty.rate","pdf"] <- "Poisson"

#Our estimate of the range in the penalty rate to be used in our ABM is based on considering the failure to file penalty and any accuracy related penalties. We assume that any amount of income that a taxpayer desires to under-report is not disclosed as part of their tax return. We further assume that all taxpayer who do file a tax return, including those that under-report their income pay their due taxes to the IRS on time. Consider the scenario whereby a taxpayer filed their tax return but instead of reporting the the amount of taxes \$ $A$ that they owe the IRS, they report a substantially lower amount \$  $A_R$. If the IRS audits and catches this taxpayer, s/he will have to pay the remaining \$  $A-A_R$ and will be charged an accuracy related penalty rate of  20\% plus interest on this amount. If instead the taxpayer under-reported their entire income (i.e., $A_R=0$) and does not file a tax return and is audited and caught,  will have to pay both the failure to file penalty and any accuracy related penalties that sum to  45\% plus interest on \$ $A$. The daily compound interest rate is assumed to be 4\% per year. Thus, by denoting $R_C=(1+0.04/365)^{365}$, the interest rate on taxes that are due $y$ tax years ago is given by $R_C^{y+0.5}-1$. However, assuming the back audit is three years we chose to simplify and assume an average rate of 5\% that applies to all years. Our estimated penalty rate is therefore 50\%. The lower bound value is 25\%. Our upper bound estimate is instead estimated as 80\% which is roughly the ratio between the total assessed civil penalties of \$25.6 billion and the total additional tax assessments of \$33.1 billion according estimated by the IRS for 2014 ( IRS Data Book 2014 pages 24 \& 44). 

### Experience importance and Generation half life 

time.parameters <- c("morale.half.life","generation.half.life","full.tendency.factor","t.media.range")
model.parameters[time.parameters, "type"] <- "Time Scales"

model.parameters["generation.half.life", "pdf"] <-"Derived"
model.parameters["generation.half.life","Derived.from"] <- "morale.half.life"  

model.parameters["generation.half.life",c(1:3)] <- c(25,10,35)  

model.parameters["t.media.range", "pdf"] <-"Derived"
model.parameters["t.media.range","Derived.from"] <- "morale.half.life"  

model.parameters["t.media.range",c(1:3)] <- c(25,10,35)  

# To model the bomb crater effect,  we thus assume that 1 and 3 years after an audit, taxpayers perceived audit rate that is 42 and 35\% below their baseline. We fit a linear relationship until the baseline line audit rate is recovered. Thus percieved audit rate drops to 1-0.42 = 0.58 in year 1 and 1-0.35 = 0.65 in year 3. Putting this in an exponential form we have that (0.35/0.42)=exp(-(1-s)*2) => s=0.9088 or 1/2 life of 7.6 years. 

model.parameters["morale.half.life",c(1:3)] <- c(2,1,8)  


model.parameters["full.tendency.factor",c(1:3)] <- c(1,1,2)
model.parameters["full.tendency.factor","pdf"] <- "Uni"
model.parameters["full.tendency.factor","integer"] <- TRUE

### Personal Fairness Parameters.

personal.parameters <- c("c1.dist.weight","c2","beta.personal")
model.parameters[personal.parameters, "type"] <- "Personal"

model.parameters["c1.dist.weight",c(1:3)]<- c(0,0,1) ## RV change
model.parameters["c1.dist.weight","pdf"]<- "Uni"

model.parameters["beta.personal",c(2,1,3)] <-  ALP.model.param.tab$importance.on.morale$personal/100
model.parameters["beta.personal","pdf"] <- "PERT"

model.parameters["beta.personal","Upper"] <- 1
model.parameters["beta.personal","Mode"] <- 0.55
model.parameters["beta.personal","Lower"] <- 0.45

### Penalty scale

penalty.scale.parameters <- c("v.PP")
model.parameters[penalty.scale.parameters, "type"] <- "Evasion Relapse"

model.parameters["v.PP",c(1:3)]<- c(0.9,0.5,1) ## RV change
model.parameters["v.PP","pdf"]<- "PERT"


### Network Parameters.
network.parameters <- c("ave.degree.tTaxes","beta.network")
model.parameters[network.parameters, "type"] <- "Social"

model.parameters["ave.degree.tTaxes",c(1:3)] <- ALP.model.param.tab$network.degree.manipulations[c(2:4)]
model.parameters["ave.degree.tTaxes","pdf"] <- "PERT"

model.parameters["beta.network",c(1:3)] <- (1-model.parameters["beta.personal",c(1:3)])/2
model.parameters["beta.network",c(2:3)] <- model.parameters["beta.network",c(3:2)] 
model.parameters["beta.network","Lower"] <- 0.0
model.parameters["beta.network","Mode"] <- 0.225
model.parameters["beta.network","Upper"] <- 0.275

model.parameters["beta.network","pdf"] <- "Derived"
model.parameters["beta.network","Derived.from"] <- "beta.personal"
model.parameters["beta.network","inv.direction"] <- TRUE


### Media Parameters.
media.parameters <- c("media.mid.effect","media.steepness","media.stochastic.offset","tax.gap.reporting.media.threshold","beta.media")
model.parameters[media.parameters, "type"] <- "Media"

model.parameters["media.mid.effect",c(2,3)] <- sort(ALP.model.param.tab$Med.Eff.morale$Med.Eff.morale$m)
model.parameters["media.mid.effect",1] <- mean(ALP.model.param.tab$Med.Eff.morale$Med.Eff.morale$m) 
model.parameters["media.mid.effect","pdf"] <- "Uni"
model.parameters["media.steepness",c(2,3)] <- sort(ALP.model.param.tab$Med.Eff.morale$Med.Eff.morale$s)
model.parameters["media.steepness",c(1)] <- mean(ALP.model.param.tab$Med.Eff.morale$Med.Eff.morale$s)
model.parameters["media.steepness","pdf"] <- "Derived"
model.parameters["media.steepness","Derived.from"] <- "media.mid.effect"
model.parameters["media.stochastic.offset",c(1)] <- 0 #ALP.model.param.tab$Med.Eff.morale$Med.Eff.morale$K[2] ### remove the stochastic offset cases
model.parameters["media.stochastic.offset",c(2,3)] <-NA
model.parameters["media.stochastic.offset","pdf"] <- "Point"

model.parameters["tax.gap.reporting.media.threshold",c(1:3)] <- c(0.1,0.1,0.15)
model.parameters["tax.gap.reporting.media.threshold","pdf"] <- "Uni"
#model.parameters["media.deterrence.above.avg.threshold.factor",c(1:3)] <- c(1.14,1.09,1.17)
#model.parameters["media.deterrence.above.avg.threshold.factor","pdf"] <- "Uni"
model.parameters["beta.media",] <- model.parameters["beta.network",]
model.parameters["beta.media","type"] <-  "Media"

# 
# model.parameters["media.tax.rate.effect.point",c(1:3)] <- c(ALP.model.param.tab$Med.Eff.morale$tax.rate.effect.point,NA,NA)
# model.parameters["media.tax.rate.effect.point",c("pdf","integer")] <- c("Point",FALSE)

### Response to Deterrance 

Deterrence.parameters <- c("penalty.asymmetry.factor",
                   "m.qP","s.qP",
                   "gamblers.fallacy.grad","gamblers.fallacy.intercept",
                   "bomb.crater.factor")

model.parameters[Deterrence.parameters, "type"] <- "Deterrence Response"

model.parameters["penalty.asymmetry.factor",c(1:3)] <- c(1,NA,NA)
model.parameters["penalty.asymmetry.factor","pdf"] <-"Point"

model.parameters<- model.parameters[!rownames(model.parameters)%in%"mu", ]
model.parameters["m.qP",c(1:3)]<- ALP.model.param.tab$qP$qP.summary["m",c("Median","1st Qu.","3rd Qu.")]
model.parameters["m.qP","pdf"] <- "PERT"
model.parameters["s.qP",c(1:3)]<- ALP.model.param.tab$qP$qP.summary["s",c("Median","1st Qu.","3rd Qu.")]
model.parameters["s.qP","pdf"] <- "Derived"
model.parameters["s.qP","Derived.from"] <- "m.qP"

model.parameters["gamblers.fallacy.grad",c(1:3)] <- ALP.model.param.tab$gamb.fallacy.effect[c(2,1,3),2]
model.parameters["gamblers.fallacy.grad","pdf"] <- "PERT"


model.parameters["gamblers.fallacy.intercept",c(1:3)] <- ALP.model.param.tab$gamb.fallacy.effect[c(2,1,3),1]
model.parameters["gamblers.fallacy.intercept","pdf"] <- "Derived"
model.parameters["gamblers.fallacy.intercept","Derived.from"] <- "gamblers.fallacy.grad"


model.parameters["bomb.crater.factor",c(1:3)] <- ALP.model.param.tab$bomb.crater.effect[c(2,1,3)]
model.parameters["bomb.crater.factor","pdf"] <- "PERT"

### Refund effect Parameters 

### Here we use tabele 6 of Christan 1994:
### estimate 1: return.weight = 1-(1-0.58)/(1-0.326)=0.37 
### estimate 2: return.weight = 1-(0.888)/(0.968)=0.08264463 

refund.parameters <- c("rate.refund.movement","return.weight")
model.parameters[refund.parameters, "type"] <- "Refund"

#model.parameters["prob.additional.to.refund",] <- model.parameters["prob.refund.to.additional",]
#model.parameters["prob.additional.to.refund",c(1:3)] <- model.parameters["prob.refund.to.additional",c(1:3)]*1.5

model.parameters["rate.refund.movement",c(1:3)] <- c(0.1,0.05,0.2)
model.parameters["rate.refund.movement","pdf"] <- "PERT"

model.parameters["return.weight",c(1:3)] <- c(0.08,0,0.37)
model.parameters["return.weight","pdf"] <- "PERT"

model.parameters[is.na(model.parameters$type), "type"] <- "Other"

### Adjust Table
model.parameters<- model.parameters[order(model.parameters$type,rownames(model.parameters)),]

model.parameters$integer <- as.logical(model.parameters$integer)
model.parameters$integer[is.na(model.parameters$integer)] <- FALSE 

model.parameters$inv.direction <- as.logical(model.parameters$inv.direction)
model.parameters$inv.direction[is.na(model.parameters$inv.direction)] <- FALSE 
model.parameters$inv.direction[is.na(model.parameters$Derived.from)] <- NA


model.parameters[model.parameters$pdf%in%"Point",
                            c("Lower","Upper")] <- NA 

model.parameters<- model.parameters[rownames(model.parameters) %in% 
                              c(controls.parameters,
                                time.parameters,
                                personal.parameters,
                                penalty.scale.parameters,
                                network.parameters,
                                media.parameters, 
                                Deterrence.parameters,
                                refund.parameters),]

if(all(!model.parameters$inv.direction,na.rm=T)){
  model.parameters<-model.parameters[,
                !(colnames(model.parameters)%in%"inv.direction")]
}

print(model.parameters)

View(model.parameters)

write.csv(model.parameters, paste(model.inputs.dir,"Exp.Design.model.parameters",".csv",sep=""), row.names = T)

saveRDS(ALP.model.param.tab$network.degree.manipulations, paste(model.inputs.dir,"ALP.network.degree.manipulations",".Rdata",sep=""))


config.mode <- read.csv(paste(model.inputs.dir,"model.config",".csv",sep=""),stringsAsFactors =F)

config.mode$value[match(rownames(model.parameters),config.mode$config.vars)]<- model.parameters$Mode

write.csv(config.mode, paste(model.inputs.dir,"model.config",".csv",sep=""), row.names = F)


### Export Latex table

tab<- model.parameters
names(tab)[4] <- "Distribution"
names(tab)[8] <- "Type"

tab$Name <- rownames(tab) 
tab$Symbol <- NA

tab$Symbol <- c("$q_t$","$\\epsilon_A$","$K$","$P_t$","$T_t$","$\\kappa_{\\mbox{\\tiny BC}}$", "$\\gamma_{\\mbox{\\tiny GF}}$","$\\alpha_{\\mbox{\\tiny GF}}$", "$m_x$", "asymm.","$s_x$","$\\beta_M$","$\\vartheta_M$","$m_m$","$s_m$","M.Stoch.Off","$\\Theta_M$","$\\beta_P$","c1.dist.weight","$c_2$","$\\rho_{a\\to r}$","$\\rho_{r\\to a}$","$r_w$","$\\langle k_N \\rangle$","$\\beta_N$","$v$","$\\tau_G$","$\\tau$")

tab$Source <- c("IRS","IRS","Expert Opinion","IRS","IRS",rep("ALP",7),"Estimate",rep("ALP",2),NA, "Estimate","ALP","Estimate","Literature","Expert Opinion","Expert Opinion","Literature", "ALP","ALP", rep("ALP",3)) 

tab[,"Code Section"] <- "TBD" 

tab  <- tab[,c("Name","Symbol","Type","Code Section","Mode","Lower","Upper","Distribution","Source")]
tab  <- tab[,c("Name","Symbol","Type","Code Section","Source")]

tab  <- tab[!(tab$Name%in%c("c1.dist.weight","media.stochastic.offset","penalty.asymmetry.factor")),] 

#print(xtable(tab,digits=2,auto=T),file=paste("Writeup/Tables/modelinputpa#rameters",".tex",sep=""), 
 #     include.rownames=F,sanitize.text.function = function(x){x})

print(xtable(tab,digits=2,auto=T),file=paste("Writeup/Tables/modelinputparameters",".html",sep=""), 
      include.rownames=F,sanitize.text.function = function(x){x}, type = "html")


population.data.file = "Inputs/PN1_population_data.csv"
pop.data <- get.population.data(population.data.file)

tab.attribures <- data.frame(attribute=colnames(pop.data) )
tab.attribures$Symbol <- NA

print(xtable(tab.attribures,digits=2,auto=T),file=paste("Writeup/Tables/modeltaxpayerattributes",".tex",sep=""), 
     include.rownames=F,sanitize.text.function = function(x){x})



tab.attribures <- data.frame(attribute=colnames(state.at.time.t) )
tab.attribures$Symbol <- NA

print(xtable(tab.attribures,digits=2,auto=T),file=paste("Writeup/Tables/modeltaxpayerattributesDyn",".tex",sep=""), 
      include.rownames=F,sanitize.text.function = function(x){x})


taxrate.data.file = "Inputs/US_Income_Tax_Rates_2016.csv"
taxrate.data <- get.population.data(taxrate.data.file )
taxrate.data$range <- paste("[",taxrate.data$min/1000,", ",taxrate.data$max/1000,")",sep="")

tab<-table(taxrate.data$tax.rate,taxrate.data$filing.status)
for(ii in colnames(tab)){
  tab[,ii] <- taxrate.data$range[taxrate.data$filing.status%in%ii]
}
#print(xtable(tab),file=paste("Writeup/Tables/modelTaxBracketsRates2016",".tex",sep=""), 
 #     include.rownames=T,sanitize.text.function = function(x){x})


taxrate.data.file = "Inputs/US_Income_Tax_Rates_Trump_old.csv"
taxrate.data <- get.population.data(taxrate.data.file )
taxrate.data$range <- paste("[",taxrate.data$min/1000,", ",taxrate.data$max/1000,")",sep="")

tab<-table(taxrate.data$tax.rate,taxrate.data$filing.status)
for(ii in colnames(tab)){
  tab[,ii] <- taxrate.data$range[taxrate.data$filing.status%in%ii]
}
# print(xtable(tab),file=paste("Writeup/Tables/modelTaxBracketsRates2018_proposed",".tex",sep=""), 
#       include.rownames=T,sanitize.text.function = function(x){x})


taxrate.data.file = "Inputs/US_Income_Tax_Rates_Trump.csv"
taxrate.data <- get.population.data(taxrate.data.file )
taxrate.data$range <- paste("[",taxrate.data$min/1000,", ",taxrate.data$max/1000,")",sep="")

tab<-table(taxrate.data$tax.rate,taxrate.data$filing.status)
for(ii in colnames(tab)){
  tab[,ii] <- taxrate.data$range[taxrate.data$filing.status%in%ii]
}
# print(xtable(tab),file=paste("Writeup/Tables/modelTaxBracketsRates2018",".tex",sep=""), 
#      include.rownames=T,sanitize.text.function = function(x){x})

