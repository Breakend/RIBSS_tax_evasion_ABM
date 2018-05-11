remove(list = ls(all = TRUE))
gc()

library(xtable)
library(texreg)
library(yacca)
library(ggplot2)
library(reshape2)
require(Hmisc)
library(foreign)
library(corrgram)
library(xlsx)
library(dplyr)
library(nnet)


options(warn=1) 
set.seed(55333) 

## Specify directories 
data.dir   <- "Tax data/"
network.data.dir <- "Network data/"
survey.data.dir <- "Survey data/"
survey.output.dir <- "SurveyTablesPlots/"
model.inputs.dir   <- "Inputs/"
library.dir   <- "Survey code/SurveyLibrary/"
library <- file.path(library.dir, "library.R")
source(library)
library.dir   <- "Library/"
library <- file.path(library.dir, "library.R")
source(library)

### Load eff tax rates

eff.tax.rate <- read.csv("Tax data/US_Income_Tax_Rates_2016.csv", stringsAsFactors = F)
eff.audit.rate <- read.csv("Tax data/Audit_Rates_2015.csv", stringsAsFactors = F)

### Load Survey Data
ego.data <- readRDS(file=paste(survey.data.dir,"ego.data.Rdata",sep=""))
alters.table<-readRDS(file=paste(survey.data.dir,"EgoNet",".Rdata",sep=""))
SD.clean<-readRDS(file=paste(survey.data.dir,"SD.clean",".Rdata",sep=""))


### Load Survey data analysis output from ALP_Survey_Analysis.R
model.param.tab<- readRDS(file=paste(survey.data.dir,"model.param.tab.Rdata",sep=""))

### Create Pop Subsets
subsets  <- data.frame(all=rep(T,nrow(ego.data)),
                       audited = ego.data$hheveraudited,
                       nonaudited = !(ego.data$hheveraudited),
                       selfemployed = ego.data$selfemployed,
                       not.selfemployed = !(ego.data$selfemployed))

tmp <- alters.table$altertaxaudit
in.contact.with.audited.alters<- (tmp$Yes>0 | tmp$`I think so`>0)
subsets$nonaudited.alters.nonaudited <- subsets$nonaudited & !in.contact.with.audited.alters
subsets$nonaudited.alters.audited <- subsets$nonaudited & in.contact.with.audited.alters
subsets$audited.alters.nonaudited <- subsets$audited & !in.contact.with.audited.alters
subsets$audited.alters.audited <- subsets$audited & in.contact.with.audited.alters

### Get selected fields for the regressions
selected.fields <- c("prim_key","currentlivingsituation" ,
                     "perceivedevasionratepopulation","perceivedevasionrate",
                     "perceivedpenaltyrate",
                     "perceivedauditrate",
                     "perceivedcaught",
                     "riskauditspenalties_a","riskauditspenalties_b",
                     "riskauditspenalties_c",
                     "perceivedtaxrate","tax.rate.threshold.min",
                     "tax.rate.threshold.max",
                     "c1.guessed.unmodified","c1.guessed",
                     "c1.guess.majority.int", "tax.rate.threshold.tri", 
                     "servicestaxes",
                     "actor",
                     "s","m",
                     "perceivedaruprob",
                     "bombcrateramount")

df<- SD.clean[,selected.fields]


df$actor<- -(df$actor-3) ### NOTE: changed the sign and definition here
df$actor.logical<- df$actor>0
df$servicestaxes<- df$servicestaxes-3
df$preptaxes.CPA <- SD.clean$preptaxes
tmp<-(df$preptaxes.CPA<3)
tmp[df$preptaxes.CPA>3] <- NA
df$preptaxes.CPA <- as.numeric(!(tmp) )


df <- merge(ego.data,df, by ="prim_key")

df$perceivedevasionratepopulation <- as.numeric(df$perceivedevasionratepopulation)
x<- df$n.alters/df$n.alters.tTaxes
x[x==Inf] <- NA
x[is.nan(x)] <-NA
df$prop.alters.tTaxes<-x


subsets$actor <-  (df$actor>0)
tmp<- rep("Head.of.Household",nrow(df))
tmp[df$currentlivingsituation==1] <- "Married.Filing.Jointly"
tmp[df$currentlivingsituation==5] <- "Single"
df$filing.status<- tmp

tax.rate.income.bracket.file <- paste(data.dir,"US_Income_Tax_Rates_2013",".csv",sep="")
tax.rate.income.bracket <- read.csv(file=tax.rate.income.bracket.file,head=TRUE,stringsAsFactors = FALSE)
tax.rate.income.bracket <- split(tax.rate.income.bracket,tax.rate.income.bracket$filing.status)


df$income.val.5 <- df$income.val/10^5

income.info<- df[,c("income.val","filing.status")]
names(income.info) <- c("Income","filing.status")

income.tax <- 
  apply(income.info,1,get.effective.tax.rate,tax.rate.income.bracket=tax.rate.income.bracket)

df$effectivetaxrate <- round(100*income.tax/ income.info$Income,0)

df$diff.perceived.effective.taxrate <- df$perceivedtaxrate - df$effectivetaxrate

df<-ALP.fill.out.missing.variables.for.regression(df)

write.csv(df, paste(survey.data.dir,"Survey_data_input_to_Regression_Analysis",".csv",sep=""),row.names = F)

########################################
###                                  ###
### Load Demo data for the ABM and add missing attributes
########################################

network.data.file <- "PN1"
for( network.data.file in c("PN1","PN10")){
  
  model.pop.attributes<- readRDS(file=paste(network.data.dir,network.data.file,
                              ".demog.person.RData",sep=""))
  
  fields.to.keep<-colnames(model.pop.attributes)
  
  
  ### set intial fields in model.pop.attributes with dummy values.
  model.pop.attributes$calcage<- model.pop.attributes$age
  model.pop.attributes$income.val.5<- model.pop.attributes$income/10^5
  model.pop.attributes$gender <- as.factor(model.pop.attributes$gender)
  levels(model.pop.attributes$gender) <- c("Female","Male")
  model.pop.attributes$selfemployed <- model.pop.attributes$self.employed
  model.pop.attributes$alterselfemployed[model.pop.attributes$self.employed]<- 
    mean(df$alterselfemployed[df$selfemployed],na.rm=T)
  model.pop.attributes$alterselfemployed[!(model.pop.attributes$self.employed)]<- 
    mean(df$alterselfemployed[!(df$selfemployed)],na.rm=T)
  model.pop.attributes$prop.alters.tTaxes <- mean(df$prop.alters.tTaxes,na.rm=T)
  model.pop.attributes$actor <- mean(df$actor,na.rm=T)
  model.pop.attributes$perceivedevasionratepopulation <- mean(df$perceivedevasionratepopulation,na.rm=T)
  model.pop.attributes$everaudited <- F
  model.pop.attributes$spouseaudit <- F
  model.pop.attributes$altertaxaudit.tTaxes <- mean(df$altertaxaudit.tTaxes,na.rm=T)
  model.pop.attributes$servicestaxes <- mean(df$servicestaxes,na.rm=T)
  model.pop.attributes$tax.rate.threshold.max <-   mean(df$tax.rate.threshold.max,na.rm=T) 
  model.pop.attributes$perceivedevasionrate <- mean(df$perceivedevasionrate,na.rm=T)
  model.pop.attributes$perceivedpenaltyrate <- mean(df$perceivedpenaltyrate,na.rm=T)
  model.pop.attributes$perceivedauditrate <- mean(df$perceivedpenaltyrate,na.rm=T)
  
  
  NA.fields<-names(df)[grepl("NA.Indicator", names(df))]
  model.pop.attributes[,NA.fields] <- FALSE
  
  ########################################
  ###                                  ###
  ### Regression : perceivedtaxrate
  ########################################
  
  
  dependent <- "perceivedtaxrate"
  covariates <- c("calcage","gender","income.val.5",
                  "everaudited","spouseaudit","selfemployed")
                 # "prop.alters.tTaxes","altertaxaudit.tTaxes","alterselfemployed",
             #     "actor","servicestaxes","perceivedevasionratepopulation",
              #    "perceivedevasionrate","tax.rate.threshold.max")
  
  lab<- "Linear Regression on perceivedtaxrate"
  gillo<-ALP.glm(df,dependent,covariates,
                 threshold = 0.05,lab=lab,
                 survey.output.dir=survey.output.dir)
  
  saveRDS(gillo,file=paste(survey.output.dir,"Reg_perceivedtaxrate",".Rdata",sep=""))
  
  model.pop.attributes$perceivedtaxrate<-round(predict(gillo,newdata=model.pop.attributes),0)/100
  
  fields.to.keep<- c(fields.to.keep,"perceivedtaxrate")
  
  
  #### Summary stats of the percieved audit rate for different subsets.
  perceivedtaxrate <- SD.clean[,"perceivedtaxrate"]
  tab <- list()
  tab$all<- summaryfunctionFull(perceivedtaxrate)
  tab$nonaudited.alters.nonaudited<-
    summaryfunctionFull(perceivedtaxrate[subsets$nonaudited.alters.nonaudited])
  tab$nonaudited.alters.audited<-
    summaryfunctionFull(perceivedtaxrate[subsets$nonaudited.alters.audited])
  tab$audited.alters.nonaudited<-
    summaryfunctionFull(perceivedtaxrate[subsets$audited.alters.nonaudited])
  tab$audited.alters.audited<-summaryfunctionFull(perceivedtaxrate[subsets$audited.alters.audited])
  ### Save summary table
  tab <- as.data.frame(do.call("rbind",tab))
  tab <- cbind(rownames(tab),tab)
  colnames(tab)[1] <- "Respondent subsample"
  print(xtable(tab,auto=T,digits=1), 
        file=paste(survey.output.dir,"perceivedtaxrate_summary",".tex",sep=""),
        include.rownames=F)
  
  
  
  ########################################
  ###                                  ###
  ### Regression : find who uses CPA
  ########################################
  
  
  dependent <- "preptaxes.CPA"
  covariates <- c("calcage","gender","income.val.5","perceivedtaxrate",
                  "everaudited","spouseaudit","perceivedauditrate",
                  "prop.alters.tTaxes","altertaxaudit.tTaxes",
                  "selfemployed","alterselfemployed",
                  "actor","servicestaxes","perceivedevasionratepopulation",
                  "perceivedevasionrate","tax.rate.threshold.max")
  
  lab<- "Logistic Regression on whether respondent uses an tax expert to prepare"
  gillo<-ALP.glm(df,dependent,covariates,family = binomial(link = "logit"),
          threshold = 0.0,lab=lab,
          survey.output.dir=survey.output.dir)
  
  saveRDS(gillo,file=paste(survey.output.dir,"Reg_CPA",".Rdata",sep=""))
  
  
  model.pop.attributes$preptaxes.CPA<-as.logical(sapply(predict(gillo,newdata=model.pop.attributes,type="response"),stoch.round))
  
  fields.to.keep<- c(fields.to.keep,"preptaxes.CPA")
  
  
  ########################################
  ###                                  ###
  ### Regression : find who can be influenced by actor/high earner being audited.
  ########################################
  
  
  dependent <- "actor.logical"
  covariates <- c("calcage","gender","income.val.5","perceivedtaxrate",
                  "everaudited","spouseaudit","perceivedauditrate","perceivedpenaltyrate",
                  "prop.alters.tTaxes","altertaxaudit.tTaxes",
                  "selfemployed","alterselfemployed",
                  "servicestaxes","perceivedevasionratepopulation",
                  "perceivedevasionrate","tax.rate.threshold.max")
  
  lab<- "Logistic Regression on whether respondent is affected by knowing an actor was audited"
  gillo<-ALP.glm(df,dependent,covariates,family = binomial(link = "logit"),
                 threshold = 0.05,lab=lab,
                 survey.output.dir=survey.output.dir)
  
  saveRDS(gillo,file=paste(survey.output.dir,"Reg_actor_affect",".Rdata",sep=""))
  
  
  model.pop.attributes$actor.logical<-as.logical(sapply(predict(gillo,newdata=model.pop.attributes,type="response"),stoch.round) )
  
  fields.to.keep<- c(fields.to.keep,"actor.logical")
  
  
  
  ########################################
  ###                                  ###
  ### Regression: Perceived audit depends
  ########################################
  
  
  dependent <- "perceivedauditrate"
  covariates <- c("calcage","gender","income.val.5","perceivedtaxrate",
                  "everaudited","spouseaudit",
                  "prop.alters.tTaxes","altertaxaudit.tTaxes",
                  "selfemployed","alterselfemployed",
                  "actor","servicestaxes",
                  "perceivedevasionrate","tax.rate.threshold.max")
  
  lab<- "Linear Regression of the Perceived Audit Rate"
  gillo<-ALP.glm(df,dependent,covariates,
                 threshold = 0.05,lab=lab,
                 survey.output.dir=survey.output.dir)
  
  
  saveRDS(gillo,file=paste(survey.output.dir,"Reg_perceivedauditrate",".Rdata",sep=""))
  
  reg.perceivedauditrate<-round(predict(gillo,newdata=model.pop.attributes,type="response"),0)
  
  
  
  #### Summary stats of the percieved audit rate for different subsets.
  perceivedauditrate <- SD.clean[,"perceivedauditrate"]
  tab <- list()
  tab$all<- summaryfunctionFull(perceivedauditrate[!is.na(perceivedauditrate)])
  tab$nonaudited.alters.nonaudited<-
    summaryfunctionFull(perceivedauditrate[subsets$nonaudited.alters.nonaudited])
  tab$nonaudited.alters.audited<-
    summaryfunctionFull(perceivedauditrate[subsets$nonaudited.alters.audited])
  tab$audited.alters.nonaudited<-
    summaryfunctionFull(perceivedauditrate[subsets$audited.alters.nonaudited])
  tab$audited.alters.audited<-summaryfunctionFull(perceivedauditrate[subsets$audited.alters.audited])
  ### Save summary table
  tab <- as.data.frame(do.call("rbind",tab))
  tab <- cbind(rownames(tab),tab)
  colnames(tab)[1] <- "Respondent subsample"
  print(xtable(tab,auto=T,digits=1), 
        file=paste(survey.output.dir,"perceivedaudit_summary",".tex",sep=""),
         include.rownames=F)
  
  
  #### Generated distribution of perceived audit rate for those never audited and that know
  ## no-one that was audited using the survey
  pa <- perceivedauditrate[subsets$nonaudited.alters.nonaudited]
  r<- do.ALP.per.rate.analysis(per.rate=pa ,lab="Perceived Audit Rate",
                               scale.up=1, n.bins=50,text.size=12)
  print(r)
  log.per.audit.dist <- r$fit.log.rate
  per.audit.dist<- log.per.audit.dist
  per.audit.dist$x <- 10^(log.per.audit.dist$x)
  n<- nrow(model.pop.attributes)
  sampled.per.audit <-sample(per.audit.dist$x,n, replace = T, prob=per.audit.dist$y)
  rescale.to.match.mean <- mean(pa,na.rm=T)/mean(sampled.per.audit)
  sampled.per.audit<- rescale.to.match.mean*sampled.per.audit
  sampled.per.audit[sampled.per.audit>100] <- 100
  summary(sampled.per.audit)
  
  for(k in c("Income")){
  print(cor(sampled.per.audit,model.pop.attributes[,k]) )## cor should be ~ 0
  }
  
  #### Rearrange the distibution by assigning this perceieved audit rate according to the 
  ### regression model - this will now include the correlations with the significant covariates
  sampled.per.audit<- ALP.get.rearranged.sampled.dist(sampled.per.audit,
                                                      reg.perceivedauditrate)
  
  sampled.per.audit<- round(sampled.per.audit,3)
  sampled.per.audit[sampled.per.audit>1] <- round(sampled.per.audit[sampled.per.audit>1],2)
  sampled.per.audit[sampled.per.audit>5] <- round(sampled.per.audit[sampled.per.audit>5],1)
  sampled.per.audit[sampled.per.audit>10] <- round(sampled.per.audit[sampled.per.audit>10],0)
  model.pop.attributes$perceivedauditrate <- sampled.per.audit/100
  
  for(k in c("Income")){
    print(cor(model.pop.attributes$perceivedauditrate,model.pop.attributes[,k]))## cor is no longer 0. 
  }
  
  fields.to.keep<- c(fields.to.keep,"perceivedauditrate")
  
  ########################################
  ###                                  ###
  ### Regression2: find how percieved penalty rate
  ########################################
  
  
  dependent <- "perceivedpenaltyrate"
  covariates <- c("calcage","gender","income.val.5","perceivedtaxrate",
                  "everaudited","spouseaudit",
                  "prop.alters.tTaxes","altertaxaudit.tTaxes",
                  "selfemployed","alterselfemployed",
                  "actor","servicestaxes","perceivedevasionratepopulation",
                  "perceivedevasionrate","tax.rate.threshold.max")
  
  lab<- "Linear Regression of the Percieved Penalty Rate"
  gillo<-ALP.glm(df,dependent,covariates,
                 threshold = 0.05,lab=lab,
                 survey.output.dir=survey.output.dir)
  
  saveRDS(gillo,file=paste(survey.output.dir,"Reg_perceivedpenaltyrate",".Rdata",sep=""))
  
  
  reg.perceivedpenaltyrate<-round(predict(gillo,newdata=model.pop.attributes,type="response"),0)
  
  
  
  #### Summary stats of the percieved audit rate for different subsets.
  perceivedpenaltyrate <- SD.clean$perceivedpenaltyrate
  tab <- list()
  tab$all<- summaryfunctionFull(perceivedpenaltyrate)
  tab$nonaudited.alters.nonaudited<-
    summaryfunctionFull(perceivedpenaltyrate[subsets$nonaudited.alters.nonaudited])
  tab$nonaudited.alters.audited<-
    summaryfunctionFull(perceivedpenaltyrate[subsets$nonaudited.alters.audited])
  tab$audited.alters.nonaudited<- 
    summaryfunctionFull(perceivedpenaltyrate[subsets$audited.alters.nonaudited])
  tab$audited.alters.audited<-  
    summaryfunctionFull(perceivedpenaltyrate[subsets$audited.alters.audited])
  ### Save summary table
  tab <- as.data.frame(do.call("rbind",tab))
  tab <- cbind(rownames(tab),tab)
  colnames(tab)[1] <- "Respondent subsample"
  print(xtable(tab,auto=T,digits=1), 
        file=paste(survey.output.dir,"perceivedpenaltyrate_summary",".tex",sep=""),
        include.rownames=F)
  
  #### Generated distribution of percied penalty rate for those never penalized and that know
  ## no-one that was penalized using the survey
  pp <- perceivedpenaltyrate[subsets$nonaudited.alters.nonaudited]
  r<- do.ALP.per.rate.analysis(per.rate=pp ,lab="Percieved Penalty Rate",scale.up=1, n.bins=25,text.size=12)
  print(r)
  log.per.penalty.dist <- r$fit.log.rate
  per.penalty.dist<- log.per.penalty.dist
  per.penalty.dist$x <- 10^(log.per.penalty.dist$x)
  n<- nrow(model.pop.attributes)
  sampled.per.penalty <-sample(per.penalty.dist$x,n, replace = T, prob=per.penalty.dist$y)
  rescale.to.match.mean <- mean(pp,na.rm=T)/mean(sampled.per.penalty)
  sampled.per.penalty<- rescale.to.match.mean*sampled.per.penalty
  summary(sampled.per.penalty)
  cor(sampled.per.penalty,model.pop.attributes$Income) ## cor should be ~ 0
  
  #### Rearrange the distibution by assigning this perceieved penalty rate according to the 
  ### regression model - this will now include the correlations with the significant covariates
  sampled.per.penalty<- ALP.get.rearranged.sampled.dist(sampled.per.penalty,
                                                        reg.perceivedpenaltyrate)
  model.pop.attributes$perceivedpenaltyrate <- round(sampled.per.penalty,0)/100
  
  for(k in c("Income")){
    print(cor(model.pop.attributes$perceivedpenaltyrate,model.pop.attributes[,k]))## cor is no longer 0. 
  }
  
  fields.to.keep<- c(fields.to.keep,"perceivedpenaltyrate")
  
  
  source(paste("Survey code/","Calibration.Target.Increased.Perceived.Audit.Rate.R",sep=""))
  
  ########################################
  ###                                  ###
  ### Regression 3: find how c_1 (i.e.) tax.rate.threshold.max
  ### depends on age, income.cat , "perceivedevasionratepopulation" "perceivedevasionrate"
  ###
  ########################################
  
  
  ### Note we include "perceivedevasionratepopulation" "perceivedevasionrate" to see how fairness (c_1)
  ### depends on these perceived evasion rates. 
  
  dependent <- "tax.rate.threshold.max"
  covariates <- c("calcage","gender","income.val.5","perceivedtaxrate",
                  "everaudited","spouseaudit",
                  "prop.alters.tTaxes","altertaxaudit.tTaxes",
                  "selfemployed","alterselfemployed",
                  "actor","servicestaxes","perceivedevasionratepopulation",
                  "perceivedevasionrate",
                  "perceivedauditrate","perceivedpenaltyrate")
  
  lab<- "Linear Regression of the c1 threshold"
  gillo<-ALP.glm(df,dependent,covariates,
                 threshold = 0.05,lab=lab,
                 survey.output.dir=survey.output.dir)
  
  saveRDS(gillo,file=paste(survey.output.dir,"Reg_tax.rate.threshold.max",".Rdata",sep=""))
  
  reg.tax.rate.threshold.max <- round(predict(gillo,newdata=model.pop.attributes,type="response"),0)
  
  #### Generated distribution of c1 
  
  tab.label<- paste("ALP_c1_fit_corrected","all",sep="_")
  
  
  for(type.c1.fit in rev(names(model.param.tab$c1))[-1] ){
    c1.dist <- model.param.tab$c1[[type.c1.fit]]
    
    n<- nrow(model.pop.attributes)
    sampled.c1<-sample(c1.dist$x,n, replace = T, prob=c1.dist$y)
    summary(sampled.c1)
    cor(sampled.c1,model.pop.attributes$perceivedtaxrate) ## cor should be ~ 0
    
    #### Rearrange the distibution by assigning this c1 according to the 
    ### regression model - this will now include the correlations with the 
    ### significant covariates
    
    boh <- ALP.match.cor(model.pop.attributes$perceivedtaxrate,sampled.c1,rho=0.08762806,verbose=F)
    model.pop.attributes[,type.c1.fit]<-round(boh$y,0)/100

    for(k in c("perceivedtaxrate","Income")){
      print(cor(model.pop.attributes[,type.c1.fit],model.pop.attributes[,k]))
    }
    
    fields.to.keep<- c(fields.to.keep,type.c1.fit)
  }
  
  
  ########################################
  ###                                  ###
  ### Regression 4: find how  m
  ### depends on age, income.cat 
  ########################################
  
  dependent <- "m"
  covariates <- c("calcage","gender","income.val.5","perceivedtaxrate",
                  "everaudited","spouseaudit",
                  "prop.alters.tTaxes","altertaxaudit.tTaxes",
                  "selfemployed","alterselfemployed",
                  "actor","servicestaxes","perceivedevasionratepopulation",
                  #"perceivedevasionrate",
                  "perceivedauditrate") 
  ## note removed ,"perceivedpenaltyrate" as it is colinear with "perceivedauditrate"
  
  lab<- "Linear Regression of the m value"
  gillo<-ALP.glm(df,dependent,covariates,
                 threshold = 0.00,lab=lab,
                 survey.output.dir=survey.output.dir,texreg.digits=10)
  
  saveRDS(gillo,file=paste(survey.output.dir,"Reg_m_value.max",".Rdata",sep=""))
  
  reg.m <- predict(gillo,newdata=model.pop.attributes,type="response")
  df$m <- predict(gillo,newdata=df,type="response")
  
  #### Generated distribution of m
  qP.parameters<- model.param.tab$qP$qP.summary
  
  n<- nrow(model.pop.attributes)
  sampled.m <- rpert(n, x.min=as.numeric(qP.parameters["m","1st Qu."]), 
            x.max= as.numeric(qP.parameters["m","3rd Qu."]), 
            x.mode=as.numeric(qP.parameters["m","Median"]))
  
  for(k in c("Income","perceivedauditrate","perceivedpenaltyrate")){
  print(cor(sampled.m,model.pop.attributes[,k]))
  }
  
  #### Rearrange the distibution by assigning m according to the regression
  sampled.m <- ALP.get.rearranged.sampled.dist(sampled.m, reg.m)
  model.pop.attributes$m <- sampled.m
  
  for(k in c("Income","perceivedauditrate","perceivedpenaltyrate")){
  print(cor(model.pop.attributes$m,model.pop.attributes[,k]) )## cor is no longer 0. 
  }
  
  fields.to.keep<- c(fields.to.keep,"m")
  
  ########################################
  ###                                  ###
  ### Regression 5: find how  s
  ### depends on age, income.cat 
  ########################################
  
  
  dependent <- "s"
  covariates <- c("calcage","gender","income.val.5","perceivedtaxrate",
                  "everaudited","spouseaudit",
                  "prop.alters.tTaxes","altertaxaudit.tTaxes",
                  "selfemployed","alterselfemployed",
                  "actor","servicestaxes","perceivedevasionratepopulation",
                  "perceivedevasionrate",
                  "perceivedauditrate","m")
  lab<- "Linear Regression of the s value"
  gillo<-ALP.glm(df,dependent,covariates,
                 threshold = 0.00,lab=lab,
                 survey.output.dir=survey.output.dir,texreg.digits=10)
  
  saveRDS(gillo,file=paste(survey.output.dir,"Reg_s_value.max",".Rdata",sep=""))
  
  reg.s <- predict(gillo,newdata=model.pop.attributes,type="response")
  
  #### Generated distribution of m
  qP.parameters<- model.param.tab$qP$qP.summary
  
  n<- nrow(model.pop.attributes)
  sampled.s <- rpert(n, x.min=as.numeric(qP.parameters["s","1st Qu."]), 
                     x.max= as.numeric(qP.parameters["s","3rd Qu."]), 
                     x.mode=as.numeric(qP.parameters["s","Median"]))
  
  for(k in c("Income","perceivedauditrate","perceivedpenaltyrate")){
    print(cor(sampled.s,model.pop.attributes[,k]))
  }
  
  #### Rearrange the distibution by assigning m according to the regression
  sampled.s <- ALP.get.rearranged.sampled.dist(sampled.s, reg.s)
  model.pop.attributes$s <- sampled.s
  
  for(k in c("Income","perceivedauditrate","perceivedpenaltyrate")){
    print(cor(model.pop.attributes$s,model.pop.attributes[,k]) )## cor is no longer 0. 
  }
  
  fields.to.keep<- c(fields.to.keep,"s")
  
  
  ########################################
  ###                                  ###
  ### Write to File
  ########################################
  
  fields.to.keep<- unique(fields.to.keep)
  
  model.pop.attributes <- model.pop.attributes[,fields.to.keep]
  
  print(head(model.pop.attributes))
  
  saveRDS(model.pop.attributes,file=paste(network.data.dir,network.data.file,
                                         ".demog.person.RData",sep=""))
  
  
  # colnames(model.pop.attributes)<-tolower(colnames(model.pop.attributes))
  # model.pop.attributes$tax.ids <- 1:nrow(model.pop.attributes)
  # colnames(model.pop.attributes)[match(c("perceivedauditrate","perceivedpenaltyrate"),
  #                                      colnames(model.pop.attributes))]<- 
  #   c("per.audit.rate","per.penalty.rate")
  
  
  ########################################
  ###                                  ###
  ### Prepare Population for input to the ABM 
  ###
  ########################################
  
  pop.data<-model.pop.attributes
  names(pop.data) <- tolower(names(pop.data))
  pop.data[, 'tax.ids'] <- 1:nrow(pop.data)

  pop.data[, 'tax.rate'] <- sapply(pop.data[, "tax.ids"], function(id){
    inc.f.stat <- pop.data[id, c("income", "filing.status")]
    sub.set <- eff.tax.rate[eff.tax.rate$filing.status == inc.f.stat$filing.status, ]
    interval.row <- findInterval(inc.f.stat$income, sub.set$min, rightmost.closed = T)
    sub.set[interval.row, 'tax.rate']
  })
  

  i <- which(names(pop.data) == "perceivedauditrate")
  names(pop.data)[i] <- "per.audit.rate"
  
  i <- which(names(pop.data) == "perceivedpenaltyrate")
  names(pop.data)[i] <- "per.penalty.rate"
  
  write.csv(pop.data, 
        paste(model.inputs.dir,network.data.file,"_population_data",".csv",sep=""), 
        row.names = FALSE)
  
  
  netw.data <- readRDS(file=paste(network.data.dir,network.data.file,
                                            ".contact.RData",sep=""))
  
  netw.data[, 'id1'] <- match(netw.data$Person.Id.1, pop.data$person.id)
  netw.data[, 'id2'] <- match(netw.data$Person.Id.2, pop.data$person.id)

  write.csv(netw.data, 
            paste(model.inputs.dir,network.data.file,"_network_data",".csv",sep=""), 
            row.names = F)
}
