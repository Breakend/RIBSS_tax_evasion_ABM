
library(xtable)
library(yacca)
library(ggplot2)
library(reshape2)
require(Hmisc)
library(foreign)
library(corrgram)
library(xlsx)

options(warn=2) 


## Specify directories 
survey.data.dir <- "Survey data/"
survey.output.dir <- "SurveyTablesPlots/"
library.dir   <- "Survey code/SurveyLibrary/"
library <- file.path(library.dir, "library.R")
source(library)


#ALP Tax Evasion Survey
ALP.data.file.name <- "Well_Being_4562016_09_02_10_54_58"
SurveyData <- read.csv(paste(survey.data.dir,ALP.data.file.name,".csv",sep=""),stringsAsFactors = FALSE)
#str(SurveyData)
#View(SurveyData)
#head(SurveyData)


#Check the number of incomplete surveys
#(t = as.data.frame(table(SurveyData$tsend)))

#Dataset with the completed surveys only 
CompletedData <- SurveyData[(SurveyData$tsend != ""),]

dim(CompletedData)


########################################
###                                  ###
###     Create Ego Data
###                                  ###
########################################

IDs <- SurveyData$prim_key

ego.data <- SurveyData[,c("prim_key","calcage","gender","familyincome","familyincome_part2","highesteducation","selfemployed","everaudited","spouseaudit","preptaxes","everfiledtaxes","workforpay","currentlivingsituation")]


tab.label<- "Ego_summary_age"
out<-summaryfunctionFull(ego.data$calcage)
print(xtable(out,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=TRUE, include.colnames=T)


####################
####################


tab.label<- "Ego_summary_gender"

gender<-as.character(ego.data$gender)
gender[gender=="Male"] <- 1
gender[gender=="Female"] <- 2
gender<- as.numeric(gender)

out <- ALP.fn.get.perc.of.respondents(gender,levels= c("Male","Female"),label = "gender")
tab.label<- "Ego_summary_gender"
print( xtable(out,auto=T,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)



tmp<-as.factor(ego.data$gender)
levels(tmp)<-c("Male","Female")
ego.data$gender <- tmp

####################
####################




tab.label<- "Ego_summary_income"
tmp.inc<-ALP.combineIncome(ego.data$familyincome,ego.data$familyincome_part2)
out <- 100*table(tmp.inc,useNA = "ifany")/ length(tmp.inc)
#out <- as.table(out[c(15,11,1,4,5,7,8,9,10,12,13,14,2,3,6)])
out <- as.data.frame(out)
colnames(out)<- c("income category","percentage")
out$`income category`<- as.character(out$`income category`)
out$`income category`[16] <- "Missing"
print( xtable(out,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)

ego.data$income.cat <- tmp.inc


ego.data$income.val <- ALP.combineIncome(ego.data$familyincome,ego.data$familyincome_part2,categorical =FALSE)


# 1 Less than 1st grade
# 2 1st,2nd,3rd,or 4th grade
# 3 5th or 6th grade
# 4 7th or 8th grade
# 5 9th grade
# 6 10th grade
# 7 11th grade
# 8 12th grade NO DIPLOMA
# 9 HIGH SCHOOL GRADUATE high school DIPLOMA <br>or the equivalent (For example: GED)
# 10 Some college but no degree
# 11 Associate degree in college Occupational/vocational program
# 12 Associate degree in college Academic program
# 13 Bachelor's degree (For example: BA,AB,BS)
# 14 Master's degree (For example: MA,MS,MEng,MEd,MSW,MBA)
# 15 Professional School Degree (For example: MD,DDS,DVM,LLB,JD)
# 16 Doctorate degree (For example: PhD,EdD)

## So 1:8= "No high school diploma", 9= "High school diploma",  10:12="Associate degree",
## 13= "Bachelor's degree", 14:15 = "Graduate degree"

educ <- rep(NA,length(ego.data$highesteducation))

educ <- 5
educ[ego.data$highesteducation==13] <- 4
educ[ego.data$highesteducation<13] <- 3
educ[ego.data$highesteducation==9] <- 2
educ[ego.data$highesteducation<9] <- 1

out <- ALP.fn.get.perc.of.respondents(educ,levels= c("No high school diploma", 
                                                     "High school diploma", 
                                                     "Associate degree", 
                                                     "Bachelor's degree", 
                                                     "Graduate degree"),label = "education level")
tab.label<- "Ego_summary_educ"
print( xtable(out,auto=T,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)


ego.data$educ <- "Graduate degree"
ego.data$educ[ego.data$highesteducation==13] <- "Bachelor's degree"
ego.data$educ[ego.data$highesteducation<13] <- "Associate degree"
ego.data$educ[ego.data$highesteducation==9] <- "High school diploma"
ego.data$educ[ego.data$highesteducation<9] <- "No high school diploma"


############################################################




# EverAudited EverAudited
# Have you ever been audited by the IRS?
# 1 Yes
# 2 No
# 3 Don't know or can't remember
# 4 Prefer not to say

#######################################

out <-ALP.fn.get.perc.of.respondents(ego.data$everaudited, levels=c("Yes","No","DK/DR","Pref. not. say"), label = "ever audited")
tab.label<- "Ego_summary_audit"
print( xtable(out,auto=T,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)

##########################################


# | SpouseAudit SpouseAudit
# | Has your spouse or domestic partner ever been audited by the IRS at any time
# | during the past five years?
# | 1 I am not currently married or living with a domestic partner
# | 2 Yes
# | 3 No
# | 4 Don't know or can't remember
# | 5 Prefer not to answer

##########################################################

received.question<- (ego.data$currentlivingsituation==1) ## those Married or living with a partner THEN

s.e.a <-ALP.fn.get.perc.of.respondents(ego.data$spouseaudit, 
                                  levels=c("I am not currently married or living with a domestic partner",
                                           "Yes","No",
                                           "DK/DR","Prefer not to answer"),
                                  received.question= received.question,
                                  label = "spouse ever audited")

tab.label<- "Ego_summary_spouseaudit"
print( xtable(s.e.a,auto=T,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)


#Combined Ever audited including spouse
######################################


received.question<- (ego.data$currentlivingsituation==1)

ego.data$hheveraudited <- ego.data$everaudited==1

tmp<-!(is.na(ego.data$spouseaudit))
ego.data$hheveraudited[tmp] <- ((ego.data$everaudited==1 | ego.data$spouseaudit==2))[tmp]

tmp <-  ego.data$everaudited>2 & (ego.data$spouseaudit>3)
ego.data$hheveraudited[tmp] <- NA 

tmp <- ego.data$hheveraudited

tmp <- -as.numeric(tmp)+2
tmp[is.na(tmp)] <- 3

out <- ALP.fn.get.perc.of.respondents(tmp, levels=c("Yes","No","DK/Pref. not. say /NA"),label="ever audited including spouse")

tab.label<- "Ego_summary_hheveraudited"
print( xtable(out,auto=T,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)



tmp <- NA*ego.data$everaudited
tmp[ego.data$everaudited==1] <- TRUE
tmp[ego.data$everaudited==2] <- FALSE
ego.data$everaudited <- as.logical(tmp)

tmp <- NA*ego.data$spouseaudit
tmp[ego.data$spouseaudit==2] <- TRUE
tmp[ego.data$spouseaudit==3] <- FALSE
ego.data$spouseaudit <- as.logical(tmp)


# | SelfEmployed SelfEmployed
# | Do you work for someone else, are you self-employed, or what?
# | 1 Work for someone else
# | 2 Self-employed
# | 3 Other

##########################

received.question<- (SurveyData$currentjobstatuss1==1) 

se.dat <-ALP.fn.get.perc.of.respondents(ego.data$selfemployed, 
                                       levels=c("Work for someone else","Self-employed","Other"),
                                       received.question= received.question,
                                       label = "self-employed")

tab.label<- "Ego_summary_selfemployed"
print( xtable(se.dat,auto=T,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)


tmp <- ego.data$selfemployed
ego.data$selfemployed<- NA
ego.data$selfemployed[tmp==1] <- FALSE#"No"
ego.data$selfemployed[tmp==2] <- TRUE#"Yes"
ego.data$selfemployed[tmp==3] <- FALSE#"Other"



# PrepTaxes PrepTaxes
# Do you typically prepare your own tax returns or do you pay someone (e.g., an
#                                                                      accountant or lawyer) to prepare them for you?
# 1 I prepare my own tax returns using tax software on the computer
# 2 I prepare my own tax returns, without using tax software
# 3 I pay someone else to prepare my tax returns
# 4 I do not prepare a tax return
# 5 I don't know or would prefer not to say

####################################
out <-ALP.fn.get.perc.of.respondents(ego.data$preptaxes, levels=c("I prepare my own tax returns using tax software on the computer","I prepare my own tax returns, without using tax software","I pay someone else to prepare my tax returns","I do not prepare a tax return","I don't know or would prefer not to say"),label="method of preparing taxes")
tab.label<- "Ego_summary_preptaxes"
print( xtable(out,auto=1,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)



tmp <- ego.data$preptaxes
ego.data$preptaxes<- "DK/PNS"
ego.data$preptaxes[tmp==1] <- "Yes using tax software"
ego.data$preptaxes[tmp==2] <- "Yes without using tax software"
ego.data$preptaxes[tmp==3] <- "Yes with tax expert"
ego.data$preptaxes[tmp==4] <- "No tax return"


# EverFiledTaxes EverFiledTaxes
# Have you ever filed a tax return yourself or had someone file it for you?
# 1 Yes
# 2 No
# 3 Don't know or can't remember

#######################################
out <-ALP.fn.get.perc.of.respondents(ego.data$everfiledtaxes, levels=c("Yes","No","DK/DR"),label="ever filed taxes")
tab.label<- "Ego_summary_everfiledtaxes"
print( xtable(out,auto=T,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)


tmp <- ego.data$everfiledtaxes
ego.data$everfiledtaxes<- NA
ego.data$everfiledtaxes[tmp==1] <- TRUE
ego.data$everfiledtaxes[tmp==2] <- FALSE




# | WorkForPay WorkForPay
# | Have you ever worked for pay?
# | 1 Yes
# | 2 No

##########################################
currently.working<- (SurveyData$currentjobstatuss1==1) 

ego.data$workforpay[currently.working]<- 1 

out <-ALP.fn.get.perc.of.respondents(ego.data$workforpay, 
                                            levels=c("Yes","No"),
                                     label="ever worked for pay")
tab.label<- "Ego_summary_workforpay"
print( xtable(out,auto=T,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)


ego.data$workforpay <- as.logical(ego.data$workforpay)

ego.data<- ego.data[,!(colnames(ego.data)%in%c("familyincome","familyincome_part2","highesteducation"))]

write.csv(ego.data,file=paste(survey.data.dir,"ego.data.csv",sep=""),row.names = F)
saveRDS(ego.data,file=paste(survey.data.dir,"ego.data.Rdata",sep=""))



########################################
###                                  ###
###     Reorganize Data for          ###
###     Social Network questions     ###
###            Qus.1-7               ###
###                                  ###
########################################


df <- SurveyData[, 38:121]


empty_str = c(""," ","  ","    ")

### Create New more manageble tables of the alters data called alters.table.
### alters.table is a list containing (i) the number of alters (n.alters)
### (ii) the number of alters they talk about taxes (n.alters.tTaxes)
### (iii) table of the propotion of alters belonging to the different relationship clases (alters.rel)
### (iv) table of the propotion of alters belonging to the 
### different relationship clases with which they talk about taxes (alters.rel.tTaxes)
### (v) similarly with education, selfemployed and taxaudit
### (vi) a table of the proportion out of n.alters of how freqent you speak about taxes (altertaxhowoften)

max.num.alters.asked <- 10 
alters.question <- paste("alters_",1:max.num.alters.asked,"_",sep="") 
alters.rel <- paste("alterrel_",1:max.num.alters.asked,"_",sep="") 
altereduc <- paste("altereduc_",1:max.num.alters.asked,"_",sep="") 
altertalktax <- paste("altertalktax_",1:max.num.alters.asked,"_",sep="")
altertaxhowoften <- paste("taxhowoften_",1:max.num.alters.asked,"_",sep="")
alterselfemployed <- paste("taxselfemployed_",1:max.num.alters.asked,"_",sep="")
altertaxaudit <- paste("taxaudit_",1:max.num.alters.asked,"_",sep="")
df$n.alters<- 0

alters.table<- list()
for(i in 1:nrow(df)){
  n.alters<-0
  for(j in 1:max.num.alters.asked ){
    n.alters <- n.alters + as.numeric(!(df[i,alters.question[j]] %in%empty_str))
  }
  alters.table[["n.alters"]][[i]] <- n.alters 
  
  
#   AlterTalkTax_intro alter talk tax intro
#   For each of the people listed below, please check the box next to any person
#   with whom you have talked or consulted with about taxes in the past 5 years.
#   This could include any aspect of taxes, including state or federal taxes, tax
#   audits or penalties, how fair taxes seem, or any other related topic.
#   IF Alters{1} != empty THEN
#   |
#   | AlterTalkTax alter talk tax
#   |
#   | 1 Yes
#   | 2 No
#   | 3 Don't know
#   | 4 I would prefer not to say


  tmp <- df[i,altertalktax[1:n.alters]]
  tmp[tmp>2] <- NA
  tmp<- as.logical(2-tmp)
  talk.taxes <- tmp  ## convert the 2=False to 0= False
  
  
  n.alters.tTaxes <- sum(talk.taxes)
  alters.table[["n.alters.tTaxes"]][[i]]  <- n.alters.tTaxes
  
  
#   AlterRel alter relatives
#   |
#   | 1 Family member
#   | 2 Friend
#   | 3 Coworker
#   | 4 Other
  
  x.rel<-ALP.fn.get.proportion.of.alters( df[i,alters.rel[1:n.alters]],
                                      levels=c("Family member", 
                                               "Friend", "Coworker", "Other"),useNA ="always")
  alters.table[["alters.rel"]][[i]]<-x.rel
  
  
#   AlterEduc alter education
#   |
#   | 1 Less than a high school diploma or the equivalent (For example: GED)
#   | 2 High school diploma or the equivalent (For example: GED)
#   | 3 Associate degree in college
#   | 4 Bachelor's degree (For example: BA,AB,BS)
#   | 5 Graduate degree, such as Master's or Doctoral-level degree
  
  x.educ<-ALP.fn.get.proportion.of.alters( df[i,altereduc[1:n.alters]],
                                      levels=c("No high school diploma", 
                                               "High school diploma",  
                                               "Associate degree", 
                                               "Bachelor's degree", 
                                               "Graduate degree","DK"),useNA ="always")

  alters.table[["altereduc"]][[i]]<-x.educ
  if(all(is.nan(x.educ))) alters.table[["altereduc"]][[i]]<-NA
 
#   | TaxSelfEmployed tax self employed
#   |
#   | 1 Yes
#   | 2 No
#   | 3 I don't know or don't remember
   
  x.selfemployed<-ALP.fn.get.proportion.of.alters(df[i,alterselfemployed[1:n.alters]],
                                     levels=c("Yes", "No", "DK/DR"),
                                     useNA ="always")
  
  alters.table[["alterselfemployed"]][[i]]<-x.selfemployed
  if(all(is.nan(x.selfemployed))) alters.table[["alterselfemployed"]][[i]]<-NA
  
  
 
#   | TaxAudit tax audit
#   |
#   | 1 Yes, I know they have been audited
#   | 2 Yes, I think they have been audited
#   | 3 I don't know or don't rememeber
#   | 4 No, I don't think they have been audited
#   | 5 No, I know they have not been audited
  
  x.audit<-ALP.fn.get.proportion.of.alters(df[i,altertaxaudit[1:n.alters]],
                                     levels=c("Yes", "I think so", 
                                              "DK" , "I don't think so",  
                                              "No" ),useNA ="always")
  alters.table[["altertaxaudit"]][[i]]<-x.audit
  if(all(is.nan(x.audit))) alters.table[["altertaxaudit"]][[i]]<-NA
  
  
  tmp <- NA*df[i,altertaxaudit[1:n.alters]]
  ss<-(df[i,alterselfemployed[1:n.alters]] ==1 & 
         df[i,altertaxaudit[1:n.alters]] <= 2)
  tmp[ss] <- 1 # "SE_A"
  ss<-(df[i,alterselfemployed[1:n.alters]] ==1 &
         df[i,altertaxaudit[1:n.alters]] >3 )
  tmp[ss] <- 2 # "SE_nA"
  ss<-(df[i,alterselfemployed[1:n.alters]] ==2 &
         df[i,altertaxaudit[1:n.alters]] <= 2)
  tmp[ss] <- 3# "nSE_A"
  ss<-(df[i,alterselfemployed[1:n.alters]] ==2 &
         df[i,altertaxaudit[1:n.alters]] > 3)
  tmp[ss] <- 4 #"nSE_nA"
  
  ss <- (df[i,alterselfemployed[1:n.alters]] ==1 &
           df[i,altertaxaudit[1:n.alters]] == 3)
  tmp[ss]<- 5 ## SE_DK
  
  
  ss <- (df[i,alterselfemployed[1:n.alters]] ==2 &
           df[i,altertaxaudit[1:n.alters]] == 3)
  tmp[ss]<- 6 ## nSE_DK
  
  ss <- (df[i,alterselfemployed[1:n.alters]] ==3 &
           df[i,altertaxaudit[1:n.alters]] <= 2)
  tmp[ss]<- 7 ## DK_A
  ss<-(df[i,alterselfemployed[1:n.alters]] ==3 &
         df[i,altertaxaudit[1:n.alters]] >3 )
  tmp[ss] <- 8 # "DK_nA"
  ss <- (df[i,alterselfemployed[1:n.alters]] ==3 &
           df[i,altertaxaudit[1:n.alters]] == 3)
  tmp[ss]<- 9 ## DK
  
  
#   
#   data.frame(a=as.numeric(df[i,alterselfemployed[1:n.alters]]),
#                b=as.numeric(df[i,altertaxaudit[1:n.alters]]),
#                c=as.character(tmp))

  x.se.a<-ALP.fn.get.proportion.of.alters(tmp,
                                           levels=c(
                                             "SE_A","SE_nA","nSE_A","nSE_nA","SE_DK","nSE_DK",
                                             "DK_A","DK_nA","DK"
                                           ),useNA ="always")
  
  alters.table[["alter_SE_A"]][[i]]<-x.se.a
  if(all(is.nan(x.se.a))) alters.table[["alter_SE_A"]][[i]]<-NA
  
 
  
  if(!is.na(n.alters.tTaxes)){
    if(n.alters.tTaxes>0){  
      
      
      
#       | TaxHowOften tax how often
#       |
#       | 1 Once every five years
#       | 2 Once every two years
#       | 3 Once a year
#       | 4 Twice a year
#       | 5 Monthly
#       | 6 More frequently
#       | 7 I don't know or don't remember
      
      x.taxhowoften<-ALP.fn.get.proportion.of.alters(
        df[i,altertaxhowoften[1:n.alters][talk.taxes]],
        levels=c("Once every five years",
                 "Once every two years",
                 "Once a year", 
                 "Twice a year",
                 "Monthly",
                 "More frequently",
                 "DK"),useNA ="always")
      
      alters.table[["altertaxhowoften"]][[i]] <- x.taxhowoften#*n.alters.tTaxes/n.alters 
      
      x.rel<-ALP.fn.get.proportion.of.alters( df[i,alters.rel[1:n.alters][talk.taxes]],
                                          levels=c("Family member", 
                                                   "Friend", "Coworker", "Other"),useNA ="always")
      alters.table[["alters.rel.tTaxes"]][[i]]<-x.rel#*n.alters.tTaxes/n.alters
      
      x.educ<-ALP.fn.get.proportion.of.alters( df[i,altereduc[1:n.alters][talk.taxes]],
                                          levels=c("No high school diploma", 
                                                   "High school diploma",  
                                                   "Associate degree", 
                                                   "Bachelor's degree", 
                                                   "Graduate degree","DK"),useNA ="always")
      
      alters.table[["altereduc.tTaxes"]][[i]]<-x.educ#*n.alters.tTaxes/n.alters
      
      x.selfemployed<-ALP.fn.get.proportion.of.alters(df[i,
                                            alterselfemployed[1:n.alters][talk.taxes]],
                                         levels=c("Yes", "No", "DK/DR"),
                                         useNA ="always")
      
      alters.table[["alterselfemployed.tTaxes"]][[i]]<-x.selfemployed#*n.alters.tTaxes/n.alters 
      
      
      x.audit<-ALP.fn.get.proportion.of.alters(df[i,altertaxaudit[1:n.alters][talk.taxes]],
                                         levels=c("Yes", "I think so", 
                                                  "DK" , "I don't think so",  
                                                  "No" ),useNA ="always")
      
      alters.table[["altertaxaudit.tTaxes"]][[i]]<-x.audit#*n.alters.tTaxes/n.alters
      
      
      tmp <- NA*df[i,altertaxaudit[1:n.alters][talk.taxes]]
      ss<-(df[i,alterselfemployed[1:n.alters][talk.taxes]] ==1 & 
             df[i,altertaxaudit[1:n.alters][talk.taxes]] <= 2)
      tmp[ss] <- 1 # "SE_A"
      ss<-(df[i,alterselfemployed[1:n.alters][talk.taxes]] ==1 &
             df[i,altertaxaudit[1:n.alters][talk.taxes]] >3 )
      tmp[ss] <- 2 # "SE_nA"
      ss<-(df[i,alterselfemployed[1:n.alters][talk.taxes]] ==2 &
             df[i,altertaxaudit[1:n.alters][talk.taxes]] <= 2)
      tmp[ss] <- 3# "nSE_A"
      ss<-(df[i,alterselfemployed[1:n.alters][talk.taxes]] ==2 &
             df[i,altertaxaudit[1:n.alters][talk.taxes]] > 3)
      tmp[ss] <- 4 #"nSE_nA"
      ss <- (df[i,alterselfemployed[1:n.alters][talk.taxes]] ==1 &
               df[i,altertaxaudit[1:n.alters][talk.taxes]] == 3)
      tmp[ss]<- 5 ## SE_DK
      
      
      ss <- (df[i,alterselfemployed[1:n.alters][talk.taxes]] ==2 &
               df[i,altertaxaudit[1:n.alters][talk.taxes]] == 3)
      tmp[ss]<- 6 ## nSE_DK
      
      ss <- (df[i,alterselfemployed[1:n.alters][talk.taxes]] ==3 &
               df[i,altertaxaudit[1:n.alters][talk.taxes]] <= 2)
      tmp[ss]<- 7 ## DK_A
      ss<-(df[i,alterselfemployed[1:n.alters][talk.taxes]] ==3 &
             df[i,altertaxaudit[1:n.alters][talk.taxes]] >3 )
      tmp[ss] <- 8 # "DK_nA"
      ss <- (df[i,alterselfemployed[1:n.alters][talk.taxes]] ==3 &
               df[i,altertaxaudit[1:n.alters][talk.taxes]] == 3)
      tmp[ss]<- 9 ## DK
      
      
      
      x.se.a<-ALP.fn.get.proportion.of.alters(tmp,
                                              levels=c(
                                                "SE_A","SE_nA","nSE_A","nSE_nA","SE_DK","nSE_DK",
                                                "DK_A","DK_nA","DK"
                                              ),useNA ="always")
      
      alters.table[["alter_SE_A.tTaxes"]][[i]]<-x.se.a#*n.alters.tTaxes/n.alters
    }}
  if( is.na(n.alters.tTaxes)){
    alters.table[["altertaxhowoften"]][[i]]<-rep(NA,8)
    alters.table[["alters.rel.tTaxes"]][[i]]<-rep(NA,5)
    alters.table[["altereduc.tTaxes"]][[i]]<-rep(NA,7)
    alters.table[["alterselfemployed.tTaxes"]][[i]]<-rep(NA,4)
    alters.table[["altertaxaudit.tTaxes"]][[i]]<-rep(NA,6)
    alters.table[["alter_SE_A.tTaxes"]][[i]] <- rep(NA)
  }
  if(!is.na(n.alters.tTaxes)){if( n.alters.tTaxes==0){
    alters.table[["altertaxhowoften"]][[i]]<-rep(0,8)
    alters.table[["alters.rel.tTaxes"]][[i]]<-0*alters.table[["alters.rel"]][[i]]
    alters.table[["altereduc.tTaxes"]][[i]]<-0*alters.table[["altereduc"]][[i]]
    alters.table[["alterselfemployed.tTaxes"]][[i]]<-0*
      alters.table[["alterselfemployed"]][[i]]
    alters.table[["altertaxaudit.tTaxes"]][[i]]<-0*
      alters.table[["altertaxaudit"]][[i]]
    alters.table[["alter_SE_A.tTaxes"]][[i]]<-0*
      alters.table[["alter_SE_A"]][[i]]
  }}
}



table.names <- c("alters.rel","alters.rel.tTaxes","altereduc","altereduc.tTaxes","alterselfemployed","alter_SE_A","alterselfemployed.tTaxes","altertaxaudit","altertaxaudit.tTaxes","alter_SE_A.tTaxes","altertaxhowoften")


tmp <- alters.table
alters.table<-list()
for(tt in table.names){
  print(tt)
  binded.tab <- 100*as.data.frame(do.call("rbind",tmp[[tt]]))
  binded.tab <- cbind(prim_key= IDs, 
                      n.alters=tmp[["n.alters"]],
                      n.alters.tTaxes=tmp[["n.alters.tTaxes"]],
                      binded.tab)
  alters.table[[tt]]<-binded.tab
  
  write.csv(alters.table[[tt]],file=paste(survey.data.dir,"EgoNet_",tt,".csv",sep=""),row.names = F)
  
  
  #Sanity check.
  #print(mean(tmp[,2]/tmp[,1],na.rm=T))
  #print(sum(colMeans(tmp[,-c(1,2)],na.rm=T)))
}



table.names2 <- table.names[!grepl(".tTaxes",table.names)] 
tt <-"alters.rel" 
for(tt in rev(table.names2)[-1]){

  tab.label<- paste("Alter_prepare_table_",tt,sep="")
  
  x <- alters.table[[tt]]
  select <- (rowSums(x[,-c(1:3)])==100)
  x <- colMeans(x[select,-c(1,3)],na.rm=T)
  y <- alters.table[[paste(tt,".tTaxes",sep="")]]
  tmp <- mean(y[,3],na.rm=T)
  select <- (rowSums(y[,-c(1:3)])==100)
  y <- colMeans(y[select,-c(1,2)],na.rm=T)
  y[1]<-tmp
  
  out <- data.frame(rbind(n.alters=x,n.alters.tTaxes=y))
  rownames(out)<- c("All","Talk about taxes")
  out <- cbind(rownames(out),out)
  names(out)<-c("Alters",names(x))
  out <- xtable(out,digits=1)
  #align(out)<-"lp{2in}p{3in}p{1in}" #here is the change
  vv.c <-nchar(names(out))>12
  vv.c <- c(F,vv.c)
  vv<- align(out)[vv.c]
  vv[vv %in% "r"]<- "L{2cm}"
  align(out)[vv.c]<- vv
  
  print( out, file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F)
}
  

saveRDS(alters.table,file=paste(survey.data.dir,"EgoNet",".Rdata",sep=""))


########################################
###                                  ###
###     Merge some Alter data into the  Ego Data
###                                  ###
########################################



tmp <- alters.table$alter_SE_A.tTaxes
tmp[,4:9]<-tmp[,4:9]/100 

ego.data<- merge(ego.data,tmp[,1:9], by = "prim_key")


tmp <- alters.table$altertaxaudit.tTaxes
tmp[,4:9]<-tmp[,4:9]/100 
tmp <- tmp[,4]+tmp[,5]

ego.data$altertaxaudit.tTaxes <- tmp

tmp <- alters.table$alterselfemployed
tmp[,4:6]<-tmp[,4:6]/100 
tmp <- tmp[,4]

ego.data$alterselfemployed <- tmp



write.csv(ego.data,file=paste(survey.data.dir,"ego.data.csv",sep=""),row.names = F)
saveRDS(ego.data,file=paste(survey.data.dir,"ego.data.Rdata",sep=""))

print(xtable(ego.data[1:55,]),file=paste(survey.output.dir,
                                        "ego_data",".tex",sep=""),
      include.rownames=FALSE)







