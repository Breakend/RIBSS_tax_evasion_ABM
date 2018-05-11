#ALP Tax Evasion Survey
#SurveyData <- read.csv("Survey data/Well_Being_4562016_09_02_10_54_58.csv")
#output.dir <- "SurveyTablesPlots/"
SurveyData <- read.csv("/Users/galiyev/Documents/OJT/Tax evasion/Survey data/Well_Being_4562016_09_02_10_54_58.csv")
#str(SurveyData)
#View(SurveyData)

theme_set(theme_bw(12))

setwd("/Users/galiyev/Documents/OJT/Tax evasion/Survey data")
output.dir <- "Output/"

library(xtable)
library(yacca)
library(ggplot2)
library(reshape2)
require(Hmisc)
library(foreign)
library(psych)


#Check the number of incomplete surveys
(t = as.data.frame(table(SurveyData$tsend)))


#Dataset with the completed surveys only 
CompletedData <- SurveyData[(SurveyData$tsend != ""),]


#Number and percent of respondent who are born in the USA
SurveyData$borninus <- factor(SurveyData$borninus, levels = c(1,2), labels = c("Yes", "No"))
(Tbl_borninus <- table(SurveyData$borninus, exclude = NULL))
prop.table(Tbl_borninus)

#Number and percent of respondent who are born in the USA and completed the survey
CompletedData$borninus <- factor(CompletedData$borninus, levels = c(1,2), labels = c("Yes", "No"))
(Tbl_borninus_Completed <- table(CompletedData$borninus, exclude = NULL))
prop.table(Tbl_borninus_Completed)


### REVISE THIS PART
summary(SurveyData)
summary(SurveyData$currentjobstatuss)
SurveyData$currentjobstatuss <- 1
SurveyData$currentjobstatuss[(SurveyData$currentjobstatuss2 == 2)] <- 2

summary(SurveyData$perceivedauditrate)

summary(SurveyData$perceivedauditratemagnifier)
##########REVISE THE PART ABOVE

#???RAFF, DO YOU WANT ME TO ESTIMATE MODE????
#CONSIDER rbind(cbind(a,b),cbind(c,d)) FOR SUMMARY OUTPUTS. FIRST CBIND IS FOR SUMMARY, SECOND IS FOR SD.ASK STEVE.
a <- summary(SurveyData$perceivedpenaltyrate)
b <- sd(SurveyData$perceivedpenaltyrate, na.rm = TRUE)
b <- round(b,digits=1)
X <- (cbind(c(names(a),"SD"), c(a,b)))
X[include.rownames = F]
xtable(X, caption = "Descriptive Statistics for Penalty-rate Perception")

###################

#Section 4: Penalty-Rate Perception
a<-round(describe(SurveyData$perceivedpenaltyrate), digits =1)
X<- t(a)
xtable(X, caption = "Descriptive Statistics for Penalty-rate Perception", digits =1)

text.label<- summary(SurveyData$perceivedpenaltyrate)
text.label<-paste(c(paste(names(text.label),text.label, sep="=")),collapse="   ")

p.histogram <- ggplot(data = SurveyData) + 
  geom_histogram(aes(x=perceivedpenaltyrate),alpha=1, color="black", fill = "red", bins=100) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived Penalty for Unpaid Taxes of $1000 (in $)") +
  ylab("count") +
  ggtitle("Histogram for Perceived Penalty Rate (PerceivedPenaltyRate)") +
  xlim(c(0,max(SurveyData$perceivedpenaltyrate))) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))+
  annotate("text", label = "Descriptive Statistics",x=9600,y=405, size = 5, colour = "black",angle = 0)+
  annotate("text", label = text.label,x=9600,y=380, size = 5, colour = "black",angle = 0)
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedPenaltyRate",".pdf",sep=""),  width = 12, height= 8)


#Section 5: Tax-rate Perception
a<-round(describe(SurveyData$perceivedtaxrate), digits =1)
X<- t(a)
xtable(X, caption = "Descriptive Statistics for Tax-rate Perception", digits =1)


text.label <- summary(SurveyData$perceivedtaxrate)
text.label <-paste(c(paste(names(text.label),text.label, sep="=")),collapse="   ")

sd(SurveyData$perceivedtaxrate, na.rm = TRUE)

p.histogram <- ggplot(data = SurveyData) + 
  geom_histogram(aes(x=perceivedtaxrate),alpha=1, color="black", fill = "blue", bins=20) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived Effective Tax Rate (%)") +
  ylab("count") +
  ggtitle("Histogram for Perceived Tax Rate (PerceivedTaxRate)") +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1)) +
  annotate("text", label = "Descriptive Statistics",x=68,y=186, size = 5, colour = "black",angle = 0)+
  annotate("text", label = text.label,x=68,y=176, size = 5, colour = "black",angle = 0)
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedTaxRate",".pdf",sep=""),  width=12, height=8)


#Section 6: Tax Evasion Perception
#Perceived Evasion Rate for Population
str(SurveyData$perceivedevasionratepopulation)
SurveyData$perceivedevasionratepopulation[SurveyData$perceivedevasionratepopulation == "00-"] <- 0
SurveyData$perceivedevasionratepopulation.num<-as.numeric(as.character(SurveyData$perceivedevasionratepopulation)) #converting variable saved as factor into numeric variable

a<-round(describe(SurveyData$perceivedevasionratepopulation.num), digits =1)
X<- t(a)
xtable(X, caption = "Descriptive Statistics for Perceived Population Tax Evasion Rate", digits =1)


text.label <- summary(SurveyData$perceivedevasionratepopulation.num)
text.label <-paste(c(paste(names(text.label),text.label, sep="=")),collapse="   ")

p.histogram <- ggplot(data = SurveyData) + 
  geom_histogram(aes(x=perceivedevasionratepopulation.num),alpha=1, color="black", fill = "orange", bins=20) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived Population Tax Evasion Rate (%)") +
  ylab("count") +
  ggtitle("Histogram for Perceived Population Tax Evasion Rate (PerceivedEvasionRatePopulation)") +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1)) +
  annotate("text", label = "Descriptive Statistics",x=68,y=186, size = 5, colour = "black",angle = 0)+
  annotate("text", label = text.label,x=68,y=176, size = 5, colour = "black",angle = 0)
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedEvasionRatePopulation",".pdf",sep=""),  width=12, height=8)


#Perceived Evasion Rate (out of 100 people like you)
a<-round(describe(SurveyData$perceivedevasionrate), digits =1)
X<- t(a)
xtable(X, caption = "Descriptive Statistics for Perceived Tax Evasion Rate (out of 100 people like you)", digits =1)


text.label <- summary(SurveyData$perceivedevasionrate)
text.label <- paste(c(paste(names(text.label),text.label, sep="=")),collapse="   ")

summary(SurveyData$perceivedevasionrate)
sd(SurveyData$perceivedevasionrate, na.rm = TRUE)
p.histogram <- ggplot(data = SurveyData) + 
  geom_histogram(aes(x=perceivedevasionrate),alpha=1, color="black", fill = "chartreuse4", bins=20) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived Tax Evasion Rate (%)") +
  ylab("count") +
  ggtitle("Histogram for Perceived Tax Evasion Rate (out of 100 people like you)") +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1)) +
  annotate("text", label = "Descriptive Statistics",x=57,y=215, size = 5, colour = "black",angle = 0)+
  annotate("text", label = text.label,x=57,y=200, size = 5, colour = "black",angle = 0)
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedEvasionRate",".pdf",sep=""),  width=9, height=6)


#Perceived Evasion Rate When There Are Many Evaders
a<-round(describe(SurveyData$perceivedevasionmanyevaders), digits =1)
X<- t(a)
xtable(X, caption = "Descriptive Statistics for Perceived Tax Evasion Rate (when half of all US taxpayers evade taxes)", digits =1)


text.label <- summary(SurveyData$perceivedevasionmanyevaders)
text.label <- paste(c(paste(names(text.label),text.label, sep="=")),collapse="   ")


summary(SurveyData$perceivedevasionmanyevaders)
sd(SurveyData$perceivedevasionmanyevaders, na.rm = TRUE)
p.histogram <- ggplot(data = SurveyData) + 
  geom_histogram(aes(x=perceivedevasionmanyevaders),alpha=1, color="black", fill = "seagreen", bins=20) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived Tax Evasion Rate (%)") +
  ylab("count") +
  ggtitle("Histogram for Perceived Tax Evasion Rate When There Are Many Evaders") +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1)) +
  annotate("text", label = "Descriptive Statistics",x=68,y=225, size = 5, colour = "black",angle = 0)+
  annotate("text", label = text.label,x=68,y=210, size = 5, colour = "black",angle = 0)
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedEvasionManyEvaders",".pdf",sep=""),  width=12, height=8)


#Perceived % of Evaders That Get Caught
summary(SurveyData$perceivedcaught)
sd(SurveyData$perceivedcaught, na.rm = TRUE)
p.histogram <- ggplot(data = SurveyData) + 
  geom_histogram(aes(x=perceivedcaught), alpha=1, color="red4", fill = "red4", bins=20) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived Percentage of Evaders That Get Caught (%)") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived Percentage of Evaders That Get Caught", 
         atop("Question: In a typical year, what percent of these people [the US taxpayers who intentionally underreport on their taxes] will be caught by the IRS?"), 
         sep=""))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedCaught",".pdf",sep=""),  width=11, height=7)


#Correlation between Perceived % of Caught Tax Evaders and Perceived Evasion Rate for Population
cor.test(SurveyData$perceivedcaught, SurveyData$perceivedevasionratepopulation.num, alternative = "two.sided", method = "pearson", conf.level = 0.95)

#????CHECK IF CONDITION DATA ARE CORRECT?????

################################### CONDITION A #############################################
#CONDITION A: Perceived Behavioral Reaction to INCREASED Tax Rate
Cond.A <- SurveyData[(SurveyData$perceivedevasionrate == 0 | SurveyData$behavreactionrandom == 1) & SurveyData$perceivedevasionrate != 0,]
View(Cond.A)

#Perceived Underreporting When Tax Rates are 50% Higher
str(Cond.A$perceivedunderreporttaxhigher) #??????ASK TIM ABOUT "00" AND "0O"?????
Cond.A$perceivedunderreporttaxhigher[Cond.A$perceivedunderreporttaxhigher == "0o"] <- 0
Cond.A$perceivedunderreporttaxhigher.num<-as.numeric(as.character(Cond.A$perceivedunderreporttaxhigher)) #converting variable saved as factor into numeric variable

summary(Cond.A$perceivedunderreporttaxhigher.num)
sd(Cond.A$perceivedunderreporttaxhigher.num, na.rm = TRUE)
p.histogram <- ggplot(data = Cond.A) + 
  geom_histogram(aes(x=perceivedunderreporttaxhigher.num), alpha=1, color="red1", fill = "red1", bins=20) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When Tax Rates are 50% Higher", 
         atop("Question: Imagine instead that people's effective income tax rates were 50% higher than they currently are. Out of 100 US taxpayers like you, who continue to work just as many hours, how many do you think would intentionally underreport their taxes to the IRS?"), 
         sep=""))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportTaxHigher",".pdf",sep=""),  width=18.5, height=11)



#Perceived Underreporting When Tax Rates are Much Higher (Twice As High)
summary(Cond.A$perceivedunderreporttaxmuchhigher)
sd(Cond.A$perceivedunderreporttaxmuchhigher, na.rm = TRUE)
p.histogram <- ggplot(data = Cond.A) + 
  geom_histogram(aes(x=perceivedunderreporttaxmuchhigher), alpha=1, color="seagreen3", fill = "seagreen3", bins=21) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When Tax Rates are Twice as High", 
         atop("Question: Imagine instead that people’s effective income tax rates were twice as high as they currently are. Out of 100 US taxpayers like you, who continue to work just as many hours, how many do you think would intentionally underreport their taxes to the IRS?"), 
         sep=""))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportTaxMuchHigher",".pdf",sep=""),  width=18.5, height=11)



#Correlation between perceived underreporting when tax rates are 50% higher and twice as high
cor.test(Cond.A$perceivedunderreporttaxmuchhigher, Cond.A$perceivedunderreporttaxhigher.num, alternative = "two.sided", method = "pearson", conf.level = 0.95)


#????CHECK IF CONDITION DATA ARE CORRECT?????

################################### CONDITION B #############################################
#CONDITION B: Perceived Behavioral Reaction to DECREASED Tax Rate
Cond.B <- SurveyData[(SurveyData$perceivedevasionrate == 100 | SurveyData$behavreactionrandom == 2),]
View(Cond.B)

#Perceived Underreporting When Tax Rates are 25% Lower
summary(Cond.B$perceivedunderreporttaxlower)
sd(Cond.B$perceivedunderreporttaxlower, na.rm = TRUE)
p.histogram <- ggplot(data = Cond.B) + 
  geom_histogram(aes(x=perceivedunderreporttaxlower), alpha=1, color="paleturquoise4", fill = "paleturquoise4", bins=21) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When Tax Rates are 25% Lower", 
         atop("Question: Imagine instead that people’s effective income tax rates were 25% lower than what they currently are.", 
              atop("Out of 100 US taxpayers like you, who continue to work just as many hours, how many do you think would intentionally report less income to the IRS than they actually earned?"), 
         sep="")))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportTaxLower",".pdf",sep=""),  width=9.5, height=6)


#Perceived Underreporting When Tax Rates are Much Lower (half of what they currently are)
summary(Cond.B$perceivedunderreporttaxmuchlower)
sd(Cond.B$perceivedunderreporttaxmuchlower, na.rm = TRUE)
p.histogram <- ggplot(data = Cond.B) + 
  geom_histogram(aes(x=perceivedunderreporttaxlower), alpha=1, color="salmon4", fill = "salmon4", bins=21) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When Tax Rates are Half of What They Currently Are", 
         atop("Question: Imagine instead that people’s effective income tax rates were half of what they currently are.", 
              atop("Out of 100 US taxpayers like you, who continue to work just as many hours, how many do you think would intentionally report less income to the IRS than they actually earned?"), 
              sep="")))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportTaxMuchLower",".pdf",sep=""),  width=11.5, height=8)



#Correlation between perceived underreporting when tax rate is 50% higher and twice as high
cor.test(Cond.B$perceivedunderreporttaxmuchlower, Cond.B$perceivedunderreporttaxlower, alternative = "two.sided", method = "pearson", conf.level = 0.95)

###############################################################################################



#Section 8: Perceived Behavioral Reaction to Increased Audit Rate

#Perceived Underreporting When the Audit Rate is TWICE AS HIGH
summary(SurveyData$perceivedunderreportaudithigher)
sd(SurveyData$perceivedunderreportaudithigher, na.rm = TRUE)
p.histogram <- ggplot(data = SurveyData) + 
  geom_histogram(aes(x=perceivedunderreportaudithigher), alpha=1, color="purple4", fill = "purple4", bins=21) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When the Audit Rate is Twice as High", 
         atop("Question: Imagine that the audit rate was twice as high as it currently is. Out of 100 US taxpayers like you, how many do you think would intentionally underreport their taxes to the IRS?"), 
              sep=""))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportAuditHigher",".pdf",sep=""),  width=14, height=8)


#Perceived Underreporting When the Audit Rate is THREE TIMES AS HIGH
summary(SurveyData$perceivedunderreportauditmuchhigher)
sd(SurveyData$perceivedunderreportauditmuchhigher, na.rm = TRUE)
p.histogram <- ggplot(data = SurveyData) + 
  geom_histogram(aes(x=perceivedunderreportauditmuchhigher), alpha=1, color="magenta3", fill = "magenta3", bins=21) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When the Audit Rate is Three Times as High", 
         atop("Question: Imagine that the audit rate was three times as high as it currently is.  Out of 100 US taxpayers like you, how many do you think would intentionally underreport their taxes to the IRS?"), 
         sep=""))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportAuditMuchHigher",".pdf",sep=""),  width=14, height=8)



#????CHECK IF CONDITION DATA ARE CORRECT?????

################################### CONDITION A FOR SECTION 9 #############################################
#Perceived Behavioral Reaction to Increased Penalty Rate
Cond.A.S9 <- SurveyData[(SurveyData$perceivedevasionrate == 100 | SurveyData$behavreactionrandom == 1) & SurveyData$perceivedevasionrate != 0,]

#Perceived Underreporting When the Penalty Rate is 50% Higher
summary(Cond.A.S9$perceivedunderreportpenaltyhigher)
sd(Cond.A.S9$perceivedunderreportpenaltyhigher, na.rm = TRUE)
p.histogram <- ggplot(data = Cond.A.S9) + 
  geom_histogram(aes(x=perceivedunderreportpenaltyhigher), alpha=1, color="lightslateblue", fill = "lightslateblue", bins=21) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When the Penalty Rate is 50% Higher", 
         atop("Question: Imagine instead that the penalty rate was 50% higher than it currently is. Out of 100 US taxpayers like you, how many do you think would intentionally underreport their taxes to the IRS?"), 
         sep=""))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportPenaltyHigher",".pdf",sep=""),  width=14, height=8)



#Perceived Underreporting When the Penalty Rate is Twice as High
summary(Cond.A.S9$perceivedunderreportpenaltymuchhigher)
sd(Cond.A.S9$perceivedunderreportpenaltymuchhigher, na.rm = TRUE)
p.histogram <- ggplot(data = Cond.A.S9) + 
  geom_histogram(aes(x=perceivedunderreportpenaltymuchhigher), alpha=1, color="lightseagreen", fill = "lightseagreen", bins=21) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When the Penalty Rate is Twice as High", 
         atop("Question: Now imagine that the penalty rate was twice as high as it currently is. Out of 100 US taxpayers like you, how many do you think would intentionally underreport their taxes to the IRS?"), 
         sep=""))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportPenaltyMuchHigher",".pdf",sep=""),  width=14, height=8)



################################### CONDITION B FOR SECTION 9 #############################################
#Perceived Behavioral Reaction to Decreased Penalty Rate

Cond.B.S9 <- SurveyData[(SurveyData$perceivedevasionrate == 0 | SurveyData$behavreactionrandom == 2),]

#Perceived Underreporting When the Penalty Rate is Half of What Currently Is
summary(Cond.B.S9$perceivedunderreportpenaltymuchlower)
sd(Cond.B.S9$perceivedunderreportpenaltymuchlower, na.rm = TRUE)
p.histogram <- ggplot(data = Cond.B.S9) + 
  geom_histogram(aes(x=perceivedunderreportpenaltymuchlower), alpha=1, color="mediumvioletred", fill = "mediumvioletred", bins=20) + 
  theme_bw(base_size = 14) + 
  xlab("Perceived % of Underreporters") +
  ylab("count") +
  ggtitle(expression(
    atop("Histogram for Perceived % of Underreporters When the Penalty Rate is Half of What It Currently Is", 
         atop("Question: Now imagine that the penalty rate was half of what it currently is. Out of 100 US taxpayers like you, how many do you think would intentionally underreport their taxes to the IRS?"), 
         sep=""))) +
  xlim(c(0,100)) +
  theme(axis.text.x=element_text(angle= 0, hjust=0, vjust=1)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.histogram,file=paste(output.dir,"HistPerceivedUnderreportPenaltyMuchLower",".pdf",sep=""),  width=14, height=8)


p.histogram <- qplot(Cond.B.S9$perceivedunderreportpenaltymuchlower, 
                     geom = "histogram", binwidth = 5, 
                     main = "Histogram for Perceived % of Underreporters When the 
                     Penalty Rate is Half of What It Currently Is", 
                     xlab = "Perceived % of Underreporters", fill=I("mediumvioletred"))


#Perceived Underreporting When the Penalty Rate is 25% Lower Than It Currently Is
summary(Cond.B.S9$perceivedunderreportpenaltylower)
sd(Cond.B.S9$perceivedunderreportpenaltylower, na.rm = TRUE)
p.histogram <- qplot(Cond.B.S9$perceivedunderreportpenaltylower, geom = "histogram", 
                     binwidth = 5, 
                     main = "Histogram for Perceived % of Underreporters When the 
                     Penalty Rate is is 25% Lower Than What It Currently Is", 
                     xlab = "Perceived % of Underreporters", 
                     fill=I("navajowhite4"))
ggsave(p.histogram,file=paste(survey.output.dir,"HistPerceivedUnderreportPenaltyLower",".jpeg",sep=""),  width=10, height=8)


#IT SEEMS THAT THE DIRECTION OF THE RELATIONSHIP IS IN THE OPPOSITE DIRECTION FROM WHAT ONE WHOULD EXPECT
a <- summary(SurveyData$perceivedunderreportpenaltymuchlower)
b <- summary(SurveyData$perceivedunderreportpenaltylower)
X <- cbind(a,b)
colnames(X) <- c("Penalty Half of What Currently is", "Penalty is 25% lower")
xtable(X, caption = "THE DIRECTION OF THE RELATIONSHIP IS IN THE OPPOSITE DIRECTION FROM WHAT ONE WHOULD EXPECT")
write.csv(X, file = paste(output.dir, "Perceived Underreporting under Different Penalty Rates", ".csv", sep = ""))

#MAYBE ADD LOOP TO THIS SECTION 
############################# Section 10 ############################# 
#Section 10: Perceived Behavioral Reaction to Zero Audit and Penalty Rates
#Adding value lables
SurveyData$perceivedunderreportauditpenalty_a <- factor(SurveyData$perceivedunderreportauditpenalty_a,
                                                        levels = c(1,2),
                                                        labels = c("Majority would report 100% of their taxes", "Majority would underreport their taxes"))
SurveyData$perceivedunderreportauditpenalty_b <- factor(SurveyData$perceivedunderreportauditpenalty_b,
                                                        levels = c(1,2),
                                                        labels = c("Majority would report 100% of their taxes", "Majority would underreport their taxes"))
SurveyData$perceivedunderreportauditpenalty_c <- factor(SurveyData$perceivedunderreportauditpenalty_c,
                                                        levels = c(1,2),
                                                        labels = c("Majority would report 100% of their taxes", "Majority would underreport their taxes"))
SurveyData$perceivedunderreportauditpenalty_d <- factor(SurveyData$perceivedunderreportauditpenalty_d,
                                                        levels = c(1,2),
                                                        labels = c("Majority would report 100% of their taxes", "Majority would underreport their taxes"))
SurveyData$perceivedunderreportauditpenalty_e <- factor(SurveyData$perceivedunderreportauditpenalty_e,
                                                        levels = c(1,2),
                                                        labels = c("Majority would report 100% of their taxes", "Majority would underreport their taxes"))
SurveyData$perceivedunderreportauditpenalty_f <- factor(SurveyData$perceivedunderreportauditpenalty_f,
                                                        levels = c(1,2),
                                                        labels = c("Majority would report 100% of their taxes", "Majority would underreport their taxes"))
SurveyData$perceivedunderreportauditpenalty_g <- factor(SurveyData$perceivedunderreportauditpenalty_g,
                                                        levels = c(1,2),
                                                        labels = c("Majority would report 100% of their taxes", "Majority would underreport their taxes"))
SurveyData$perceivedunderreportauditpenalty_h <- factor(SurveyData$perceivedunderreportauditpenalty_h,
                                                        levels = c(1,2),
                                                        labels = c("Majority would report 100% of their taxes", "Majority would underreport their taxes"))

#Creating dataframe for "People" and "People like you" options in Question 27
PeopleData <- SurveyData[SurveyData$behavreactionrandom2 == 1,] 
PeopleLikeYouData <- SurveyData[SurveyData$behavreactionrandom2 == 2,]


#PerceivedUnderreportAuditPenalty_People
Tbl_a <- table(PeopleData$perceivedunderreportauditpenalty_a)
Tbl_b <- table(PeopleData$perceivedunderreportauditpenalty_b)
Tbl_c <- table(PeopleData$perceivedunderreportauditpenalty_c)
Tbl_d <- table(PeopleData$perceivedunderreportauditpenalty_d)
Tbl_e <- table(PeopleData$perceivedunderreportauditpenalty_e)
Tbl_f <- table(PeopleData$perceivedunderreportauditpenalty_f)
Tbl_g <- table(PeopleData$perceivedunderreportauditpenalty_g)
Tbl_h <- table(PeopleData$perceivedunderreportauditpenalty_h)

a <- prop.table(Tbl_a) #Income tax rate = 1%
b <- prop.table(Tbl_b) #Income tax rate = 2.5%
c <- prop.table(Tbl_c) #Income tax rate = 5%
d <- prop.table(Tbl_d) #Income tax rate = 10%
e <- prop.table(Tbl_e) #Income tax rate = 15%
f <- prop.table(Tbl_f) #Income tax rate = 20%
g <- prop.table(Tbl_g) #Income tax rate = 25%
h <- prop.table(Tbl_h) #Income tax rate = 30%

TblForQ27 <- round(cbind(a,b,c,d,e,f,g,h),4)
rownames(TblForQ27) <- c("Majority of people would report 100% of their taxes", "Majority of people would underreport their taxes")
colnames(TblForQ27) <- c("Income tax rate = 1%", "Income tax rate = 2.5%", "Income tax rate = 5%", "Income tax rate = 10%", "Income tax rate = 15%","Income tax rate = 20%","Income tax rate = 25%", "Income tax rate = 30%")
print(t(TblForQ27))
FinalTblForQ27 <- xtable((t(TblForQ27)), caption ="If each of the effective income tax rates below were applied to everyone, please indicate if you think (a) the majority of people would report their full income OR (b) the majority of people would underreport their income.")
print.xtable(FinalTblForQ27, type = "latex", file = "Table for Q27_People")
write.csv(FinalTblForQ27, file = paste(output.dir, "Table for Q27_People", ".csv", sep = ""))

#PerceivedUnderreportAuditPenalty_People like you
Tbl_a <- table(PeopleLikeYouData$perceivedunderreportauditpenalty_a)
Tbl_b <- table(PeopleLikeYouData$perceivedunderreportauditpenalty_b)
Tbl_c <- table(PeopleLikeYouData$perceivedunderreportauditpenalty_c)
Tbl_d <- table(PeopleLikeYouData$perceivedunderreportauditpenalty_d)
Tbl_e <- table(PeopleLikeYouData$perceivedunderreportauditpenalty_e)
Tbl_f <- table(PeopleLikeYouData$perceivedunderreportauditpenalty_f)
Tbl_g <- table(PeopleLikeYouData$perceivedunderreportauditpenalty_g)
Tbl_h <- table(PeopleLikeYouData$perceivedunderreportauditpenalty_h)

a <- prop.table(Tbl_a) #Income tax rate = 1%
b <- prop.table(Tbl_b) #Income tax rate = 2.5%
c <- prop.table(Tbl_c) #Income tax rate = 5%
d <- prop.table(Tbl_d) #Income tax rate = 10%
e <- prop.table(Tbl_e) #Income tax rate = 15%
f <- prop.table(Tbl_f) #Income tax rate = 20%
g <- prop.table(Tbl_g) #Income tax rate = 25%
h <- prop.table(Tbl_h) #Income tax rate = 30%

TblForQ27 <- round(cbind(a,b,c,d,e,f,g,h),4)
rownames(TblForQ27) <- c("Majority of people like you would report 100% of their taxes", "Majority of people like you would underreport their taxes")
colnames(TblForQ27) <- c("Income tax rate = 1%", "Income tax rate = 2.5%", "Income tax rate = 5%", "Income tax rate = 10%", "Income tax rate = 15%","Income tax rate = 20%","Income tax rate = 25%", "Income tax rate = 30%")
print(t(TblForQ27))
FinalTblForQ27 <- xtable((t(TblForQ27)), caption ="If each of the effective income tax rates below were applied to everyone, please indicate if you think (a) the majority of people like you would report their full income OR (b) the majority of people like you would underreport their income.")
print.xtable(FinalTblForQ27, type = "latex", file = "Table for Q27_People Like You")
write.csv(FinalTblForQ27, file = paste(output.dir, "Table for Q27_People Like You", ".csv", sep = ""))

#########################################################


#Section 11: Perceived Behavioral Reaction to Past Refund/Tax Debt
#"Receiving $1000 refund"
RefundData <- SurveyData[SurveyData$behavreactionrandom3 == 1,]
summary(RefundData$refunddebt)
sd(RefundData$refunddebt, na.rm = TRUE)
p.histogram <- qplot(RefundData$refunddebt, geom = "histogram", binwidth = 5, main = "Histogram for Percent Chance of Claiming Potentially Inappropraite $1000 Deduction: REFUND", xlab = "Percent Chance", fill=I("chocolate4"))
ggsave(p.histogram,file=paste(output.dir,"HistRefundDebt_REFUND",".jpeg",sep=""),  width=10, height=8)


#"Owing additional $1000"
DebtData <- SurveyData[SurveyData$behavreactionrandom3 == 2,]
summary(DebtData$refunddebt)
sd(DebtData$refunddebt, na.rm = TRUE)
p.histogram <- qplot(DebtData$refunddebt, geom = "histogram", binwidth = 5, main = "Histogram for Percent Chance of Claiming Potentially Inappropraite $1000 Deduction: DEBT", xlab = "Percent Chance", fill=I("coral4"))
ggsave(p.histogram,file=paste(output.dir,"HistRefundDebt_DEBT",".jpeg",sep=""),  width=10, height=8)

SurveyData$Refund.or.Debt[SurveyData$behavreactionrandom3 == 2] <- "Refund"
SurveyData$Refund.or.Debt[SurveyData$behavreactionrandom3 == 1] <- "Debt"

#t-test for the differences in the mean (Assuming equal variances)
X <- t.test(refunddebt ~ Refund.or.Debt, data = SurveyData)
var.test(refunddebt ~ behavreactionrandom3, data = SurveyData, conf.level = 0.05)


#t-test for the differences in the mean (Assuming equal variances) for self-employed only
SelfemployedData <- SurveyData[(SurveyData$selfemployed== 2),]
t.test(refunddebt ~ Refund.or.Debt, data = SelfemployedData)

################################################
#GENERATE TWO HISTOGRAMS IN ONE FOR REFUNDDEBT VARIABLE
#Combining two dataframes into one, by first, making a new column, v1, in each.
RefundData$Refund.or.Debt <- 'Refund'
DebtData$Refund.or.Debt <- 'Debt'

#and then combining them into new data frame, Reaction
Reaction <- rbind(RefundData, DebtData)

#Two histograms in one
p.histogram <- ggplot(Reaction, aes(refunddebt, fill = Refund.or.Debt)) + 
  ggtitle("Histogram for Percent Chance of Claiming Potentially Inappropraite $1000 Deduction") + 
  xlab("Percent Chance") + geom_histogram(alpha = 0.9, aes(y = ..density..), position = 'dodge')
ggsave(p.histogram,file=paste(output.dir,"HistRefundDebt_TWO IN ONE",".pdf",sep=""),  width=10, height=8)
#################################################


#Section 12: Weight of Tax Fairness-related Considerations
SurveyData$taxesimportant_total1 <- (SurveyData$taxesimportant_a + 
                                       SurveyData$taxesimportant_b + 
                                       SurveyData$taxesimportant_c + 
                                       SurveyData$taxesimportant_d)
#SurveyData$taxesimportant_total1[SurveyData$taxesimportant_total1 > 100 | SurveyData$taxesimportant_total1 < 100] <- NA

#??SHOULD WE STILL INCLUDE THE OBSERVATIONS THAT HAVE LESS THAN 100 IN TOTAL OF TOKENS??
#Removed observations where the total of tokens were not equal to 100 and observations that had error in the tokens during the survey
TaxesImportantData <- SurveyData[SurveyData$taxesimportant_total1 == 100 & is.na(SurveyData$error_tokens),]
X <- cbind(TaxesImportantData$taxesimportant_a, TaxesImportantData$taxesimportant_b, 
      TaxesImportantData$taxesimportant_c, TaxesImportantData$taxesimportant_d, TaxesImportantData$error_tokens)
colnames(X) <- c("The amount of taxes that I owe","The cost to figure out taxes",
                 "Benefits and public services supported by taxes",
                 "A moral obligation to report and pay taxes", "error_tokens")
sapply(data.frame(X), FUN = mean, na.rm = TRUE) #GETTING SLIGHTLY DIFFERENT NUMBERS IN STATA

#latex table for this question (Q29)
a <- quantile(TaxesImportantData$taxesimportant_a,0.25, na.rm = T)
b <- mean(TaxesImportantData$taxesimportant_a, na.rm = T)
c <- quantile(TaxesImportantData$taxesimportant_a,0.75, na.rm = T)
d <- quantile(TaxesImportantData$taxesimportant_b,0.25, na.rm = T)
e <- mean(TaxesImportantData$taxesimportant_b, na.rm = T)
f <- quantile(TaxesImportantData$taxesimportant_b,0.75, na.rm = T)
g <- quantile(TaxesImportantData$taxesimportant_c,0.25, na.rm = T)
h <- mean(TaxesImportantData$taxesimportant_c, na.rm = T)
i <- quantile(TaxesImportantData$taxesimportant_c,0.75, na.rm = T)
j <- quantile(TaxesImportantData$taxesimportant_d,0.25, na.rm = T)
k <- mean(TaxesImportantData$taxesimportant_d, na.rm = T)
l <- quantile(TaxesImportantData$taxesimportant_d,0.75, na.rm = T)

X <- rbind(cbind(a,b,c),cbind(d,e,f), cbind(g,h,i), cbind(j,k,l))
rownames(X) <- c("The amount of taxes that I owe","The cost to figure out taxes",
                 "Benefits and public services supported by taxes",
                 "A moral obligation to report and pay taxes")
colnames(X) <- c("Q1", "Mean", "Q3")
xtable(X, caption = "Imagine that you have 100 tokens. Please allocate 100 tokens to the issues below. 
More tokens means more important.In terms of how you think about taxes and paying your taxes, how 
       important is each of the following?", digits = 1)

#latex table for this question (Q29) for Self-Employed
TaxesImportantDataSelfEmployed <- TaxesImportantData[TaxesImportantData$selfemployed == 2,]
a <- quantile(TaxesImportantDataSelfEmployed$taxesimportant_a,0.25, na.rm = T)
b <- mean(TaxesImportantDataSelfEmployed$taxesimportant_a, na.rm = T)
c <- quantile(TaxesImportantDataSelfEmployed$taxesimportant_a,0.75, na.rm = T)
d <- quantile(TaxesImportantDataSelfEmployed$taxesimportant_b,0.25, na.rm = T)
e <- mean(TaxesImportantDataSelfEmployed$taxesimportant_b, na.rm = T)
f <- quantile(TaxesImportantDataSelfEmployed$taxesimportant_b,0.75, na.rm = T)
g <- quantile(TaxesImportantDataSelfEmployed$taxesimportant_c,0.25, na.rm = T)
h <- mean(TaxesImportantDataSelfEmployed$taxesimportant_c, na.rm = T)
i <- quantile(TaxesImportantDataSelfEmployed$taxesimportant_c,0.75, na.rm = T)
j <- quantile(TaxesImportantDataSelfEmployed$taxesimportant_d,0.25, na.rm = T)
k <- mean(TaxesImportantDataSelfEmployed$taxesimportant_d, na.rm = T)
l <- quantile(TaxesImportantDataSelfEmployed$taxesimportant_d,0.75, na.rm = T)

X <- rbind(cbind(a,b,c),cbind(d,e,f), cbind(g,h,i), cbind(j,k,l))
rownames(X) <- c("The amount of taxes that I owe","The cost to figure out taxes",
                 "Benefits and public services supported by taxes",
                 "A moral obligation to report and pay taxes")
colnames(X) <- c("Q1", "Mean", "Q3")
xtable(X, caption = "THE RESULTS ARE FOR SELF-EMPLOYED ONLY: Imagine that you have 100 tokens. Please allocate 100 tokens to the issues below. 
       More tokens means more important.In terms of how you think about taxes and paying your taxes, how 
       important is each of the following?", digits = 1)

#??SHOULD WE STILL INCLUDE THE OBSERVATIONS THAT HAVE LESS THAN 100 IN TOTAL OF TOKENS??
#Section 13: Weight of Personal, Network, and Media Information on Fairness
SurveyData$taxesfairness_total1 <-(SurveyData$taxesfairness_a + SurveyData$taxesfairness_b + 
                                     SurveyData$taxesfairness_c)
TaxesFairnessData <- SurveyData[SurveyData$taxesfairness_total1 == 100,] #Removed observations which do not add up to 100 tokens
X <- cbind(TaxesFairnessData$taxesfairness_a, TaxesFairnessData$taxesfairness_b,TaxesFairnessData$taxesfairness_c)
colnames(X) <- c("Your own thoughts","What you hear and know from friends, family, and other close contacts",
                 "What you hear broadly from the media and other sources")
sapply(data.frame(X), FUN = mean, na.rm = TRUE)

#t-test for the differences in the mean for the importance of the personal thoughts on the fairness of taxes
TaxesFairnessData$selfemployed.or.not <- "Not self-employed"
TaxesFairnessData$selfemployed.or.not[TaxesFairnessData$selfemployed == 2] <- "Self-employed"
t.test(taxesfairness_a ~ selfemployed.or.not, data = TaxesFairnessData)

#latex table for Q30 question
a <- quantile(TaxesFairnessData$taxesfairness_a,0.25, na.rm = T)
b <- mean(TaxesFairnessData$taxesfairness_a, na.rm = T)
c <- quantile(TaxesFairnessData$taxesfairness_a,0.75, na.rm = T)
d <- quantile(TaxesFairnessData$taxesfairness_b,0.25, na.rm = T)
e <- mean(TaxesFairnessData$taxesfairness_b, na.rm = T)
f <- quantile(TaxesFairnessData$taxesfairness_b,0.75, na.rm = T)
g <- quantile(TaxesFairnessData$taxesfairness_c,0.25, na.rm = T)
h <- mean(TaxesFairnessData$taxesfairness_c, na.rm = T)
i <- quantile(TaxesFairnessData$taxesfairness_c,0.75, na.rm = T)


X <- rbind(cbind(a,b,c),cbind(d,e,f), cbind(g,h,i))
rownames(X) <- c("Your own thoughts",
                 "What you hear and know from friends, family, and other close contacts",
                 "What you hear broadly from the media and other sources")
colnames(X) <- c("Q1", "Mean", "Q3")
xtable(X, caption = "Now let’s consider your thoughts on the fairness of taxes, and what 
you’ve seen and heard from those around you.Again, please allocate 100 tokens to the issues below.  
In terms of how fair taxes seem to you, how important is each of the following?", digits = 1)

#latex table for Q30 question for Self-Employed
TaxesFairnessDataSelfEmployed <- TaxesFairnessData[TaxesFairnessData$selfemployed == 2,]
a <- quantile(TaxesFairnessDataSelfEmployed$taxesfairness_a,0.25, na.rm = T)
b <- mean(TaxesFairnessDataSelfEmployed$taxesfairness_a, na.rm = T)
c <- quantile(TaxesFairnessDataSelfEmployed$taxesfairness_a,0.75, na.rm = T)
d <- quantile(TaxesFairnessDataSelfEmployed$taxesfairness_b,0.25, na.rm = T)
e <- mean(TaxesFairnessDataSelfEmployed$taxesfairness_b, na.rm = T)
f <- quantile(TaxesFairnessDataSelfEmployed$taxesfairness_b,0.75, na.rm = T)
g <- quantile(TaxesFairnessDataSelfEmployed$taxesfairness_c,0.25, na.rm = T)
h <- mean(TaxesFairnessDataSelfEmployed$taxesfairness_c, na.rm = T)
i <- quantile(TaxesFairnessDataSelfEmployed$taxesfairness_c,0.75, na.rm = T)


X <- rbind(cbind(a,b,c),cbind(d,e,f), cbind(g,h,i))
rownames(X) <- c("Your own thoughts",
                 "What you hear and know from friends, family, and other close contacts",
                 "What you hear broadly from the media and other sources")
colnames(X) <- c("Q1", "Mean", "Q3")
xtable(X, caption = "THE RESULTS ARE FOR SELF-EMPLOYED ONLY: Now let’s consider your thoughts on the fairness of taxes, and what 
you’ve seen and heard from those around you.Again, please allocate 100 tokens to the issues below.  
In terms of how fair taxes seem to you, how important is each of the following?", digits = 1)



#??SHOULD WE STILL INCLUDE THE OBSERVATIONS THAT HAVE LESS THAN 100 IN TOTAL OF TOKENS??
#Section 14: Weight of Personal, Network, and Media Information on Audit/Penalty Risk
SurveyData$riskauditspenalties_total1 <- (SurveyData$riskauditspenalties_a + SurveyData$riskauditspenalties_b + 
                                            SurveyData$riskauditspenalties_c)
RiskAuditsPenaltiesData <- SurveyData[SurveyData$riskauditspenalties_total1 == 100,] #Removed observations which do not add up to 100 tokens
X <- cbind(RiskAuditsPenaltiesData$riskauditspenalties_a, RiskAuditsPenaltiesData$riskauditspenalties_b,
      RiskAuditsPenaltiesData$riskauditspenalties_c)
colnames(X) <- c("Your own thoughts","What you hear and know from friends, family, and other close contacts",
                 "What you hear broadly from the media and other sources")
sapply(data.frame(X),FUN = mean, na.rm = TRUE)

#latex table for Q31 question
a <- quantile(RiskAuditsPenaltiesData$riskauditspenalties_a,0.25, na.rm = T)
b <- mean(RiskAuditsPenaltiesData$riskauditspenalties_a, na.rm = T)
c <- quantile(RiskAuditsPenaltiesData$riskauditspenalties_a,0.75, na.rm = T)
d <- quantile(RiskAuditsPenaltiesData$riskauditspenalties_b,0.25, na.rm = T)
e <- mean(RiskAuditsPenaltiesData$riskauditspenalties_b, na.rm = T)
f <- quantile(RiskAuditsPenaltiesData$riskauditspenalties_b,0.75, na.rm = T)
g <- quantile(RiskAuditsPenaltiesData$riskauditspenalties_c,0.25, na.rm = T)
h <- mean(RiskAuditsPenaltiesData$riskauditspenalties_c, na.rm = T)
i <- quantile(RiskAuditsPenaltiesData$riskauditspenalties_c,0.75, na.rm = T)


X <- rbind(cbind(a,b,c),cbind(d,e,f), cbind(g,h,i))
rownames(X) <- c("Your own thoughts",
                 "What you hear and know from friends, family, and other close contacts",
                 "What you hear broadly from the media and other sources")
colnames(X) <- c("Q1", "Mean", "Q3")
xtable(X, caption = "Now let’s consider your thoughts on the risk of audits and penalties for not paying 
one’s taxes.  Again, please allocate 100 tokens to the issues below.In terms of how you think about these
risks, how important is each of the following?", digits = 1)

#latex table for Q31 question for Self-Employed
RiskAuditsPenaltiesDataSelfEmployed <- RiskAuditsPenaltiesData[RiskAuditsPenaltiesData$selfemployed == 2,]

a <- quantile(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_a,0.25, na.rm = T)
b <- mean(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_a, na.rm = T)
c <- quantile(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_a,0.75, na.rm = T)
d <- quantile(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_b,0.25, na.rm = T)
e <- mean(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_b, na.rm = T)
f <- quantile(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_b,0.75, na.rm = T)
g <- quantile(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_c,0.25, na.rm = T)
h <- mean(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_c, na.rm = T)
i <- quantile(RiskAuditsPenaltiesDataSelfEmployed$riskauditspenalties_c,0.75, na.rm = T)


X <- rbind(cbind(a,b,c),cbind(d,e,f), cbind(g,h,i))
rownames(X) <- c("Your own thoughts",
                 "What you hear and know from friends, family, and other close contacts",
                 "What you hear broadly from the media and other sources")
colnames(X) <- c("Q1", "Mean", "Q3")
xtable(X, caption = "THE RESULTS ARE FOR SELF-EMPLOYED ONLY: Now let’s consider your thoughts on the risk of audits and penalties for not paying 
one’s taxes.  Again, please allocate 100 tokens to the issues below.In terms of how you think about these
risks, how important is each of the following?", digits = 1)


#Section 15: Perception of services provided by taxes
TblServiceTaxes <- table(SurveyData$servicestaxes, exclude = NULL)
X <- prop.table(TblServiceTaxes)
rownames(X) <- c("Not at all worth it","2","3","4", "Definitely worth it", "Missing")
Z <- t(X*100)
xtable(Z, caption = "Perception of Services Provided by Taxes: To what extent are the public goods and services that you receive worth 
       the federal income taxes you pay?", digits = 1)

SurveyData$servicestaxes <- factor(SurveyData$servicestaxes,levels = c(1,2,3,4,5),labels = c("Not at all worth it","2","3","4", "Definitely worth it"))

p.barplot <- ggplot(SurveyData, aes(x = servicestaxes)) + geom_bar(fill = "blue") + 
  theme_bw(base_size = 14) + 
  ylab("count") +
  xlab("") +
  ggtitle("To what extent are the public goods and services that you receive worth 
          the federal income taxes you pay?") +
  theme(axis.text.x=element_text(size =14, angle= 0, hjust=0.5, vjust=1)) +
  theme(axis.text.y=element_text(size =14)) +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=-1))
ggsave(p.barplot,file=paste(output.dir,"BarplotServiceTaxes",".pdf",sep=""), width=12, height=8)



#FOR SECTIONS 16-18 SHOULD I INCLUDE MISSING VALUES IN THE FREQUENCIES
#Section 16: Free-riding, Part 1
#FR11:Regularly listen to public radio without ever contributing
CompletedData$fr11 <- factor(CompletedData$fr11,levels = c(1,2,3),labels = c("Always OK", "Sometimes OK", "Never OK"))
Tbl_FR11 <- table(CompletedData$fr11)
prop.table(Tbl_FR11)
barplot(prop.table(Tbl_FR11), main = "Regularly listen to public radio without ever contributing", 
        names.arg = c("Always OK", "Sometimes OK", "Never OK"),
        ylab = "Relative frequency",
        col = "green")                                                       

#FR12:Illegally copying, downloading, or streaming movies
CompletedData$fr12 <- factor(CompletedData$fr12,levels = c(1,2,3),labels = c("Always OK", "Sometimes OK", "Never OK"))
Tbl_FR12 <- table(CompletedData$fr12)
prop.table(Tbl_FR12)
barplot(prop.table(Tbl_FR12), main = "Illegally copying, downloading, or streaming movies", 
        names.arg = c("Always OK", "Sometimes OK", "Never OK"), 
        ylab = "Relative frequency",
        col = "orange")                                                       

#FR13:Have a dog but not getting it spayed or neutered
CompletedData$fr13 <- factor(CompletedData$fr13,levels = c(1,2,3),labels = c("Always OK", "Sometimes OK", "Never OK"))
Tbl_FR13 <- table(CompletedData$fr13)
prop.table(Tbl_FR13)
barplot(prop.table(Tbl_FR13), main = "Have a dog but not getting it spayed or neutered", 
        names.arg = c("Always OK", "Sometimes OK", "Never OK"), 
        ylab = "Relative frequency", col = "red")                                                       

#FR14:Avoid getting the flu vaccine
CompletedData$fr14 <- factor(CompletedData$fr14,levels = c(1,2,3),labels = c("Always OK", "Sometimes OK", "Never OK"))
Tbl_FR14 <- table(CompletedData$fr14)
prop.table(Tbl_FR14)
p.barplot <- barplot(prop.table(Tbl_FR14), main = "Avoid getting the flu vaccine", 
        names.arg = c("Always OK", "Sometimes OK", "Never OK"), 
        ylab = "Relative frequency", col = "purple")                                                       


#FR15:Avoid paying all of the income tax that you owe
CompletedData$fr15 <- factor(CompletedData$fr15,levels = c(1,2,3),labels = c("Always OK", "Sometimes OK", "Never OK"))
Tbl_FR15 <- table(CompletedData$fr15)
prop.table(Tbl_FR15)
barplot(prop.table(Tbl_FR15), main = "Avoid paying all of the income tax that you owe", 
        names.arg = c("Always OK", "Sometimes OK", "Never OK"), 
        ylab = "Relative frequency", col = "dark red")                                                       



#Section 17: Free-riding, Part 2
#FR21: Regularly listen to public radio without ever contributing
Tbl_FR21 <- table(CompletedData$fr21)
prop.table(Tbl_FR21)
barplot(prop.table(Tbl_FR21), main = "Regularly listen to public radio without ever contributing", 
        xlab = "Number of people out of 100 who would say that it is at least sometimes OK", 
        ylab = "Relative frequency", col = "dark green")                                                       

#FR22: Illegally copying, downloading, or streaming movies
Tbl_FR22 <- table(CompletedData$fr22)
prop.table(Tbl_FR22)
barplot(prop.table(Tbl_FR22), main = "Illegally copying, downloading, or streaming movies", 
        xlab = "Number of people out of 100 who would say that it is at least sometimes OK", 
        ylab = "Relative frequency", col = "light blue")                                                       

#FR23: Have a dog but not getting it spayed or neutered
Tbl_FR23<- table(CompletedData$fr23)
prop.table(Tbl_FR23)
barplot(prop.table(Tbl_FR23), main = "Have a dog but not getting it spayed or neutered", 
        xlab = "Number of people out of 100 who would say that it is at least sometimes OK", 
        ylab = "Relative frequency", col = "light green")                                                       

#FR24: Avoid getting the flu vaccine
Tbl_FR24<- table(CompletedData$fr24)
prop.table(Tbl_FR24)
barplot(prop.table(Tbl_FR24), main = "Avoid getting the flu vaccine", 
        xlab = "Number of people out of 100 who would say that it is at least sometimes OK", 
        ylab = "Relative frequency", col = "salmon")                                                       

#FR25: Avoid paying all of the income tax that you owe
Tbl_FR25<- table(CompletedData$fr25)
prop.table(Tbl_FR25)
barplot(prop.table(Tbl_FR25), main = "Avoid paying all of the income tax that you owe",
        xlab = "Number of people out of 100 who would say that it is at least sometimes OK", 
        ylab = "Relative frequency", col = "brown")                                                       

#Chi-square TEST BETWEEN FR VARIABLES (PART 1 VARIABLES AGAINST PART 2 VARIABLES)???



#Section 18: Setting up Future Policy Experiments with ABMs
CompletedData$actor <- factor(CompletedData$actor,levels = c(1,2,3,4,5),
                              labels = c("I would be much more likely to fully report my income",
                                         "I would be somewhat more likely to fully report my income", 
                                         "It would not affect my income reporting either way", 
                                         "I would be somewhat less likely to fully report my income", 
                                         "I would be much less likely to fully report my income"))
Tbl_Actor<- table(CompletedData$actor)
prop.table(Tbl_Actor)
barplot(prop.table(Tbl_Actor), 
        main = "Imagine that you heard a famous actor was caught and prosecuted for tax evasion.
        In your mind,would hearing about this make you more or less likely to report all of taxes
        you owe to the IRS?", 
        names.arg = c("much more likely to fully report income",
                      "somewhat more likely to fully report income",
                      "It would not affect my income reporting either way", 
                      "somewhat less likely to fully report income", 
                      "much less likely to fully report income"),
        ylab = "Relative frequency", col = "light grey")                                                       



#Section 19: Tax and Audit Experience
#Generatingting a variable with the following categories:
# 1.Not working now but worked in the past
# 2.Not working now and never worked
# 3.Working now for someone else
# 4.Working now and self-employed
# 5.Working now and other
SurveyData$workstatus[SurveyData$workforpay == "Yes"] <- 1
SurveyData$workstatus[SurveyData$workforpay == "No"] <- 2
SurveyData$workstatus[SurveyData$selfemployed == 1] <- 3
SurveyData$workstatus[SurveyData$selfemployed == 2] <- 4
SurveyData$workstatus[SurveyData$selfemployed == 3] <- 5
SurveyData$workstatus <- factor(SurveyData$workstatus, levels = c(1,2,3,4,5),
                                labels = c("Not working now but worked in the past",
                                           "Not working now and never worked",
                                           "Working now for someone else",
                                           "Working now and self-employed",
                                           "Working now and other"))

Tbl <- table(SurveyData$workstatus, exclude = NULL)
X <- prop.table(Tbl)
Z <- cbind(Tbl, X*100)
rownames(Z) <- c("Not working now but worked in the past",
                 "Not working now and never worked",
                 "Working now for someone else",
                 "Working now and self-employed",
                 "Working now and other", "Missing")
xtable(Z, caption = "Work Status of Respondents", digits = 1)

#Have you ever worked for pay? (Asked those who did not indicate "Working now" in CURRENTJOBSTATUS)
SurveyData$workforpay <- factor(SurveyData$workforpay, levels = c(1,2), labels = c("Yes", "No"))
Tbl_WorkForPay <- table(SurveyData$workforpay, exclude = NULL)
prop.table(Tbl_WorkForPay)
(w = as.data.frame(table(SurveydData$workforpay, exclude = NULL))) #Simple frequencies with missing values
cbind(Tbl_WorkForPay,prop.table(Tbl_WorkForPay))


#Number of Self-employed
(t = as.data.frame(table(SurveyData$currentjobstatuss1)))
(t = as.data.frame(table(SurveyData$selfemployed, exclude = NULL)))

SurveyData$selfemployed <- factor(SurveyData$selfemployed, levels = c(1,2,3), 
                                     labels = c("Work for someone else","Self-employed","Other"))
TblSelfEmployed <- table(SurveyData$selfemployed, exclude = NULL)

#Percent of self-employed
X <- prop.table(TblSelfEmployed)
Z <- cbind(TblSelfEmployed,X*100)
rownames(Z) <- c("Work for someone else","Self-employed","Other", "Missing")
colnames(Z) <- c("Number of Respondents", "Percent of Respondents")
Y <- xtable(Z, caption = "Self-employment Status of Respondents")
digits(Y) <- c(0,0,1)
print(Y, include.rownames = T)

#Have you ever filed a tax return yourself or had someone file it for you? 
SurveyData$everfiledtaxes <- factor(SurveyData$everfiledtaxes, levels = c(1,2,3), labels = c("Yes", "No", "Don't know or can't remember"))
Tbl_EverFiledTaxes <- table(SurveyData$everfiledtaxes, exclude = NULL)
X <- prop.table(Tbl_EverFiledTaxes)
(w = as.data.frame(table(SurveyData$everfiledtaxes, exclude = NULL))) #Simple frequencies with missing values
Z <- cbind(Tbl_EverFiledTaxes,X*100)
rownames(Z) <- c("Yes", "No", "Don't know or can't remember", "Missing")
colnames(Z) <- c("Number of Respondents", "Percent of Respondents")
Y <- xtable(Z, caption = "Have you ever filed a tax return yourself or had someone file it for you?")
digits(Y) <- c(0,0,1)
print(Y, include.rownames = T)

#Percent of those who pay someone else to prepare their tax returns
SurveyData$preptaxes <- factor(SurveyData$preptaxes, levels = c(1,2,3,4,5), labels = c("I prepare my own tax returns using tax software on the computer","I prepare my own tax returns, without using tax software","I pay someone else to prepare my tax returns", "I do not prepare a tax return", "I don't know or would prefer not to say"))
TblPrepTaxes <- table(SurveyData$preptaxes, exclude = NULL)
#Percent of individuals who pay someone else to prepare their tax returns
X <- prop.table(TblPrepTaxes)
Z <- cbind(TblPrepTaxes,X*100)
rownames(Z) <- c("I prepare my own tax returns using tax software on the computer","I prepare my own tax returns, without using tax software","I pay someone else to prepare my tax returns", "I do not prepare a tax return", "I don't know or would prefer not to say", "Missing")
colnames(Z) <- c("Number of Respondents", "Percent of Respondents")
Y <- xtable(Z, caption = "Do you typically prepare your own tax returns or do you pay someone (e.g., an accountant or lawyer) to prepare them for you?")
digits(Y) <- c(0,0,1)
print(Y, include.rownames = T)

#Percent of Audited (self)
TblEverAudited <- table(SurveyData$everaudited, exclude = NULL)
X <- prop.table(TblEverAudited)
Z <- cbind(TblEverAudited,X*100)
rownames(Z) <- c("Yes","No","Don't know or can't remember", "Prefer not to answer", "Missing")
colnames(Z) <- c("Number of Respondents", "Percent of Respondents")
Y <- xtable(Z, caption = "Have you ever been audited by the IRS?")
digits(Y) <- c(0,0,1)
print(Y, include.rownames = T)

#Percent of Respondents Whose Spouse or Domestic Partner Was Audited
TblSpouseAudit <- table(SurveyData$spouseaudit, exclude = NULL)
X <- prop.table(TblSpouseAudit)
Z <- cbind(TblSpouseAudit,X*100)
rownames(Z) <- c("I am not currently married or living with a domestic partner",
                 "Yes","No","Don't know or can't remember", "Prefer not to answer", "Missing")
colnames(Z) <- c("Number of Respondents", "Percent of Respondents")
Y <- xtable(Z, caption = "Has your spouse or domestic partner ever been audited by the IRS at any time during the past five years?")
digits(Y) <- c(0,0,1)
print(Y, include.rownames = T)

#Percent of Audited (either self or spouse)
(CrossTab <- table(SurveyData$everaudited, SurveyData$spouseaudit, exclude = NULL))
SurveyData$Audited <- ifelse((SurveyData$everaudited == 1 | SurveyData$spouseaudit == 2), 1,0)
SurveyData$Audited[which(is.na(SurveyData$Audited))] <- 0
SurveyData$Audited <- factor(SurveyData$Audited, levels = c(0,1), labels = c("Not audited/Missing", "Audited"))
TblAudited <-table(SurveyData$Audited)
#Percent of Audited (either self or spouse)
X <- prop.table(TblAudited)
Z <- cbind(TblAudited,X*100)
rownames(Z) <- c("Not audited/Missing", "Audited")
colnames(Z) <- c("Number of Respondents", "Percent of Respondents")
Y <- xtable(Z, caption = "Has either respondent or spouse/domestic partner ever been audited?")
digits(Y) <- c(0,0,1)
print(Y, include.rownames = T)

#Some cross-tabs within the "Tax and Audit Experience" Section
table(SurveyData$selfemployed, SurveyData$everfiledtaxes, exclude = NULL)
table(SurveyData$selfemployed, SurveyData$everaudited, exclude = NULL)
