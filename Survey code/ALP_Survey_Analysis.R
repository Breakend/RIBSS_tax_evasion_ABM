remove(list = ls(all = TRUE))
gc()

library(xtable)
library(yacca)
library(ggplot2)
library(reshape2)
require(Hmisc)
library(foreign)
library(corrgram)
library(xlsx)
library(dplyr)
library(schoRsch)
library(plm)


options(warn=1) 
text.size <- 20

## Specify directories 
survey.data.dir <- "Survey data/"
survey.output.dir <- "SurveyTablesPlots/"
library.dir   <- "Survey code/SurveyLibrary/"
library <- file.path(library.dir, "library.R")
source(library)

model.param.tab <- list()

### 

#ALP Tax Evasion Survey
ALP.data.file.name <- "ALP_MS456_weighted"
ALP.data.file.name.weights <- "ALP_MS456_2017_01_19_04_40_42_weighted"
SD <- read.csv(paste(survey.data.dir,ALP.data.file.name,".csv",sep=""),stringsAsFactors = FALSE)

head(SD)

# load(paste(survey.data.dir,"tmp2.image",".Rdata",sep=""))
#Check the number of incomplete surveys
#(t = as.data.frame(table(SD$tsend)))

#Dataset with the completed surveys only 
CompletedData <- SD[(SD$tsend != ""),]



########################################
###                                  ###
###     Reorganize Data for          ###
###     Social Network questions     ###
###            Qus.1-7               ###
###                                  ###
########################################

ego.data <- readRDS(file=paste(survey.data.dir,"ego.data.Rdata",sep=""))
alters.table<-readRDS(file=paste(survey.data.dir,"EgoNet",".Rdata",sep=""))

subsets  <- data.frame(all=rep(T,nrow(ego.data)),
                       audited = ego.data$hheveraudited,
                       nonaudited = !(ego.data$hheveraudited),
                       selfemployed = ego.data$selfemployed,
                       not.selfemployed = !(ego.data$selfemployed))

########################################
###                                  ###
###     Analyze                      ###
###     Social Network questions     ###
###            Qus.1-7               ###
###                                  ###
########################################
table.names <- c("alters.rel","alters.rel.tTaxes","altereduc","altereduc.tTaxes","alterselfemployed","alterselfemployed.tTaxes","altertaxaudit","altertaxaudit.tTaxes","alter_SE_A","alter_SE_A.tTaxes", "altertaxhowoften")
table.names2 <- table.names[!grepl(".tTaxes",table.names)] 

tt <- "alters.rel"
tmp.list<- list()
for(tt in table.names2){
  
  tmp1 <- alters.table[[tt]]
  add100 <- (rowSums(tmp1[,-c(1:3)],na.rm=T)==100)
  summary.tab <- as.data.frame(colMeans(tmp1[add100,-c(1:3)],na.rm=T))
  summary.tab<- t(summary.tab)
  rownames( summary.tab) <- "percentage"
  
  
  tab.label <- paste("Percentage of",tt,sep=" ")
  print(xtable(round(summary.tab,2)),  
        file=paste(survey.output.dir,tab.label,".tex",sep=""))
  
  m1<- summary.tab
  
  
  ll<- paste(tt,".tTaxes",sep="")
  if(ll %in% table.names){
    tmp2 <- alters.table[[ll]]
    select <- (rowSums(tmp2[,-c(1:3)])==100)
    m2<- colMeans(tmp2[select,-c(1:3)],na.rm=T)
    factor <- mean(tmp2[,3],na.rm=T)/mean(tmp2[,2],na.rm=T)
    ### set no.factor to TRUE if you want to renomalize the tTaxes table so that the percentages sum to 100% 
    ### rather than sum to the percentage of alters that you talk taxes with
    no.factor <- TRUE
    if (!no.factor){
      m2<- factor*m2
    }
    m1<-rbind(m1,m2)
    tmp.list[[tt]] <- m1
    
    
    rownames(m1) <- c(tt,ll)
    m1<- data.frame(type=c(tt,ll),m1)
    df <- melt(m1, id="type" ,variable.name= "levels", value.name = "percentage")
    #df$levels <- factor(df$levels, levels = df$levels)
    p1<- ggplot()+
      geom_bar(data= df,aes(x=levels,y=percentage,fill=type),
               stat="identity", color="black", position = "dodge")
    
    df.split <- split(df,df$levels)
    z<-(lapply(df.split, FUN=function(x){
      100*(min(x$percentage)/max(x$percentage))}))
    
    if(no.factor){
      z<-(lapply(df.split, FUN=function(x){
        100*(factor*min(x$percentage)/max(x$percentage))}))
    }
    
    z<-as.data.frame(do.call("rbind",z))
    
    z$levels <- as.factor(rownames(z))
    z$levels <-ordered(z$levels, levels =unique(as.character(df$levels)))
    
    colnames(z)<-c("percentage","levels")
    z$levels<- as.factor(z$levels)
    
    p2<- ggplot()+
      geom_bar(data= z,aes(x=levels,y=percentage),fill="cornflowerblue",
               stat="identity", color="black")
    
    
  }else{
    df <- melt(t(m1), variable.name= "levels", value.name = "percentage")
    df$levels <- rownames(df)
    df$levels <- factor(df$Var1, levels = df$Var1)
    
    p1<- ggplot()+
      geom_bar(data= df,aes(x=levels,y=percentage),stat="identity", 
               color="black",fill="red")
    
    p2<-NULL
  }
  print(m1)
  
  p1 <- p1+
    theme_bw() +
    xlab(tt)+
    ylab("Percentage of Alters")+
    #guides(fill = guide_legend(title=""))+
    guides(fill=FALSE)+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=text.size,angle=20 ), 
          axis.text.y=element_text(size=text.size ) ,
          title = element_text( size=text.size ) ,
          strip.text=element_text( size=text.size ) ,
          axis.title.x = element_text( size=text.size ) ,
          axis.title.y = element_text( size=text.size ),
          legend.text = element_text( size = 14))
  tab.label <- gsub("\\.","",tab.label)
  tab.label <- gsub(" ","_",tab.label)
  ggsave(p1,
         file=paste(survey.output.dir,tab.label,".pdf",sep=""),
         width=10, height=5)
  if(!is.null(p2)){
 
####################################
# change x labels: NA at the end 
    
 p2 <- p2+
    theme_bw() +
   #guides(fill = guide_legend(title=""))+
    guides(fill=FALSE)+
    xlab(tt)+
    ylab("% of tax talking Alters")+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=text.size,angle=20 ), 
          axis.text.y=element_text(size=text.size ) ,
          title = element_text( size=text.size ) ,
          strip.text=element_text( size=text.size ) ,
          axis.title.x = element_text( size=text.size ) ,
          axis.title.y = element_text( size= text.size),
          legend.text = element_text( size = 14))
  tab.label <- gsub("\\.","",tab.label)
  tab.label <- gsub(" ","_",tab.label)
  ggsave(p2,
         file=paste(survey.output.dir,tab.label,"_prop",".pdf",sep=""),
         width=10, height=5)
  }
}
print(tmp.list)

### Create tables

tab.out<- list()
for (subset in colnames(subsets)){  
  ss<- subsets[,subset]
  tab.out[[subset]]<-table(ss,useNA="ifany")
}
tmp.tab.out <- as.data.frame(do.call("rbind",tab.out))
tmp.tab.out <- cbind(rownames(tmp.tab.out),tmp.tab.out)
colnames(tmp.tab.out)[1] <- "Respondent subsample"
tmp.tab.out[1,c(2,4)] <- 0

tab.label<- paste("table_","subsets",sep="")

tmp.tab.out<- xtable(tmp.tab.out,digits=0)
print(tmp.tab.out,  
      file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F)


tab.out<- list()
tt <- "alters.rel"
for(tt in table.names){
  tmp.tab.out <- list()
  
  for (subset in colnames(subsets)){  
    ss<- subsets[,subset]
    tmp1 <- alters.table[[tt]][ss,]
    select <- (rowSums(tmp1[,-c(1:3)])==100)
    summary.tab <- as.data.frame(colMeans(tmp1[select,-c(1:3)],na.rm=T))
    
    summary.tab<- t(summary.tab)
    rownames( summary.tab) <- subset
    
    summary.tab <- 100*summary.tab/sum(summary.tab ) 
    tmp.tab.out[[subset]] <- summary.tab
  } 
  tmp.tab.out <- as.data.frame(do.call("rbind",tmp.tab.out))
  sums<-rowSums(tmp.tab.out)
  sums<- pmin(rowSums(tmp.tab.out),100) 
  #tmp.tab.out<- cbind(tmp.tab.out, tot=sums)
  tmp.tab.out <- cbind(rownames(tmp.tab.out),tmp.tab.out)
  colnames(tmp.tab.out)[1] <- "Respondent subsample"
  
  tab.label<- paste("table_",gsub("\\.","_",tt),sep="")
  tab.out[[tab.label]]<- tmp.tab.out 
  
  
  tmp.tab.out<- xtable(tmp.tab.out,digits=1)
  vv.c <-nchar(names(tmp.tab.out))>12
  vv.c <- c(F,vv.c)
  vv<- align(tmp.tab.out)[vv.c]
  vv[vv %in% "r"]<- "L{2cm}"
  align(tmp.tab.out)[vv.c]<- vv
  
  print(tmp.tab.out,  
        file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F)
}

for (tt in rev(table.names2)[-1]){
  ll<- paste(tt,".tTaxes",sep="")
  ll<- paste("table_",gsub("\\.","_",ll),sep="")
  tab.label<- paste("table_tTaxes_prop_",gsub("\\.","_",tt),sep="")
  tt2<- paste("table_",gsub("\\.","_",tt),sep="")
  
  tab.out[[tab.label]]<- tab.out[[ll]]
  
  tmp.tab.out<- xtable(tab.out[[tab.label]],digits=1)
  vv.c <-nchar(names(tmp.tab.out))>12
  vv.c <- c(F,vv.c)
  vv<- align(tmp.tab.out)[vv.c]
  vv[vv %in% "r"]<- "L{2cm}"
  align(tmp.tab.out)[vv.c]<- vv
  
  print(tmp.tab.out,  
        file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F)
}

print(tab.out)


########################################
###                                  ###
###    NETWORK MODEL PARAMETERs
###                                  ###
########################################

avg.k0 <- avg.k <- avg.freq.tTaxes <- list()
for(s in names(subsets)){
  subset<- subsets[,s]
  avg.k0[[s]]<- get.ALP.net.degree(alters.table[["altertaxhowoften"]],talk.taxes =F,
                                   subset,show.summary.instead=T)
  avg.k[[s]]<-get.ALP.net.degree(alters.table[["altertaxhowoften"]],
                                 subset,show.summary.instead=T)
  avg.freq.tTaxes[[s]]<-get.ALP.avg.rate.tTaxes(alters.table[["altertaxhowoften"]],
                                                subset,show.dist.instead=T)
}

avg.k0 <- as.data.frame(do.call("rbind",avg.k0))
avg.k <- as.data.frame(do.call("rbind",avg.k))
avg.freq.tTaxes<- as.data.frame(do.call("rbind",avg.freq.tTaxes))



avg.k <- cbind(rownames(avg.k),avg.k)
colnames(avg.k)[1] <- "Respondent subsample"
avg.k<- avg.k[,!(colnames(avg.k)%in%"NA's")]

avg.freq.tTaxes <- cbind(rownames(avg.freq.tTaxes),avg.freq.tTaxes)
colnames(avg.freq.tTaxes)[1] <- "Respondent subsample"

print(xtable(avg.k,digits=1),  file=paste(survey.output.dir ,"avg_k",".tex",sep=""), include.rownames=F)


tmp.tab.out<- xtable(avg.freq.tTaxes,digits=1)
vv.c <-nchar(names(tmp.tab.out))>8
vv.c <- c(F,vv.c)
vv<- align(tmp.tab.out)[vv.c]
vv[vv %in% "r"]<- "L{2cm}"
align(tmp.tab.out)[vv.c]<- vv

print(tmp.tab.out,  file=paste(survey.output.dir ,"avg_freq_tTaxes",".tex",sep=""), include.rownames=F)

print(avg.k)
print(avg.freq.tTaxes)


time.to.next.tTaxes.summary<- get.summary.avg.freq.tTaxes(avg.freq.tTaxes)
time.to.next.tTaxes.summary$N <- avg.k$N

tmp.tab.out<- xtable(time.to.next.tTaxes.summary,digits=2)
vv.c <-nchar(names(tmp.tab.out))>8
vv.c <- c(F,vv.c)
vv<- align(tmp.tab.out)[vv.c]
vv[vv %in% "r"]<- "L{2cm}"
align(tmp.tab.out)[vv.c]<- vv

print(tmp.tab.out,  file=paste(survey.output.dir ,"avg_time_to_next_tTaxes_summary",".tex",sep=""), include.rownames=F)



avg.k.dist <- alters.table[["altertaxhowoften"]]$n.alters.tTaxes
dat<- data.frame(X= avg.k.dist)
plot.rate <- ggplot(dat, aes(x = X)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)),
                 bins=11, fill="cornflowerblue",color="black") +
  theme_bw() +
  guides(fill = guide_legend(title=""))+
  ylab("Percent")+
  xlab("number of alters who respondents talk about taxes with")+
  #ggtitle(tab.label) +
  scale_x_continuous(breaks=2*c(0:5))+
  theme(axis.text.x=element_text(size=16,angle=0 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))
lab <- "Histogram number of alters who respondents talk about taxes with"
suppressWarnings(ggsave(plot.rate,
       file=paste(survey.output.dir,gsub(" ","_", lab),".pdf",sep=""),
       width=10, height=5))



### average number of self-self employed

SE.SE<-colMeans(alters.table$alterselfemployed[subsets$selfemployed,4:6],na.rm=T)
nSE.nSE <- colMeans(alters.table$alterselfemployed[subsets$not.selfemployed,4:6],na.rm=T)

tmp<- rbind(SE.SE,nSE.nSE)
rownames(tmp)<- c("SE","nSE")
colnames(tmp)[1:2] <- c("SE","nSE")

tmp <- data.frame(rownames(tmp),tmp)
colnames(tmp)[1] <- "selfemployment mixing matrix "

print(xtable(tmp,digits=1),  
      file=paste(survey.output.dir,"mix_mat_SE",".tex",sep=""), include.rownames=F)


A.A<-colMeans(alters.table$altertaxaudit[subsets$audited,4:8],na.rm=T)
nA.nA <- colMeans(alters.table$altertaxaudit[subsets$nonaudited,4:8],na.rm=T)

A.A<- c(A=sum(A.A[1:2]), nA= sum(A.A[4:5]),DK=sum(A.A[3]))
nA.nA<- c(A=sum(nA.nA[1:2]), nA= sum(nA.nA[4:5]),DK=sum(nA.nA[3]))


tmp<- rbind(A.A,nA.nA)
rownames(tmp)<- c("A","nA")
colnames(tmp)[1:2] <- c("A","nA")

tmp <- data.frame(rownames(tmp),tmp)
colnames(tmp)[1] <- "ever audited mixing matrix"
print(xtable(tmp,digits=1),file=paste(survey.output.dir,"mix_mat_A",".tex",sep=""), 
      include.rownames=F)


x <- alters.table$altertaxaudit.tTaxes[subsets$audited,]
x<- x[x$n.alters.tTaxes>0,]
A.A<-colMeans(x[,4:9],na.rm=T)

x <- alters.table$altertaxaudit.tTaxes[subsets$nonaudited,]
x<- x[x$n.alters.tTaxes>0,]
nA.nA <- colMeans(x[,4:9],na.rm=T)

A.A<- c(A=sum(A.A[1:2]), nA= sum(A.A[4:5]),DK=sum(A.A[3]))
nA.nA<- c(A=sum(nA.nA[1:2]), nA= sum(nA.nA[4:5]),DK=sum(nA.nA[3]))

tmp<- rbind(A.A,nA.nA)
rownames(tmp)<- c("A","nA")
colnames(tmp)[1:2] <- c("A","nA")

tmp <- data.frame(rownames(tmp),tmp)
colnames(tmp)[1] <- "ever audited mixing matrix"
print(xtable(tmp,digits=1),file=paste(survey.output.dir,"mix_mat_A_tTaxes",".tex",sep=""), 
      include.rownames=F)


### Write to Model Inputs 

k.all <- avg.k0["all","Mean"]
k.all.tT <- avg.k["all","Mean"]
k.all.tT_LB <- avg.k["all","Median"]
k.all.tT_UB <- avg.k["all","3rd Qu."]

k.se<-avg.k0["selfemployed","Mean"]
k.nse<-avg.k0["not.selfemployed","Mean"]
k.se.tT<-avg.k["selfemployed","Mean"]
k.nse.tT<-avg.k["not.selfemployed","Mean"]

se.se.prop<-tab.out$table_alterselfemployed["selfemployed","Yes"]/100
se.se.tT.prop<- tab.out$table_alterselfemployed_tTaxes["selfemployed","Yes"]/100
nse.nse.prop<-tab.out$table_alterselfemployed["not.selfemployed","No"]/100
nse.nse.tT.prop<- tab.out$table_alterselfemployed_tTaxes["not.selfemployed","No"]/100

M.se.se <- k.se.tT*se.se.tT.prop/(k.se*se.se.prop)
M.se.nse<-k.se.tT*(1-se.se.tT.prop)/(k.se*(1-se.se.prop))
M.nse.nse<- k.nse.tT*nse.nse.tT.prop/(k.nse*nse.nse.prop)
M.nse.se <- k.nse.tT*(1-nse.nse.tT.prop)/(k.nse*(1-nse.nse.prop))

audit.eff.se <-mean(ego.data$n.alters.tTaxes[subsets$selfemployed& subsets$audited ],na.rm=T)/mean(ego.data$n.alters.tTaxes[subsets$selfemployed& subsets$nonaudited ],na.rm=T)

audit.eff.nse <-mean(ego.data$n.alters.tTaxes[subsets$not.selfemployed& subsets$audited ],na.rm=T)/mean(ego.data$n.alters.tTaxes[subsets$not.selfemployed& subsets$nonaudited ],na.rm=T)

audit.eff <- mean(audit.eff.se,audit.eff.nse )

tab.tmp <- c(k.all=k.all,  k.all.tT_Mean=k.all.tT, k.all.tT_LB =k.all.tT_LB, k.all.tT_UB= k.all.tT_UB, M.se.se=M.se.se,M.se.nse=M.se.nse, M.nse.se=M.nse.se,M.nse.nse=M.nse.nse,
             audit.eff=audit.eff)

model.param.tab$network.degree.manipulations <- tab.tmp 

tab.tmp<- data.frame(tab.tmp)
rownames(tab.tmp) <- c("mean degree","mean degree talk taxes","LB degree talk taxes","UB degree talk taxes","prop degree SE-SE", "prop degree SE-NSE",
                       "prop degree NSE-SE", "prop degree NSE-NSE", "audit multiplicative effect")
tab.tmp<- data.frame(rownames(tab.tmp),tab.tmp)
colnames(tab.tmp) <- c("degree or effect on the degree", "value")
print(xtable(tab.tmp,digits=2,auto=T),file=paste(survey.output.dir,"Model_inputs_degree",".tex",sep=""), 
      include.rownames=F)


########################################
###                                  ###
###    Analyze questions perceives T, q, P
###                                  ###
########################################

### clean data find cutoff in the long tails.
SD.clean<- suppressWarnings(clean.perceprtions(SD,apply.cuttoff=FALSE))

cleaning.cutoffs<-SD.clean$cutoff.list
SD.clean.quantile <- SD.clean$quantile
SD.clean<-SD.clean$SD.cleaned


cleaning.cutoffs <- data.frame(rownames(cleaning.cutoffs),cleaning.cutoffs)
colnames(cleaning.cutoffs)[1] <- "question/field"
print(xtable(cleaning.cutoffs,digits=1),  
      file=paste(survey.output.dir,"cleaning.cutoffs",".tex",sep=""), include.rownames=F)

SD.clean.quantile<-SD.clean.quantile[,13:21]
SD.clean.quantile.df<- data.frame(rownames(SD.clean.quantile),SD.clean.quantile)
colnames(SD.clean.quantile.df) <- c("question/field",names(SD.clean.quantile))
print(xtable(SD.clean.quantile.df,digits=1),  
      file=paste(survey.output.dir,"SD.clean.quantile",".tex",sep=""), include.rownames=F)

SD.clean$perceivedevasionratepopulation <- suppressWarnings(as.numeric(SD.clean$perceivedevasionratepopulation))

for(subset.label  in c(names(subsets)[-1],"all")){
  
  subset <- subsets[,subset.label]
  subset[is.na(subset)] <-F
  print(subset.label)
  print( table(subset),sep=" ")
  df<-SD.clean[subset,]
  tab <- list()
  
  ### Question 8:  Get Perceived audit rate
  per.aud.rate <- as.numeric(df[,"perceivedauditrate"])
  tab$perceivedauditrate <- summaryfunctionFull(per.aud.rate)
  
  
  ### Question 9:  Get Perceived audit rate if taxpayer is under-reporting
  tab.ARUP<-get.ARUP.tables(df)
  tab.ARUP1<-as.data.frame(tab.ARUP[[1]])
  tab.ARUP2<-as.data.frame(tab.ARUP[[2]])
  
  colnames(tab.ARUP1)[colnames(tab.ARUP1)%in%"V7"]<- "NA's"
  colnames(tab.ARUP2)[colnames(tab.ARUP2)%in%"V7"]<- "NA's"
  
  suppressWarnings(print(xtable(tab.ARUP1,auto=T,digits=2),  file=paste(survey.output.dir,"tab_ARUP1_",subset.label,".tex",sep="")))
  suppressWarnings(print(xtable(tab.ARUP2,auto=T,digits=2),  file=paste(survey.output.dir,"tab_ARUP2_",subset.label,".tex",sep="")))
  
  ### Question 10:  Bomb-Crater
  tab.BC<- as.data.frame(get.BC.tables(df)[[1]])
  colnames(tab.BC) <- colnames(tab.ARUP2) 
  suppressWarnings(print(xtable(tab.BC,auto=T,digits=2),  file=paste(survey.output.dir,"tab_BC_",subset.label,".tex",sep="")))
  
  ### Question 11:  Get Perceived penalty
  per.pen.rate <- df[,"perceivedpenaltyrate"]
  tab$perceivedpenaltyrate <- summaryfunctionFull(per.pen.rate)
  
  ### Question 12:  Get Perceived effective tax rate
  per.tax.rate <- df[,"perceivedtaxrate"]
  tab$perceivedtaxrate <- summaryfunctionFull(per.tax.rate)
  
  ### Question 13:  Get Perceived evasion rate in the population 
  per.eva.rate.pop<-as.numeric(df[,"perceivedevasionratepopulation"])
  tab$perceivedevasionratepopulation <- summaryfunctionFull(per.eva.rate.pop)
  
  ### Question 14:  Get Perceived evasion rate amongst similar people.
  per.eva.rate<-df[,"perceivedevasionrate"]
  tab$perceivedevasionrate<- summaryfunctionFull(per.eva.rate)
  
  ### Question 15:  Get Perceived evasion rate if Many evaders
  per.eva.rate.ME<-df[,"perceivedevasionmanyevaders"]
  tab$perceivedevasionmanyevaders<- summaryfunctionFull(per.eva.rate.ME)
  tab$perceivedevasionmanyevadersProp<- summaryfunctionFull(per.eva.rate.ME[per.eva.rate>0]/
                                                  per.eva.rate[per.eva.rate>0])
  
  ### Question 16:  People caught by the IRS 
  per.caught <- df[,"perceivedcaught"]
  tab$perceivedcaught<- summaryfunctionFull(per.caught)
  tab$perceivedcaughtProp<-summaryfunctionFull(per.caught[per.aud.rate>0]/per.aud.rate[per.aud.rate>0])
  
  ### Save summary table
  tab <- as.data.frame(do.call("rbind",tab))
  tab <- tab[,!(colnames(tab)%in%"NA's")]
  print(xtable(tab,auto=T,digits=1),  file=paste(survey.output.dir,"perceptions_summary_",subset.label,".tex",sep=""))
}


####Do some correlations:

tmp.df <- SD.clean[,c("perceivedauditrate","perceivedpenaltyrate","perceivedtaxrate",
                      "perceivedcaught")]

tmp.cor <- cor(tmp.df,use="pairwise.complete.obs")
print(xtable(tmp.cor ,auto=T,digits=2),  file=paste(survey.output.dir,"perceptions_correlations_",subset.label,".tex",sep=""))

tmp.df <- SD.clean[,c("perceivedauditrate",
                      "perceivedevasionratepopulation","perceivedevasionrate",
                      "perceivedevasionmanyevaders")]

tmp.cor <- cor(tmp.df,use="pairwise.complete.obs")
print(xtable(tmp.cor ,auto=T,digits=2),  file=paste(survey.output.dir,"perceptions_correlations2_",subset.label,".tex",sep=""))



gamb.fallacy <- as.data.frame(cbind(evasion= 1-c(0.3,0.6,0.9),perc.q.fact=tab.ARUP2$`3rd Qu.`))
gamb.fallacy.model <-lm(log(gamb.fallacy$perc.q.fact)~log(gamb.fallacy$evasion))
## sanity check at very low evasion of 0.5%
exp(0.9803 +0.1829 *log(0.005))-1<0.02
gamb.fallacy.model3 <- gamb.fallacy.model
gamb.fallacy <- as.data.frame(cbind(evasion= 1-c(0.3,0.6,0.9),perc.q.fact=tab.ARUP2$`Median`))
gamb.fallacy.model <-lm(log(gamb.fallacy$perc.q.fact)~log(gamb.fallacy$evasion))
gamb.fallacy.model2 <- gamb.fallacy.model




library(texreg)
reg.out <- texreg(list(gamb.fallacy.model2,gamb.fallacy.model3),
                  caption="",
                  single.row=T, center=F,
                  dcolumn=FALSE)

print(reg.out,file=paste("Writeup/Tables/","GF_reg",".tex",sep=""))




tmp<-gamb.fallacy.model3$coefficients
tmp <- rbind(tmp,gamb.fallacy.model$coefficients)
tmp<-rbind(tmp,c(0,0))
rownames(tmp) <- 1:3

# gamb.fallacy <- round(c(
#   (tab.ARUP2$`1st Qu.`[1]), 
#   (tab.ARUP2$Median[1]),
#   (tab.ARUP2$`3rd Qu.`[1])),2)

gamb.fallacy<- tmp[rev(rownames(tmp)),]
rownames(gamb.fallacy) <- c("LB","Mode","UB")

model.param.tab$gamb.fallacy.effect <- gamb.fallacy

bomb.crater <- tab.BC$Median[c(2,3,1)] 
names(bomb.crater) <- c("LB","Mode","UB")

model.param.tab$bomb.crater.effect <- bomb.crater
#get.BC.factor(df)
#get.ARUP.factor(df) 

# However, in a study by Kirchler, Maciejovsky and Schwarzenberger that was based on an a laboratory experiment, the authors considered a 50\% effective tax rate, 50\% sanction rate and a 30\% audit risk and they also found that compliance decreases immediately after an audit \cite{Kirchler2003a} to 58%. So here we replace the mode with 58% 

model.param.tab$bomb.crater.effect["Mode"] <- 0.58


########################################
###                                 
###     Analysis of the Pervieved Evasion Rate (PER)              
###                                 
########################################

### Analysis of Question 13,14 and 15.

subset <- subsets[,"all"]
df<-SD.clean[subset,]
dat <- data.frame(per.eva.rate.pop,per.eva.rate,per.eva.rate.ME)
df <- melt(dat, variable.name= "Under_Reporting", value.name = "percentage")
names(df) <- c("Under_Reporting","percentage")
df<- df[!is.na(df$percentage),]

suppressWarnings(source("Survey code/ALP_PER_Analysis.R"))
print(xtable(media.PER,digits=2),  file=paste(survey.output.dir,"ALP_Media.PER",".tex",sep=""))

########################################
###                                 
###     Using the Analysis of PER to model effect of media on tax moral              
###                                 
########################################

GT_Media_effect_on_morale<-t(readRDS(file=paste(survey.data.dir,"GT_Media_effect_on_morale.Rdata",sep="")))
Med.Eff.morale <- rbind(GT_Media_effect_on_morale,GT_Media_effect_on_morale)
Med.Eff.morale <- as.data.frame(Med.Eff.morale)
rownames(Med.Eff.morale) <- c("ALP media effect","GT media effect")
Med.Eff.morale["ALP media effect","m"] <- media.PER["m","Median"]
Med.Eff.morale["ALP media effect","s"] <- media.PER["s","Median"]
Med.Eff.morale["ALP media effect","K"] <- NA

media.sampling.model<-lm(Med.Eff.morale$s~Med.Eff.morale$m)
media.sampling.model$coefficients

model.param.tab$Med.Eff.morale<- list(Med.Eff.morale=Med.Eff.morale,media.sampling.model=media.sampling.model)
model.param.tab$Med.Eff.morale$tax.rate.effect.point<-round(summaryfunctionFull(SD.clean$perceivedtaxrate)$Mean,0)

########################################
###                                  
###    Plot analysis of questions percieve effective Tax, audit , Penalty rates
###                                  
########################################

lab <- paste("percieved effective tax rate",subset.label,sep=" ")
r<- do.ALP.per.rate.analysis(per.rate=per.tax.rate ,
                             lab=lab,scale.up=2.5, 
                             n.bins=20,xmax=100)

r$summary.rate
print(r$plot.rate)
ggsave(r$plot.rate,
       file=paste(survey.output.dir,gsub(" ","_", lab),".pdf",sep=""),
       width=10, height=5)

lab <- paste("log10 percieved effective tax rate",subset.label,sep=" ")


suppressWarnings(print(r$plot.log.rate))
suppressWarnings(ggsave(r$plot.log.rate,
       file=paste(survey.output.dir,gsub(" ","_", lab),".pdf",sep=""),
       width=10, height=5))
#saveRDS(r$fit.log.rate,file=paste(survey.data.dir,gsub(" ","_", lab),".Rdata",sep=""))


lab <- paste("percieved penalty rate",subset.label,sep=" ")
r<- do.ALP.per.rate.analysis(per.rate=per.pen.rate,
                             lab=lab,scale.up=5, n.bins=20,xmax=100)

r$summary.rate
suppressWarnings(print(r$plot.rate))
suppressWarnings(ggsave(r$plot.rate,
       file=paste(survey.output.dir,gsub(" ","_", lab),".pdf",sep=""),
       width=10, height=5))

lab <- paste("log10 percieved penalty rate",subset.label,sep=" ")


suppressWarnings(print(r$plot.log.rate))
suppressWarnings(ggsave(r$plot.log.rate,
       file=paste(survey.output.dir,gsub(" ","_", lab),".pdf",sep=""),
       width=10, height=5))


###  save log10 Dist of Penalty Rate  MODEL PARAMETER
#saveRDS(r$fit.log.rate,file=paste(survey.data.dir,gsub(" ","_", lab),".Rdata",sep=""))


lab <- paste("percieved audit rate",subset.label,sep=" ")
r<- do.ALP.per.rate.analysis(per.rate=per.aud.rate ,lab=lab,scale.up=5, n.bins=50)
suppressWarnings(print(r$plot.rate))
r$summary.rate
suppressWarnings(ggsave(r$plot.rate,file=paste(survey.output.dir,gsub(" ","_", lab),".pdf",sep=""),
       width=10, height=5))
lab <- paste("log10 percieved audit",subset.label,sep=" ")
suppressWarnings(print(r$plot.log.rate))
suppressWarnings(ggsave(r$plot.log.rate,file=paste(survey.output.dir,gsub(" ","_", lab),".pdf",sep=""),
       width=10, height=5))

###  save log10 Dist of Audit Rate  MODEL PARAMETER
#saveRDS(r$fit.log.rate,file=paste(survey.data.dir,gsub(" ","_", lab),".Rdata",sep=""))

### OTHER TABLES OF INTEREST THAT APPEAR IN THE SURVEYQUESTION.TEX ARE 
### COMPUTED IN THE BEHAVIORAL_REGRESSION_MODELS.R CODE



########################################
###                                  ###
###    Analyze Hypothetical Scenarios questions 17 to 26:
###                                  ###
########################################

df.hypo<- clean.hypotheticals(SD.clean, allow.reversion= FALSE)
tab <- df.hypo$tab
df.ori <- df.hypo$df.ori
df.hypo <- df.hypo$df.out
dim(df.hypo)
tmp <- unique(df.hypo$prim_key)
tmp <- tmp[!is.na(tmp)]
print(paste("number of respondents in Analyze Hypothetical Scenarios questions is ", length(tmp)))


write.csv(df.ori, paste(survey.data.dir,"Compliance_Hypotheticals_RAW",".csv",sep=""),row.names = F)

tmp.tab.out<- xtable(tab,digits=1)
tab.label <- "creating_hypo_data.set"
print(tmp.tab.out,  
      file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F)

write.csv(df.hypo, paste(survey.data.dir,"Compliance_Hypotheticals_filtered",".csv",sep=""),row.names = F)

df.hypo<- create.df.hypotheticals(df.hypo)
dim(df.hypo)
print(xtable(df.hypo[1:35,]),file=paste(survey.output.dir,
           "Compliance_Hypotheticals",".tex",sep=""),
      include.rownames=FALSE)

tmp <- unique(df.hypo$prim_key)
tmp <- tmp[!is.na(tmp)]
print(paste("number of respondents in Analyze Hypothetical Scenarios questions is ", length(tmp)))

write.csv(SD.clean, paste(survey.data.dir,"SD.clean",".csv",sep=""),row.names = F)


suppressWarnings(source("Survey code/Do_Plots_for_hypotheticals.R"))


########################################
###                                  ###
###    Save IMAGE
###                                  ###
########################################

save.image(paste(survey.data.dir,"tmp.image",".Rdata",sep=""))

load(paste(survey.data.dir,"tmp.image",".Rdata",sep=""))

########################################
###                                  ###
###    Analyze question 27:
###                                  ###
########################################


### Analyze question 27: Extract c1 range = the range in tax rates where people 
### begin to under-report given 0 percieved audit and penalty rates.

for(subset.label  in c("selfemployed","not.selfemployed","all")){
  
  
  subset <- subsets[,subset.label]
  subset[is.na(subset)] <-F
  #subset <- subset & SD.clean$prim_key%in%unique(df.hypo$prim_key)
  
  tmp <- grepl("perceivedunderreportauditpenalty_",names(SD.clean)) & 
    !(grepl("intro",names(SD.clean)) )
  
  
  puap<- SD.clean[subset,tmp] # puap = Perceived Underreport Audit Penalty
  
  c1.analysis <- do.ALP.c1.analysis(puap,c2=0.7)
  
  tab.label <- paste("ALP_c1_data",subset.label,sep="_")
  tmp2<- 100*c1.analysis$c1.counts.table/sum(c1.analysis$c1.counts.table)
  tmp2<-rbind(c1.analysis$c1.counts.table,tmp2)
  rownames(tmp2)<- c("counts","percentage")
  print(xtable(tmp2,auto=T,digits=1),  file=paste(survey.output.dir,tab.label ,".tex",sep=""))
  tab.label<- paste("ALP_c1_fit",subset.label,sep="_")
  ggsave(c1.analysis$plot,file=paste(survey.output.dir,tab.label,".pdf",sep=""),
         width=10, height=5)
  print(c1.analysis$plot)
  print(xtable(c1.analysis$mean.puap.range,auto=T,digits=1),  
        file=paste(survey.output.dir ,tab.label,".tex",sep=""))
  
  ###  save c1 Distribution MODEL PARAMETER
  #saveRDS(c1.analysis$fit,file=paste(survey.data.dir,tab.label,".Rdata",sep=""))
}## make sure "all" is done last


SD.clean[,colnames(c1.analysis$puap.range)] <- NA 
SD.clean[rownames(c1.analysis$puap.range),colnames(c1.analysis$puap.range)] <- c1.analysis$puap.range
SD.clean$c1.guessed.unmodified <- SD.clean$c1.guessed


df.hypo <- merge(df.hypo,
                 SD.clean[,c("prim_key", 
                                "tax.rate.threshold.min",
                                "tax.rate.threshold.max",
                                "c1.guessed.unmodified", 
                                "c1.guessed")]
                  , by= "prim_key")

print(xtable(df.hypo[1:55,]),file=paste(survey.output.dir,
                                        "Compliance_Hypotheticals_full",".tex",sep=""),
      include.rownames=FALSE)


write.csv(df.hypo, paste(survey.data.dir,"Compliance_Hypotheticals_filtered_expanded",".csv",sep=""),row.names = F)


########################################
###                                  ###
###    Calculate Elasticities 
###                                  ###
########################################


### first lets calculate Elasticity of evasion proportion on tax rate (Delta E/E) /(Delta T/T) 
### Thus we do a lm of ln E ~ ln T where 
out<- list()
for(control in c("taxrate","auditrate","penaltyrate")){
  out[[control]]<- get.estimated.elasticity(df.hypo,control)
  #print(xtable(out[[control]],digits=1),file=paste(survey.output.dir ,"Elast_",control,".tex",sep=""))
}


#### Run a panel level linear model
df.hypo.tmp <- df.hypo
df.hypo.tmp<- merge(df.hypo.tmp,SD.clean[,c("prim_key","gender","calcage","selfemployed","workforpay")],by ="prim_key")
tmp <- data.frame(prim_key=SD.clean$prim_key,
             income= ALP.combineIncome(SD.clean$familyincome,
                                       SD.clean$familyincome_part2, categorical =FALSE) )
df.hypo.tmp<- merge(df.hypo.tmp,tmp,by ="prim_key")
df.hypo.tmp$log.perceivedtaxrate <- log(df.hypo.tmp$perceivedtaxrate)
df.hypo.tmp$log.perceivedauditrate <- log(df.hypo.tmp$perceivedauditrate)
df.hypo.tmp$log.perceivedpenaltyrate <- log(df.hypo.tmp$perceivedpenaltyrate)
df.hypo.tmp$log.perceivedevasionrate <- log(df.hypo.tmp$perceivedevasionrate)
df.hypo.tmp$log.income <- log(df.hypo.tmp$income)
df.hypo.tmp$log.age <- log(df.hypo.tmp$calcage)
df.hypo.tmp$selfemployed[is.na(df.hypo.tmp$selfemployed)] <- 1

plm.model <- plm(log.perceivedevasionrate~ 
                   log.perceivedtaxrate  + 
                   log.perceivedauditrate + 
                   log.perceivedpenaltyrate+ 
                   log.age+
                   #selfemployed+ ## selfemployment cuts the number of respondents.
                   log.income,
                   data = df.hypo.tmp, 
                   effect = "individual", 
                   model = "random", 
                   index = "prim_key")

summary(plm.model)
summaryfunctionFull(fixef(plm.model))



########################################
###                                  ###
###    Calculate sigmoid response of E on T
###                                  ###
########################################
### Proceed with S-curve fit doing a brute force fixed effects model 

c2<-0.7
Evasion.Tax.Scurve<- get.ET.lognormal.fit(df.hypo,c2=c2)
Evasion.Tax.Scurve$prim_key <- rownames(Evasion.Tax.Scurve)

print(paste("correlation between m and s in the ET S-curve plot",
            round(cor(Evasion.Tax.Scurve$s,Evasion.Tax.Scurve$m),2), sep =" "))

dim(Evasion.Tax.Scurve)

Evasion.Tax.Scurve.parameters <- rbind(s=c(summary(Evasion.Tax.Scurve[,1])),
                       m=c(summary(Evasion.Tax.Scurve[,2])))



Evasion.Tax.Scurve.parameters <- signif(Evasion.Tax.Scurve.parameters,3)

ET.LMU<- as.data.frame(Evasion.Tax.Scurve.parameters[,2:5])


ET.LMU[,2]<- apply(ET.LMU[,2:3], 1, min) 
ET.LMU[,3]<- ET.LMU[,4]
ET.LMU<-ET.LMU[,1:3]
colnames(ET.LMU)<- c("Lower","Mode","Upper")

m <- ET.LMU["m","Mode"]
s <- ET.LMU["s","Mode"]

g <- ET.LMU["s",]/(ET.LMU["m",]*sqrt(2*pi))

tax.vals <- seq(0,1,0.0001)
ET.dat <- as.data.frame(cbind( x=tax.vals,y=cdf.lognormal(x=tax.vals, log(m),1/s)))
ET.datL <- as.data.frame(cbind( x=tax.vals,
                               y=cdf.lognormal(x=tax.vals, log(ET.LMU["m","Lower"]),1/ET.LMU["s","Lower"])))
ET.datU <- as.data.frame(cbind(x=tax.vals,
                               y=cdf.lognormal(x=tax.vals, log(ET.LMU["m","Upper"]),1/ET.LMU["s","Upper"])))


plot(ET.dat$x,ET.dat$y,type="l",xlab="tax.vals",ylab="Phi",lwd = 2, 
     col = "dark red", main = "multiplicative factor in the tilde c1 equation")

ET.slope <- s/(m*sqrt(2*pi))  ### this comes from the PDF of the log normal evaluated at x=m
ET.intercept <- 0.5-ET.slope*m


p1<-ggplot(ET.dat) +
  geom_line(aes(x=x, y=y),size=2,color="purple")+
  geom_line(data= ET.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  geom_line(data= ET.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  xlim(0,1)+ylim(0,1)+
  xlab("Perceived Effective Tax Rate") +
  ylab("PER people like you \n with perceived deterrence")+
  #ggtitle(expression(paste("Multiplicative factor in the ",tilde(c)[1], " equation",sep=" ")))+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))+
  geom_vline(xintercept = m,  linetype="dotted") +
  geom_hline(yintercept = 0.5,  linetype="dotted")+
  geom_abline(intercept = ET.intercept, slope = ET.slope,size=1,color="darkgreen", alpha=0.5)+
  annotate("text",label="m", x=m,y=-0.0, size = 10, colour = "black", parse=TRUE)+
  annotate("text",label = "tilde(c)[1]^{(i)}",parse = TRUE,
           x=-ET.intercept/ET.slope-0.03,y=0.1, size = 10, colour = "darkgreen")+
  annotate("text",label = "g[m]",parse = TRUE,angle=20,
           x=0.45,y=0.9, size = 10, colour = "darkgreen")+
  geom_vline(xintercept = -ET.intercept/ET.slope, colour = "darkgreen",  linetype="dotted") 
p1



m2 <- ET.LMU["m","Lower"]
s2 <- ET.LMU["s","Lower"]
ET.slope2 <- s2/(m2*sqrt(2*pi))  ### this comes from the PDF of the log normal evaluated at x=m
ET.intercept2 <- 0.5-ET.slope2*m2

m3 <- ET.LMU["m","Upper"]
s3 <- ET.LMU["s","Upper"]
ET.slope3 <- s3/(m3*sqrt(2*pi))  ### this comes from the PDF of the log normal evaluated at x=m
ET.intercept3 <- 0.5-ET.slope3*m3


p2<-ggplot(ET.dat) +
  geom_line(aes(x=x, y=y),size=2,color="red")+
  geom_line(data= ET.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.3,  linetype="dotdash")+
  geom_line(data= ET.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.3,  linetype="dotdash")+
  # geom_point(data = fixed.ET.dat1,aes(x = x, y = y),size=6,color="navy",alpha=0.7)+
  # geom_point(data = fixed.ET.dat2,aes(x = x, y = y),size=6,color="blue",alpha=0.4)+
  # geom_point(data = fixed.ET.dat3,aes(x = x, y = y),size=6,color="red",alpha=0.4)+
  xlim(0,1)+ylim(0,1)+
  xlab("") +
  ylab("")+
  # #ggtitle(expression(paste("Multiplicative factor in the ",tilde(c)[1], " equation",sep=" ")))+
  theme(axis.text.x=element_text(size=16 ),
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))+
   geom_vline(xintercept = m,  linetype="dotted") +
   geom_hline(yintercept = 0.5,  linetype="dotted")+
   geom_abline(intercept = ET.intercept, slope = ET.slope,size=1,color="darkgreen", alpha=0.5)+
  geom_abline(intercept = ET.intercept2, slope = ET.slope2,size=1,color="darkgreen", alpha=0.5)+
   geom_abline(intercept = ET.intercept3, slope = ET.slope3,size=1,color="darkgreen", alpha=0.5)
  # geom_abline(intercept = ET.intercept2[3], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  # geom_abline(intercept = ET.intercept2[4], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  # geom_vline(xintercept = -ET.intercept/ET.slope, colour = "darkgreen",  linetype="dotted") 
p2
ggsave(p2,file=paste(survey.output.dir,"lognormal_Evasion_Tax_Scurve_lognormal_fit_v3",".pdf",sep=""),
       width=8, height=4)



m.range.factor<-ET.LMU["m",]/ET.LMU["m","Mode"]
### run the get.ET.lognormal.fit.plm code before - unfinished function. 
#Evasion.Tax.Scurve.plm<- get.ET.lognormal.fit.plm(df.hypo.tmp)




Evasion.Tax.Scurve.parameters <- rbind(s=c(summaryfunctionFull(Evasion.Tax.Scurve[,1])),m=c(summaryfunctionFull(Evasion.Tax.Scurve[,2])), g=c(summaryfunctionFull(Evasion.Tax.Scurve[,5])))

print(xtable(Evasion.Tax.Scurve.parameters,digits=3),  
      file=paste(survey.output.dir ,"Evasion_Tax_Scurve_parameters",".tex",sep=""))
print(xtable(Evasion.Tax.Scurve.parameters[2:3,c(2:3,6)],digits=3),  
      file=paste(survey.output.dir ,"Evasion_Tax_Scurve_parameters_reduced",".tex",sep=""))
ggsave(p1,file=paste(survey.output.dir,"Evasion_Tax_Scurve_lognormal_fit",".pdf",sep=""),
       width=8, height=4)

source("Logit_Evasion_Tax_Scurve.R")

########################################
###
###    Do first corrections of c1 on question 27
###
########################################


df <- df.hypo[df.hypo$control%in%c("baseline"),
              c("prim_key", "perceivedtaxrate",
                "perceivedevasionrate",
                "tax.rate.threshold.min","tax.rate.threshold.max")]


## Respondent Sanity 1:
### if my c1>my perceied effective tax rate => percieved evasion is 0 in people like me
case1<-((df$perceivedtaxrate<df$tax.rate.threshold.max) & df$perceivedevasionrate<=50)
### if my c1< perceied effective tax rate => percieved evasionis +ve in people like me
case2<-((df$perceivedtaxrate>df$tax.rate.threshold.min) & df$perceivedevasionrate>=50)
### if my c1< perceied effective tax rate => percieved evasionis +ve in people like me
case3<-((df$perceivedtaxrate>=df$tax.rate.threshold.min) & 
          (df$perceivedtaxrate<=df$tax.rate.threshold.max) & 
          df$perceivedevasionrate>=10 & df$perceivedevasionrate<=10)
table(case1 | case2 |case3)


## Respondent Sanity 1:
### if my c1>my perceied effective tax rate => percieved evasion is 0 in people like me
case1<-((df$perceivedtaxrate<df$tax.rate.threshold.max) & df$perceivedevasionrate<=75)
### if my c1< perceied effective tax rate => percieved evasionis +ve in people like me
case2<-((df$perceivedtaxrate>df$tax.rate.threshold.min) & df$perceivedevasionrate>=25)
### if my c1< perceied effective tax rate => percieved evasionis +ve in people like me
case3<-((df$perceivedtaxrate>=df$tax.rate.threshold.min) & 
          (df$perceivedtaxrate<=df$tax.rate.threshold.max) & 
          df$perceivedevasionrate>=10 & df$perceivedevasionrate<=10)
table(case1 | case2 |case3)


### Correct c1 values

df <- correct.c1.with.hypotheticals(df.hypo, ET.slope=ET.slope) 

#hist(df$c1.guessed)

tmp <- unique(df[,c("prim_key","c1.guessed")])
positions <- match(tmp$prim_key,SD.clean$prim_key)
SD.clean[positions,"c1.guessed"]<- tmp$c1.guessed

df.hypo<- df.hypo[,!(colnames(df.hypo)%in%"c1.guessed")]
df.hypo<- merge(df.hypo,
                SD.clean[,c("prim_key","c1.guessed")]
                , by= "prim_key")

tmp <- list()
for(subset.label  in c("selfemployed","not.selfemployed","all")){
  
  
  subset <- subsets[,subset.label]
  subset[is.na(subset)] <-F
  puap<- SD.clean[subset,]
  
  puap <- puap[puap$prim_key%in%df.hypo$prim_key,]
  
  c1.analysis <-correct.fit.c1.with.hypotheticals(puap, adjust = 2, c2 = 0.7)
  tmp[[subset.label]] <- c1.analysis$means
  tab.label<- paste("ALP_c1_fit_corrected",subset.label,sep="_")
  ggsave(c1.analysis$plot,file=paste(survey.output.dir,tab.label,".pdf",sep=""),
         width=10, height=5)
  print(c1.analysis$plot)
}
tmp <- do.call("rbind",tmp)

print(xtable(tmp,digits=1,auto=T),file=paste(survey.output.dir, "ALP_c1_fit_corrected_table_means",".tex",sep=""))

###  save c1 Distribution MODEL PARAMETER
model.param.tab$c1$c1.ALP.fit <- c1.analysis$fit

########################################
###                                  ###
###    Explorations of alternative ways of getting c1 
###                                  ###
########################################



c1.dist.summaries <- list()
if(!"c1.guessed.unmodified"%in%colnames(df.hypo)){
  df.hypo <- merge(df.hypo,SD.clean[,c("prim_key","c1.guessed.unmodified")],by="prim_key",all.x=T )
}
c1.dist <- unique(df.hypo[,c("prim_key","c1.guessed","c1.guessed.unmodified")], 
                  by=c("prim_key","c1.guessed.unmodified"))
keep.the.removed <- c1.dist[is.na(c1.dist$c1.guessed),]
c1.dist<- c1.dist[!is.na(c1.dist$c1.guessed),]
c1.dist$c1.guessed <- c1.dist$c1.guessed /100
c1.dist$c1.guessed.unmodified <- c1.dist$c1.guessed.unmodified /100

ET.slope <- s/(m*sqrt(2*pi))  ### this comes from the PDF of the log normal evaluated at x=m
intercepts <- 0.5-ET.slope*c1.dist$c1.guessed.unmodified
ET.slope.c1.tilde <- ET.slope

c1.dist.comp <- -intercepts/ET.slope
c1.dist.comp.v2 <- c1.dist.comp
c1.dist.comp.v2[c1.dist.comp<0] <-  c1.dist$c1.guessed[c1.dist.comp<0]
c1.dist.comp[intercepts>0]<- 0

hist(c1.dist.comp,breaks=20)
c1.dist.summaries[["c1.dist complex method"]]<-summaryfunctionFull(100*c1.dist.comp)
c1.dist.summaries[["c1.dist complex method v2"]]<-summaryfunctionFull(100*c1.dist.comp.v2)

dat1 <- data.frame( X= as.numeric(100*c1.dist.comp))
dat1.dens<- get.ALP.c1.density(100*c1.dist.comp,adjust=1.5)
fit1.c1.dist.comp<- data.frame(x=dat1.dens$dens$x,y=dat1.dens$dens$y/sum(dat1.dens$dens$y))

tmp <-c1.dist[,c(1:2)]
tmp[,2]<- c1.dist.comp
tmp <- rbind(tmp[,c(1:2)],keep.the.removed[,c(1:2)])
colnames(tmp)[2]<- "c1.guess.majority.int"
df.hypo <- merge(df.hypo,tmp, by = "prim_key")
SD.clean<- merge(SD.clean, tmp, by="prim_key",all.x = T)

p1<- ggplot(dat1) + 
  geom_histogram(aes(x=X, y = ..density..), 
                 binwidth = 5, fill="cornflowerblue",color="black") + 
  #geom_density(color="red",size=2)+
  geom_line(data=fit1.c1.dist.comp, aes(x=x,y=y),color="red",size=2 )+
  theme_bw() +
  guides(fill = guide_legend(title=""))+
  ylab("Density")+
  xlab(expression(paste(c[1], " value",sep=" ")))+
  #ggtitle(tab.label) +
  theme(axis.text.x=element_text(size=16,angle=0 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))
p1

ggsave(p1,file=paste(survey.output.dir,"c1_fit_using_majority_interprentation",".pdf",sep=""),
       width=10, height=5)


ET.slope <-(Evasion.Tax.Scurve[,"s"]/(Evasion.Tax.Scurve[,"m"]*sqrt(2*pi))) 
c1.tilde.dist <- -(0.5-ET.slope*Evasion.Tax.Scurve[,"m"])/ET.slope
c1.tilde.dist[c1.tilde.dist<0] <-0 

c1.dist.summaries[["c1.tilde.dist"]] <- summaryfunctionFull(100*c1.tilde.dist)

c1.dist <- unique(df.hypo[,c("prim_key","c1.guessed")], 
                  by=c("prim_key","c1.guessed"))
c1.dist.summaries[["c1.dist"]] <- summaryfunctionFull(c1.dist$c1.guessed)

epsilon<- 0.001
c1.dist.based.epsilon<-exp((erf.inv(2*epsilon - 1)*sqrt(2)/
                              Evasion.Tax.Scurve[,"s"]+log(Evasion.Tax.Scurve[,"m"]))) 

c1.dist.summaries[["c1.dist based epsilon of 0.1%"]] <-summaryfunctionFull(100*c1.dist.based.epsilon)

c1.dist.summaries <- as.data.frame(do.call("rbind",c1.dist.summaries))

c1.dist.summaries <- c1.dist.summaries[c(4,5,3,1,2),]

print(xtable(c1.dist.summaries, digits=1),file=paste(survey.output.dir,
                                        "c1_dist_summaries",".tex",sep=""),
      include.rownames=T)

model.param.tab$c1$c1.ALP.majority.fit <- fit1.c1.dist.comp

########################################
###                                  ###
###    Ranges of the c1 dist ti sample from 
###                                  ###
########################################


tri.dist<-c1.analysis$fit
# tri.dist$y<- 20-2*tri.dist$x
# tri.dist$y[tri.dist$y<0] <-0

tri.dist$y<- c(dbeta(c(0:10)/10,shape1=1,shape2=3),rep(0,100-10)) ## for a true triangular dist set shape2=2
tri.dist$y<- tri.dist$y/sum(tri.dist$y)

tab <- "ALP_c1_triangular"
saveRDS(tri.dist,file=paste(survey.data.dir,tab.label,".Rdata",sep=""))

tmp <- sample(tri.dist$x,size=length(unique(df.hypo$prim_key)),prob=tri.dist$y,replace=T)
tmp <- tmp[order(tmp)]

c1.dist <- unique(df.hypo[,c("prim_key","c1.guessed")], 
                  by=c("prim_key","c1.guessed"))

order.c1<-order(c1.dist$c1.guessed)

c1.dist[order.c1,2]<-tmp 
colnames(c1.dist)[2]<-"tax.rate.threshold.tri"


df.hypo <- merge(df.hypo,c1.dist, by = "prim_key")
SD.clean<- merge(SD.clean, c1.dist, by="prim_key",all.x = T)

#### create the illustration plot 


tmp <- cbind(c1.analysis$fit, y2=fit1.c1.dist.comp$y,y3=tri.dist$y)


p1 <- ggplot(tmp) +
  geom_line(aes(x=x, y=y),size=2,color="red")+
  geom_line(aes(x=x, y=y2),size=2,color="darkgreen")+
  geom_line(aes(x=x, y=y3),size=2,color="blue")+
  xlim(0,40)+
  xlab(expression(paste(c[1], " value",sep=" "))) +
  ylab("Sampling Probability")+
  #ggtitle(expression(paste("Multiplicative factor in the ",tilde(c)[1], " equation",sep=" ")))+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))
p1

ggsave(p1,file=paste(survey.output.dir,"ALP_c1_triangular",".pdf",sep=""),
       width=10, height=5)


p1 <- ggplot(tmp) +
  geom_line(aes(x=x, y=y2),size=2,color="darkgreen")+
  geom_line(aes(x=x, y=y3),size=2,color="blue")+
  xlim(0,30)+
  #xlab("")+ylab("")+
  xlab(expression(paste(c[1]^{(i)},"(%)",sep=" "))) +
  ylab("Sampling Probability")+
  #ggtitle(expression(paste("Multiplicative factor in the ",tilde(c)[1], " equation",sep=" ")))+
  theme(axis.text.x=element_text(size=20 ), 
        axis.text.y=element_text(size=20 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))
p1

ggsave(p1,file=paste(survey.output.dir,"ALP_c1_sampling",".pdf",sep=""),
       width=8, height=4)


model.param.tab$c1$c1.tri.dist.dist <- tri.dist

model.param.tab$c1$c1.dist.sampling <- c(lowerbound=-1,upperbound=1) 

saveRDS(df.hypo,file=paste(survey.data.dir,"Compliance_Hypotheticals",".Rdata",sep=""))
write.csv(df.hypo, paste(survey.data.dir,"Compliance_Hypotheticals",".csv",sep=""),  row.names = F)


########################################
###                                  ###
###    Save IMAGE
###                                  ###
########################################

save.image(paste(survey.data.dir,"tmp2.image",".Rdata",sep=""))

load(paste(survey.data.dir,"tmp2.image",".Rdata",sep=""))

########################################
###                                  ###
###    fit lognormal cdf  sigmoid  for qP
###                                  ###
########################################


qP.parameters.list <- get.qP.lognormal.fit_v2(df.hypo,c1.field ="c1.guess.majority.int",
                                              c2=0.7,ET.slope=ET.slope.c1.tilde )
qP.parameters.list$prim_key <- rownames(qP.parameters.list)

### note if c1.field ="tax.rate.threshold.tri" instead of c1.field ="c1.guess.majority.int" results don't change
### by much at all. 

dim(qP.parameters.list)

qP.parameters <- rbind(s=c(summary(qP.parameters.list[,1])),
          m=c(summary(qP.parameters.list[,2])))

qP.parameters <- signif(qP.parameters,3)

qP.LMU<- as.data.frame(qP.parameters[,2:5])

qP.LMU<-t(apply(qP.LMU,1, FUN=function(x){x[order(x)]}))
qP.LMU[,2]<- apply(qP.LMU[,2:3], 1, min) 
qP.LMU[,3]<-qP.LMU[,4]
qP.LMU<-qP.LMU[,1:3]
colnames(qP.LMU)<- c("Lower","Mode","Upper")

m <- qP.LMU["m","Mode"]
s <- qP.LMU["s","Mode"]

g<- qP.LMU["s",]/(sqrt(2*pi)*qP.LMU["m",])

qP <- seq(0,1,0.0001)
qP.dat <- as.data.frame(cbind(geom.mean=sqrt(qP), x=qP,y=cdf.lognormal(x=qP, log(m),1/s)))
qP.datL <- as.data.frame(cbind(geom.mean=sqrt(qP), x=qP,
                y=cdf.lognormal(x=qP, log(qP.LMU["m","Lower"]),1/qP.LMU["s","Lower"])))
qP.datU <- as.data.frame(cbind(geom.mean=sqrt(qP), x=qP,
                y=cdf.lognormal(x=qP, log(qP.LMU["m","Upper"]),1/qP.LMU["s","Upper"])))


plot(qP.dat$x,qP.dat$y,type="l",xlab="qP",ylab="Phi",lwd = 2, 
     col = "dark red", main = "multiplicative factor in the tilde c1 equation")

p1<-ggplot(qP.dat) +
  geom_line(aes(x=x, y=y),size=2,color="red")+
  geom_line(data= qP.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  geom_line(data= qP.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  xlim(0,5*m)+ylim(0,1)+
  xlab((expression(tilde(q)^(i)*tilde(P)^(i)))) +
  ylab((expression(Phi (tilde(q)^(i)*tilde(P)^(i),m,s))))+
  #ggtitle(expression(paste("Multiplicative factor in the ",tilde(c)[1], " equation",sep=" ")))+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))+
  geom_vline(xintercept = m,  linetype="dotted") +
  geom_hline(yintercept = 0.5,  linetype="dotted")+
  annotate("text", label = "m",x=m,y=-0.0, size = 10, colour = "black", parse=TRUE)
print(p1)

p2<-ggplot(qP.dat) +
  geom_line(aes(x=geom.mean, y=y),size=2,color="red")+
  geom_line(data= qP.datL , aes(x=geom.mean, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  geom_line(data= qP.datU , aes(x=geom.mean, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  xlim(0,min(5*sqrt(m),1.0))+ylim(0,1)+
  xlab((expression(sqrt(tilde(q)^(i)*tilde(P)^(i))))) +
  ylab((expression(Phi (tilde(q)^(i)*tilde(P)^(i),m,s))))+
  #ggtitle(expression(paste("Multiplicative factor in the ",tilde(c)[1], " equation",sep=" ")))+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))+
  geom_vline(xintercept = sqrt(m),  linetype="dotted") +
  geom_hline(yintercept = 0.5,  linetype="dotted")+
  annotate("text", label = "sqrt(m)",x=sqrt(m),y=-0.0, size = 10, colour = "black", parse=TRUE)
print(p2)


p3 <-   ggplot(qP.dat) +
  geom_line(aes(x=x, y=y),size=2,color="red")+
  geom_line(data= qP.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  geom_line(data= qP.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  xlim(-0.005,5*m)+ylim(0,1)+
  xlab((expression(Deterrence(tilde(q)^(i)*tilde(P)^(i))))) +
  ylab("")+
  #ylab((expression(Phi (tilde(q)^(i)*tilde(P)^(i),m,s))))+
  #ggtitle(expression(paste("Multiplicative factor in the ",tilde(c)[1], " equation",sep=" ")))+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=26),
        legend.text = element_text( size = 12))+
  geom_vline(xintercept = m,  linetype="dotted") +
  geom_hline(yintercept = 0.5,  linetype="dotted")+
  annotate("text",label = "c[1]^{(i)}",parse = TRUE,
           x=-0.005,y=0.05, size = 10, colour = "red",alpha=1)+
  annotate("text",label = "c[2]",parse = TRUE,
           x=-0.005,y=0.95, size = 10, colour = "blue",alpha=1)+    
  annotate("text",label = "tilde(c)[1]^{(i)}",parse = TRUE,
           x=0.05,y=0.3, size = 10, colour = "red",alpha=1,angle=45)



qP.parameters <- rbind(s=c(summaryfunctionFull(qP.parameters.list[,1])),
                       m=c(summaryfunctionFull(qP.parameters.list[,2])))

print(xtable(qP.parameters,digits=3),  
      file=paste(survey.output.dir ,"qP_parameters",".tex",sep=""))


qP.parameters <-rbind(qP.parameters ,g=unlist(qP.parameters["s",])/(sqrt(2*pi)*unlist(qP.parameters["m",])))

print(xtable(qP.parameters[2:3,c(2:3,6)],digits=3),  
      file=paste(survey.output.dir ,"qP_parameters_reduced",".tex",sep=""))



ggsave(p1,file=paste(survey.output.dir,"qP_lognormal_fit",".pdf",sep=""),
       width=10, height=5)
ggsave(p2,file=paste(survey.output.dir,"qP2_lognormal_fit",".pdf",sep=""),
       width=10, height=5)
ggsave(p3,file=paste(survey.output.dir,"qP3_lognormal_fit",".pdf",sep=""),
       width=8, height=4)


SD.clean<- merge(SD.clean, qP.parameters.list, by="prim_key",all.x = T)

qP.LMU<-as.data.frame(t(qP.LMU))
qP.sampling.model<-lm(qP.LMU$s~qP.LMU$m)
tmp<-qP.LMU$m
names(tmp)<- rownames(qP.LMU)
model.param.tab$qP$m <- tmp
model.param.tab$qP$qP.sampling.model <- qP.sampling.model
model.param.tab$qP$qP.summary <- qP.parameters 

source("Survey code/qP.further.analysis.R")

########################################
###                                  ###
###     Refund debt
###                                  ###
########################################

t.test.debt.refund <- list()
out.tab <- list()
for(subset.label  in c("all","selfemployed","not.selfemployed")){
  
  subset <- subsets[,subset.label]
  df<-SD.clean[subset,]
  
  print(table(subsets[,subset.label]))
  
  debt.group <- df$refunddebt[as.logical(2-df$behavreactionrandom3)]
  refund.group <- df$refunddebt[!as.logical(2-df$behavreactionrandom3)]
  
  sum.debt.group<-summaryfunctionFull(debt.group)
  sum.refund.group<-summaryfunctionFull(refund.group)
  
  out <- as.data.frame(rbind(refund.group= sum.refund.group,debt.group= sum.debt.group))
  
  out.tab[[subset.label]] <- out 
  
  
  tmp <-t.test(debt.group,refund.group) 
  print(tmp)
  t.test.debt.refund[[subset.label]] <-t_out(toutput= tmp, 
                                             n.equal = TRUE, welch.df.exact = TRUE, 
                                             welch.n = NA,d.corr = TRUE, print = TRUE)

}

out.tab<- do.call("rbind",out.tab)

rownames(out.tab) <- gsub("\\."," ",rownames(out.tab) )

tab.label <- paste("refunddebt", sep="")
print( xtable(out.tab,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), 
       include.rownames=T, include.colnames=T)

t.test.debt.refund<- as.data.frame(do.call("rbind",t.test.debt.refund))

t.test.debt.refund<- cbind(rownames(t.test.debt.refund),t.test.debt.refund)
colnames(t.test.debt.refund)[1] <-"Respondent subsample"

tmp.tab.out<- xtable(t.test.debt.refund)

print(tmp.tab.out,  
      file=paste(survey.output.dir ,"t_test_debt_refund",".tex",sep=""), include.rownames=F)


### results of the t.test reject the hypotheis that there is a differece between refund and debt.
model.param.tab$debt.to.refund.evasion.factor <- 1 


########################################
###                                  ###
###     Public Services and actor 
###                                  ###
########################################

# ServicesTaxes Many public goods and services, such as interstate highways, national defense,
# national parks, and environmental protection, are in part paid for...
# Many public goods and services, such as interstate highways, national defense,
# national parks, and environmental protection, are in part paid for by federal
# income taxes. To what extent are the public goods and services that you receive
# worth the federal income taxes you pay?
# 1 1 Not at all worth it
# 2 2
# 3 3
# 4 4
# 5 5 Definitely worth it

subset.label <- "all"
subset <- subsets[,subset.label]
df<-SD.clean[subset,]


out <- ALP.fn.get.perc.of.respondents(df$servicestaxes, levels=c("Not at all worth it","Not worth it","Neutral","Worth it","Definitely worth it"),label = "Worth level of services")

tab.label<- "ServicesTaxes"
print( xtable(out,digits=1,auto=T), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)

# SD.clean$servicestaxes <- reorder(as.factor(SD.clean$servicestaxes),order =TRUE)
# levels(SD.clean$servicestaxes) <- c("Not at all worth it","Not worth it","Neutral","Worth it","Definitely worth it")


# Actor Actor
# Imagine that you heard a famous actor was caught and prosecuted for tax evasion.
# In your mind, would hearing about this make you more or less likely to report
# all of taxes you owe to the IRS?
# 1 I would be much more likely to fully report my income
# 2 I would be somewhat more likely to fully report my income
# 3 It would not affect my income reporting either way
# 4 I would be somewhat less likely to fully report my income
# 5 I would be much less likely to fully report my income


subset.label <- "all"
subset <- subsets[,subset.label]
df<-SD.clean[subset,]

out <-ALP.fn.get.perc.of.respondents(df$actor, levels=c("I would be much more likely to fully report my income","I would be somewhat more likely to fully report my income","It would not affect my income reporting either way","I would be somewhat less likely to fully report my income","I would be much less likely to fully report my income"),label = "level or likelihood to report")
tab.label<- "Actor"
print( xtable(out,digits=1,auto=T), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F, include.colnames=T)

# SD.clean$actor <- reorder(as.factor(SD.clean$actor),order =TRUE)
# levels(SD.clean$actor) <- c("I would be much more likely to fully report my income","I would be somewhat more likely to fully report my income","It would not affect my income reporting either way","I would be somewhat less likely to fully report my income","I would be much less likely to fully report my income")
# 

tmp <- SD.clean[, c("prim_key","perceivedevasionrate",
                    "perceivedauditrate","actor","familyincome")]
tmp <- merge(tmp, ego.data[,c("prim_key","hheveraudited")],by="prim_key")
tmp <- tmp[tmp$perceivedauditrate<=75,]

tmp$actor.binary <- tmp$actor<3

glm.tmp <-glm(actor.binary~hheveraudited+perceivedauditrate,family = binomial(link = "logit"),data=tmp)

summary(glm.tmp )


actor.yes<- tmp$perceivedevasionrate[tmp$actor<3]
actor.no <- tmp$perceivedevasionrate[tmp$actor>2]

summaryfunctionFull(actor.yes)
summaryfunctionFull(actor.no)


actor.yes<- tmp$perceivedauditrate[tmp$actor<3]
actor.no <- tmp$perceivedauditrate[tmp$actor>2]

summaryfunctionFull(actor.yes)
summaryfunctionFull(actor.no)


audit.media.factor1<-summaryfunctionFull(actor.yes)$Mean/summaryfunctionFull(actor.no)$Mean

weights <- table(tmp$actor<3)/nrow(tmp)
audit.media.factor2<-weights%*%c(1,audit.media.factor1)

audit.media.factor<-c(audit.media.factor.evadors=audit.media.factor1,
                      audit.media.factor.nonevadors=audit.media.factor2)

print(audit.media.factor)

t.test(actor.yes,actor.no)



########################################
###                                  ###
###     Reorganize Data for          ###
###     Importance placed on different tax-related considerations
###            Qus.28-31              ###
###                                  ###
########################################


subset <- subsets[,"all"]
df<-SD.clean[subset,]

Qu28 <- grepl(paste(c("prim_key","refunddebt"),collapse="|"),names(df)) & !(grepl("intro",names(df)) ) & !(grepl("total",names(df)) ) & !(grepl("old",names(df)) )

Qu29 <- grepl(paste(c("prim_key","taxesimportant_","error_tokens"),collapse="|"),names(df)) & !(grepl("intro",names(df)) ) & !(grepl("total",names(df)) ) & !(grepl("old",names(df)) )

Qu30 <- grepl(paste(c("prim_key", "taxesfairness_"),collapse="|"),names(df)) & !(grepl("intro",names(df)) ) & !(grepl("total",names(df)) )& !(grepl("old",names(df)) )

Qu31 <- grepl(paste(c("prim_key","riskauditspenalties_"),collapse="|"),names(df)) & !(grepl("intro",names(df)) ) & !(grepl("total",names(df)) )& !(grepl("old",names(df)) )


f.subsets<-as.data.frame(sapply(subsets,as.factor))



########################################
### Question 29
########################################


dat.29<-df[,Qu29]
dat.29<-fix.Qu29(dat.29)

dat.29 <- cbind(dat.29,selfemployed=f.subsets$selfemployed,
                audit = f.subsets$audited)

colnames(dat.29)[2:5] <- c("Taxes that I Owe","Cost figuring out Taxes", "Benefits and Public Services","Moral Obligation to Comply")


out <- do.call("rbind",lapply(dat.29[,c(2:5)],summaryfunctionFull))
tab.label<- "ALP_taxesimportant"
out <- as.data.frame(out)
print( xtable(out,digits=1,auto=T), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=T, include.colnames=T)


detach("package:reshape", unload=TRUE)
library(reshape2)
melt.dat.29 <- melt(dat.29[,c(2:5,7,8)],id = c("selfemployed","audit"),na.rm = T, value.name="weight")

melt.dat.29<- melt.dat.29[!is.na(melt.dat.29$selfemployed) & !is.na(melt.dat.29$audit),]

p<- ggplot(data=melt.dat.29, aes(variable , weight))+
  geom_boxplot(fill ="cornflowerblue" ,
               outlier.colour =NULL,outlier.size = 0.2) +
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=3) + 
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Importance of the Tax Rate') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=10 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu29_taxesimportant",".pdf",sep=""),width=12, height=6)


p<- ggplot(data=melt.dat.29, aes(variable , weight))+
  geom_boxplot(aes(fill=selfemployed),
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Importance of the Tax Rate') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=10 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu29_taxesimportant_se",".pdf",sep=""),width=12, height=6)


p<- ggplot(data=melt.dat.29, aes(variable , weight))+
  geom_boxplot(aes(fill=audit),
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Importance of the Tax Rate') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=10 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu29_taxesimportant_audit",".pdf",sep=""),width=12, height=6)

tmp<-out$Mean/100
names(tmp) <- rownames(out)

model.param.tab$importance.of.tax.rate<- tmp


########################################
### Question 30
########################################

dat.30<-df[,Qu30]

for(i in 2:4){
  tmp <- is.na(dat.30[,i])
  dat.30[tmp,i] <-0 
}

problematic <- (rowSums(dat.30[,2:4],na.rm = T)!=100)
dat.30[problematic,2:4]<-100*dat.30[problematic,2:4]/(rowSums(dat.30[problematic,2:4],na.rm = T))
problematic <- (rowSums(dat.30[,2:4],na.rm = T)!=100)
dat.30[problematic,2:4] <- NA


colnames(dat.30)[2:4] <- c("Percieved Fairness","Social Influences", "Media Influences")

out <- do.call("rbind",lapply(dat.30[,c(2:4)],summaryfunctionFull))
tab.label<- "ALP_taxesfairness"
out <- as.data.frame(out)
print( xtable(out,digits=1,suto=T), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=T, include.colnames=T)

dat.30 <- cbind(dat.30,selfemployed=f.subsets$selfemployed,
                audit = f.subsets$audited)


melt.dat.30 <- melt(dat.30[,2:6],id = c("selfemployed","audit"), na.rm = T, value.name="weight")
melt.dat.30.ori <- melt.dat.30
melt.dat.30<- melt.dat.30[!is.na(melt.dat.30$selfemployed) & !is.na(melt.dat.30$audit),]


p<- ggplot(data=melt.dat.30, aes(variable , weight))+
  geom_boxplot(fill ="darkolivegreen3" ,
               outlier.colour =NULL,outlier.size = 0.2) +
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=3) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Importance of various Influences') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=0 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu30_Influences",".pdf",sep=""),width=12, height=6)

p<- ggplot(data=melt.dat.30, aes(variable , weight))+
  geom_boxplot(aes(fill=selfemployed) ,
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Importance of various Influences') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=0 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu30_Influences_se",".pdf",sep=""),width=12, height=6)


p<- ggplot(data=melt.dat.30, aes(variable , weight))+
  geom_boxplot(aes(fill=audit) ,
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Importance of various Influences') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=0 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu30_Influences_audit",".pdf",sep=""),width=12, height=6)


dat.30<- merge(dat.30, ego.data[,c("prim_key", "n.alters","n.alters.tTaxes")], by= "prim_key", all.x=T ) 
dat.30$n.alters.tTaxes.prop <- dat.30$n.alters.tTaxes/dat.30$n.alters
dat.30$Net.wrt.Per <- dat.30$`Social Influences`/dat.30$`Percieved Fairness`
dat.30$Net.wrt.Per[dat.30$`Percieved Fairness`==0]<- NA 
dat.30$Net.wrt.Per[is.na(dat.30$`Percieved Fairness`)]<- NA 

cor(dat.30$Net.wrt.Per,dat.30$n.alters.tTaxes.prop,use="pairwise.complete.obs")
cor(dat.30$Net.wrt.Per,dat.30$n.alters.tTaxes,use="pairwise.complete.obs")
cor(dat.30$Net.wrt.Per,dat.30$n.alters,use="pairwise.complete.obs")

tmp <- (!is.na(dat.30$n.alters.tTaxes.prop) & dat.30$n.alters.tTaxes.prop>0)
cor(dat.30$Net.wrt.Per[tmp],as.numeric(dat.30$n.alters.tTaxes[tmp]>1),use="pairwise.complete.obs")
### The analysis above shows that there is no evidence that the importance of network effects 
### increases wih the number of alters talk taxes. 

tmp<-out$Mean/100
names(tmp) <- rownames(out)

model.param.tab$importance.on.morale$full<- round(tmp,2)


tmp<- dat.30[,c(2:4)]
tmp <- tmp[!is.na(tmp$`Percieved Fairness`),]
tmp[tmp$`Percieved Fairness`==0,"Percieved Fairness"]<- 1

personal <- round(summaryfunctionFull(tmp$`Percieved Fairness`)[c("1st Qu.","Median","3rd Qu.")],2)
media <- round(summaryfunctionFull(tmp$`Media Influences`)[c("1st Qu.","Median","3rd Qu.")],2)
social<- round(summaryfunctionFull(tmp$`Social Influences`)[c("1st Qu.","Median","3rd Qu.")],2)

model.param.tab$importance.on.morale$personal <- personal
model.param.tab$importance.on.morale$media <- media
model.param.tab$importance.on.morale$social <- social


########################################
### Question 31
######################################## 


dat.31<-df[,Qu31]

for(i in 2:4){
  tmp <- is.na(dat.31[,i])
  dat.31[tmp,i] <-0 
}

problematic <- (rowSums(dat.31[,2:4],na.rm = T)!=100)
dat.31[problematic,2:4]<-100*dat.31[problematic,2:4]/(rowSums(dat.31[,2:4],na.rm = T)[problematic])
problematic <- (rowSums(dat.31[,2:4],na.rm = T)!=100)
dat.31[problematic,2:4] <- NA




colnames(dat.31)[2:4] <- c("Personal","Social", "Media")

out <- do.call("rbind",lapply(dat.31[,c(2:4)],summaryfunctionFull))
tab.label<- "ALP_riskauditspenalties"
out <- as.data.frame(out)
print( xtable(out,digits=1), file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=T, include.colnames=T)


dat.31 <- cbind(dat.31,selfemployed=f.subsets$selfemployed,
                audit = f.subsets$audited)


melt.dat.31 <- melt(dat.31[,2:6],id = c("selfemployed","audit"), na.rm = T, value.name="weight")
melt.dat.31.ori <- melt.dat.31 
melt.dat.31<- melt.dat.31[!is.na(melt.dat.31$selfemployed) & !is.na(melt.dat.31$audit),]



p<- ggplot(data=melt.dat.31, aes(variable , weight))+
  geom_boxplot(fill ="slateblue2" ,
               outlier.colour =NULL,outlier.size = 0.2) +
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=18, size=3) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Type of Pereceive Deterrence') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=0 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu31_Influences",".pdf",sep=""),width=12, height=6)

p<- ggplot(data=melt.dat.31, aes(variable , weight))+
  geom_boxplot(aes(fill=selfemployed) ,
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Type of Pereceive Deterrence') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=10 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu31_Influences_se",".pdf",sep=""),width=12, height=6)

p<- ggplot(data=melt.dat.31, aes(variable , weight))+
  geom_boxplot(aes(fill=audit) ,
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Type of Pereceive Deterrence') +
  ylab('Weight as a percentage')+
  theme(axis.text.x=element_text(size=16,angle=10 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu31_Influences_audit",".pdf",sep=""),width=12, height=6)


dat.31<- merge(dat.31, ego.data[,c("prim_key", "n.alters","n.alters.tTaxes")], by= "prim_key", all.x=T ) 
dat.31$n.alters.tTaxes.prop <- dat.31$n.alters.tTaxes/dat.31$n.alters
dat.31$Net.wrt.Per <- dat.31$Social/dat.31$Personal
dat.31$Net.wrt.Per[dat.31$Personal==0]<- NA 
dat.31$Net.wrt.Per[is.na(dat.31$Personal)]<- NA 

cor(dat.31$Net.wrt.Per,dat.31$n.alters.tTaxes.prop,use="pairwise.complete.obs")
cor(dat.31$Net.wrt.Per,dat.31$n.alters.tTaxes,use="pairwise.complete.obs")
cor(dat.31$Net.wrt.Per,dat.31$n.alters,use="pairwise.complete.obs")


tmp<-out$Mean/100
names(tmp) <- rownames(out)

model.param.tab$importance.on.percieved.deterrance$full<- round(tmp,2)


tmp<- dat.31[,c(2:4)]
tmp <- tmp[!is.na(tmp$Personal),]
tmp[tmp$Personal==0,"Personal"]<- 1
personal <- round(summaryfunctionFull(tmp$Personal)[c("1st Qu.","Median","3rd Qu.")],2)
media<- round(summaryfunctionFull(tmp$Media)[c("1st Qu.","Median","3rd Qu.")],2)
social<- round(summaryfunctionFull(tmp$Social)[c("1st Qu.","Median","3rd Qu.")],2)

model.param.tab$importance.on.percieved.deterrance$personal<- personal
model.param.tab$importance.on.percieved.deterrance$media<- media
model.param.tab$importance.on.percieved.deterrance$social <- social



########################################
###                                  ###
###    Combine Qus. 30-31              ###
###                                  ###
########################################

melt.dat.30<- melt.dat.30.ori
melt.dat.31<- melt.dat.31.ori

melt.dat.30$Question <- "fairness"
melt.dat.31$Question <- "risk"
levels(melt.dat.30$variable) <- levels(melt.dat.31$variable)

overall <- rbind(melt.dat.30,melt.dat.31)
overall$Question<- as.factor(overall$Question)
colnames(overall)[5]<- "Type"

p<- ggplot(data=overall)+
  geom_boxplot(aes(variable , weight, fill=Type),
               outlier.colour =NULL,outlier.size = 0.2) +
  stat_summary(aes(x=variable, y=weight, fill=Type),fun.y=mean, colour="black", geom="point", 
               shape=18, size=3, position = position_dodge(width = 0.75), width = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('') +
  ylab('Distribution of the Number of Tokens')+
  theme(axis.text.x=element_text(size=16,angle=0 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=16 ) ,
        axis.title.x = element_text( size=16 ) ,
        axis.title.y = element_text( size=16 ),
        legend.text = element_text( size = 14))+
        scale_fill_manual(values=c(  "#56B4E9","#E69F00"))
print(p)
ggsave(p,file=paste(survey.output.dir,"Qu30-31_Influences",".pdf",sep=""),width=8, height=4)
ggsave(p,file=paste("Writeup/Figures/","Qu30-31_Influences",".pdf",sep=""),width=9, height=4.5)


PE.fairness <- overall[overall$Type%in%"fairness" & overall$variable%in%"Personal",]

hist(PE.fairness$weight)

PE.risk <- overall[overall$Type%in%"risk" & overall$variable%in%"Personal",]

hist(PE.risk$weight)

seed(500)
xx<-rpert( length(PE.fairness$weight), 0.0, 1.00, x.mode=0.60 )

hist(xx)
xx<- data.frame(weight=100*xx,Type="PERT")

PE <-overall[overall$variable%in%"Personal",c("weight","Type")]
 
PE <- rbind(PE,xx)



p1<-ggplot()+
  geom_histogram(data=PE, aes(x = weight, y = 300*(..count..)/sum(..count..),fill=Type), 
                 binwidth=10, color="black", position = "dodge")+facet_grid(~Type)+
  theme_bw() +
  xlab("Weight")+
  ylab("Percentage")+
  #ggtitle(tab.label) +
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_fill_manual(values = c("#56B4E9","#E69F00","gray")) 
print(p1)
ggsave(p1,file=paste("Writeup/Figures/","Weight_Dist",".pdf",sep=""),width=9, height=4.5)




########################################
###                                  ###
###     Reorganize Data for          ###
###     Pulic goods considerations and Free-Riding behavior     ###
###            Qus.32-43               ###
###                                  ###
########################################


FR <- SD.clean[,c(190:194,196:200)] 
FR.labels<- paste("FR",c("publicradio","streaming","neutered.dog","flu.vacc","income.tax"),sep="_")
FR.labels<-c(paste(FR.labels,".Me",sep=""), paste(FR.labels,".Others",sep=""))
colnames(FR) <- FR.labels 


p.title<- "Attitudes towards FR"
corrgram(FR, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main=paste("Correlogram of", p.title,sep=" "))

corFR <- round(cor(FR,use="complete"),2)

tmp <- SD.clean[,c("perceivedevasionrate","perceivedevasionratepopulation")]
tmp <-as.data.frame(do.call("cbind",lapply(tmp,as.numeric)))
colnames(tmp) <- c("P.Evasion","Like.P.Evasion")
FR<- as.data.frame(cbind(tmp,FR[,c(1,5,6,10)]))

corFR <- round(cor(FR),2)

p.title<- "Attitudes towards FR"
corrgram(FR, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main=paste("Correlogram of", p.title,sep=" "))


########################################
###                                  ###
###     Reorganize Data for          ###
###     Pulic goods considerations and Free-Riding behavior     ###
###            Qus.32-43               ###
###                                  ###
########################################



########################################
###                                  ###
###     SAVE
###                                  ###
########################################

save.image(paste(survey.data.dir,"tmp2.image",".Rdata",sep=""))



df.tmp <- df.hypo
if(max(df.tmp$c1.guess.majority.int,na.rm=T)<1) {df.tmp$c1.guess.majority.int <- 100*df.tmp$c1.guess.majority.int}
print(mean(df.tmp$c1.guess.majority.int,na.rm=T))
df.tmp$x <- df.tmp$perceivedauditrate*df.tmp$perceivedpenaltyrate/ 10^2
df.tmp$Compliance <- 100 - df.tmp$perceivedevasionrate
df.tmp <- df.tmp[,c("perceivedtaxrate","x","Compliance")]/10^2
write.csv(df.tmp, paste(survey.data.dir,"Compliance_Hypotheticals_3D_data",".csv",sep=""),  
          row.names = F)

saveRDS(SD.clean,file=paste(survey.data.dir,"SD.clean.Rdata",sep=""))
saveRDS(model.param.tab,file=paste(survey.data.dir,"model.param.tab.Rdata",sep=""))


