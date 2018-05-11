remove(list = ls(all = TRUE))
gc()

set.seed(55279)

library(xtable)
library(texreg)
library(yacca)
library(ggplot2)
library(reshape2)
require(Hmisc)
library(foreign)
library(corrgram)
library(xlsx)
library(igraph)

## Specify directories 
network.data.dir <- "Network data/"
survey.data.dir <- "Survey data/"
survey.output.dir <- "SurveyTablesPlots/"
library.dir   <- "Survey code/SurveyLibrary/"
library <- file.path(library.dir, "library.R")
source(library)

se.reg<- readRDS(file=paste(survey.data.dir,"se.reg.models",".Rdata",sep=""))

demog.person<- readRDS(file="Network data/PN460.demog.person.RData")

PN1.contact<- readRDS(file="Network data/PN1.contact.RData") 
PN10.contact<- readRDS(file="Network data/PN10.contact.RData") 
PN460.contact<- readRDS(file="Network data/PN460.contact.RData") 

contact.list<- list(PN1=PN1.contact,
                    PN10=PN10.contact,
                    PN460=PN460.contact)


## Note by KB: Just FYI (and not for attribution), of tax returns submitted to date in 2016 there are 24 million sch c/f filers (self employed) and 119 million non se filers. Some characteristics of se (non-se) primary filers are 71.3% (60.1%) male, 11.6% (8.1%) working aliens, 67.6% (52.6%) used paid preparer, ave age 51 (48).

### reply RV: Thank you! This is very useful information since in constructing the network attribute for self-employment I was assuming that 10.1% of all workers are self-employed. This figure comes from a recent update from the author of “Self-employment in the United States” (http://www.bls.gov/opub/mlr/2010/09/art2full.pdf). However, the more appropriate proportion to use seems to be the one you gave me of 24/(24+119) ~16.8% of filers who are workers are self-employed. I believe that the reason these two numbers are different is because the latter takes into account the type of tax filing (I.e., filing jointly versus filing separately or as single). 

target.se <- 0.168
threshold <- 0.5

summary.tab.list <- list()
ncl <- 2
for(ncl in 1:length(contact.list)){
  
  name <- names(contact.list)[ncl]
  contact <- contact.list[[ncl]]
  
  g <- graph.data.frame(contact, directed=FALSE, vertices=NULL)
  nn <- list()
  nn <- sapply(V(g)$name,FUN=function(i) {neighbors(g, i)})
  
  
  set.Person.Ids <- unique(c(contact$Person.Id.1,contact$Person.Id.2))
  
  df <-demog.person[demog.person$person.id%in%set.Person.Ids,] 
  df <- df[,!(names(df)%in%"HH.Person.Id")]
  df <- df[!duplicated(df), ]
  
  df.tmp <- df
  df$gender[df$gender==2] <- 0 ### survey uses 0,1 for gender and PN uses 1,2
  df.tmp$gender <- df$gender
  df.tmp$gender <- as.factor(df.tmp$gender)
  levels(df.tmp$gender) <- c("Females","Males")
  
  df.tmp <- df.tmp[,c("person.id","age","gender","income")]
  
  df.tmp$self.employed<-predict(se.reg$se.reg1,newdata=df.tmp,type="response")
  
  ### http://www.bls.gov/spotlight/2016/self-employment-in-the-united-states/pdf/self-employment-in-the-united-states.pdf
  ### 10.1% of the population in the US is self employed.
  
  df.tmp$self.employed<- df.tmp$self.employed* target.se/mean(df.tmp$self.employed)
  
  
  
  df.tmp$self.employed<- (runif(nrow(df.tmp),0,1)< df.tmp$self.employed)
  
  alter_se_proportion  <- lapply(nn, FUN=function(x){
    y <- V(g)$name[x]
    rec <- df.tmp$person.id%in%y
    alter_se_proportion <- mean(df.tmp$self.employed[rec])
    return(alter_se_proportion )
  })
  alter_se_proportion <- do.call("rbind",alter_se_proportion)
  alter_se_proportion <- alter_se_proportion[as.character(df.tmp$person.id),]
  df.tmp$alter_se_proportion <- alter_se_proportion
  

  
  #### Do intermediate interations. 
  for(k in 2:10){
    m.se <- mean(df.tmp$self.employed)
    
    if(m.se<threshold){  ### the value of threshold needs to be chosen to calibrare the output named summary.iteration.method
      print(m.se)
      old.SE <-  df.tmp$self.employed
      df.tmp$self.employed.propensity<-predict(se.reg$se.reg2,newdata=df.tmp,type="response")
      #df.tmp$self.employed<- df.tmp$self.employed* target.se/mean(df.tmp$self.employed)
      df.tmp$self.employed<- (runif(nrow(df.tmp),0,1)< df.tmp$self.employed.propensity) |  old.SE
      
      alter_se_proportion  <- lapply(nn, FUN=function(x){
        y <- V(g)$name[x]
        rec <- df.tmp$person.id%in%y
        alter_se_proportion <- mean(df.tmp$self.employed[rec])
        return(alter_se_proportion )
      })
      alter_se_proportion <- do.call("rbind",alter_se_proportion)
      alter_se_proportion <- alter_se_proportion[as.character(df.tmp$person.id),]
      df.tmp$alter_se_proportion <- alter_se_proportion
    }
  }
  
  or.se <- df.tmp[df.tmp$self.employed,]
  or.se <- or.se[order(or.se$alter_se_proportion,decreasing = T),]
  retain.SE <- or.se$person.id[1:round(target.se/mean(df.tmp$self.employed)*nrow(or.se))]
  df.tmp$self.employed <- F
  df.tmp$self.employed[df.tmp$person.id%in% retain.SE] <- T
  
  alter_se_proportion  <- lapply(nn, FUN=function(x){
    y <- V(g)$name[x]
    rec <- df.tmp$person.id%in%y
    alter_se_proportion <- mean(df.tmp$self.employed[rec])
    return(alter_se_proportion )
  })
  alter_se_proportion <- do.call("rbind",alter_se_proportion)
  alter_se_proportion <- alter_se_proportion[as.character(df.tmp$person.id),]
  df.tmp$alter_se_proportion <- alter_se_proportion
  
  summary.iteration.method <- c(m.se=mean(df.tmp$self.employed),
                                m.alt.se = mean(df.tmp$alter_se_proportion),
                                m.alt.se_se=mean(df.tmp$alter_se_proportion[df.tmp$self.employed]),
                                m.alt.se_nse = mean(df.tmp$alter_se_proportion[!df.tmp$self.employed]))
  
  print(summary.iteration.method)
  
  summary.tab.list[[paste("PN", 10^(ncl-1), sep="")]] <- round(100*summary.iteration.method,2)
  
  df$self.employed.propensity <- df.tmp$self.employed.propensity
  
  df$self.employed <- df.tmp$self.employed
  
  df$prop.hideable.income[df$self.employed] <- 1 
  
  saveRDS(df ,file=paste(network.data.dir,name,".demog.person.RData",sep=""))
  
}

# population.data.file = "Inputs/PN10_population_data.csv"
# pop.data <-  read.csv(population.data.file, stringsAsFactors = F)
# dim(pop.data)
# dim(df)
# pop.data <- merge(pop.data, df[,c("person.id","self.employed.propensity")],by= "person.id")
# write.csv(pop.data, 
#           paste(population.data.file,sep=""), 
#           row.names = FALSE)


summary.tab.list<- do.call("rbind", summary.tab.list)

tab.label <- paste("SE_generation_algorithm_for_PN", sep="")
print(xtable(summary.tab.list),  
      file=paste(survey.output.dir,tab.label,".tex",sep=""))



########################################
###                                  ###
###   Sanity Check
###                                  ###
########################################

for(ncl in 1:length(contact.list)){
  
  name <- names(contact.list)[ncl]
  contact <- contact.list[[ncl]]
  
  df<-readRDS(file=paste(network.data.dir,name,".demog.person.RData",sep=""))
  
  SE.list <- df$person.id[df$self.employed]
  
  g <- graph.data.frame(contact, directed=FALSE, vertices=NULL)
  nn <- list()
  nn <- sapply(V(g)$name,FUN=function(i) {neighbors(g, i)})
  
  nn.SE <- sapply(V(g)$name,FUN=function(i) {
    tt<-table(names(nn[[i]]) %in% SE.list)
    tt<- as.numeric(tt["TRUE"]/sum(tt))
    if(is.na(tt)) {tt <- 0}
    return(tt)
    })

    nn.SE.only <-nn.SE[names(nn.SE) %in% SE.list]
    
    nn.nSE.only <-nn.SE[!(names(nn.SE) %in% SE.list)]
    
    print(mean(nn.SE.only))
    print(mean(nn.nSE.only))
    print(mean(nn.SE.only)/mean(nn.nSE.only))
}



df<-readRDS(df ,file=paste(network.data.dir,"PN1",".demog.person.RData",sep=""))

