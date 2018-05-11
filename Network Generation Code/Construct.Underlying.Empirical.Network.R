remove(list = ls(all = TRUE))
gc()

# Changing the directory to the one where this script exists
# This section should not be executed (because it does not work) 
# if you are running this script interactively
#source.dir <- dirname(sys.frame(1)$ofile)
#setwd(source.dir)


### Load Libraries: please ensure you have installed these R packages.
library(ggplot2)
library(Hmisc)
library(igraph)
library(xlsx)
library(reshape2)
library(timeDate)
library(Rcpp)
library(rgdal)
library(zipcode)
library(ggmap)
library(rgeos)
library(plyr)
library(RColorBrewer)
library(grid)
library(nnet)
library(maptools)

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

library.dir   <- "Library/"
library <- file.path(library.dir, "library.R")
source(library)

#QUESTION: Add this to git? or just leave it as is?
figures.dir   <- "Writeup/Figures/"
con.pop.dir <- "../Construct the Population/"

##Set Seed 
set.seed(55279) ### first seed given by Chris Marcum
set.seed(55333) 

#############################################################
###                                                       ###
###        Load Location file            
###                                                       ###
#############################################################

# Each line has the following fields:
# Location Id - id of the location
# Y - offset from origin in meters
# X - offset from origin in meters

## Location file
#location-portland-1-v2.dat contains location information in form of X and Y
# Universal Transverse Mercator (UTM) coordinates (Zone 10) in meters 
#  https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system
# https://sites.google.com/a/lakeheadu.ca/yong-luo/blog/convert-utm-to-longlat-in-r

location.portland.file <- "../../Network Data/locations-portland-1-v2/locations-portland-1-v2.dat"

location.Id.lookup<- read.csv(file=location.portland.file,head=FALSE,sep=" ",stringsAsFactors = FALSE)
names(location.Id.lookup) <- c("Location.Id","X","Y")

head(location.Id.lookup)
dim(location.Id.lookup)


data(zipcode)
zipcode.Portland <- zipcode[zipcode$state%in%c("OR","WA"),]

## Zone 10 is used in the Pacific Northwest see:
## https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

location.Id.lookup<-get.zipcodes(location.Id.lookup, zipcode.Portland , is.UTM=TRUE,zone=10)

#############################################################
###                                                       ###
###        Load Demographics              
###                                                       ###
#############################################################

demog.household.portland.file <- "../../Network Data/demographics-household-portland-1-v2/demographics-household-portland-1-v2.dat"
household.portland.income.lookup.file<- "../../Network Data/demographics-household-portland-1-v2/Household.Income.Lookup.Table.csv"
demog.person.portland.file <- "../../Network Data/demographics-household-portland-1-v2/demographics-person-portland-1-v2.dat"



demog.person<- read.csv(file=demog.person.portland.file,head=FALSE,sep=" ",stringsAsFactors = FALSE)
names(demog.person)<- c("Person.Id","Household.Id","Age","Gender", "Worker","Relationship.to.HH")
## assumed : RELATIONSHIP WITH HEAD OF HOUSEHOLD  CODE CLASSIFICATION :
## 1 = Head of household (could have 2 heads per household); 2 = ? ; 3=? ; 4= ?
## compare table(demog.person$Relationship.to.HH) to nrow(demog.household)

dim(demog.person)
head(demog.person)

household.income.lookup <- read.csv(file=household.portland.income.lookup.file,head=T,stringsAsFactors = FALSE)
rownames(household.income.lookup) <- 1:14

family.income.distrubution<- read.csv(file="Tax data/CPS_2015_family_income.csv",head=T,stringsAsFactors = FALSE)
family.income.distrubution$proprtion<- family.income.distrubution$number/sum(family.income.distrubution$number)

write.csv(family.income.distrubution,"Tax data/CPS_2015_family_income.csv")


input.file <- paste("Income items that can be potentially hidden from tax agencies as a percent of the reported income",".xls",sep="")

hidden.income.tab <- read.xlsx(paste("Tax data/",input.file,sep=""), sheetName="proportion",stringsAsFactors = F)
hidden.income.tab <- as.data.frame(apply(hidden.income.tab,2,FUN=as.numeric))



demog.household<- read.csv(file=demog.household.portland.file,head=FALSE,sep=" ",stringsAsFactors = FALSE)
names(demog.household)<- c("Household.Id","Household.Income.Cat","Household.Size","Home.Location.Id", "Home.SubLocation.Id","Vehicles","Workers")

income<-generate.random.income(demog.household$Household.Income.Cat, family.income.distrubution)
demog.household$Income <- income



prop.hideable.income<- unlist(sapply(income, FUN=function(x){
  pos<- hidden.income.tab$Lower<=x & x<=hidden.income.tab$Upper
  pos<- c(1:length(pos))[pos]
  return(hidden.income.tab$proportion[pos])
}))

demog.household$Prop.Hideable.Income <- prop.hideable.income




household.structure<- table(demog.person$Household.Id,demog.person$Relationship.to.HH)
household.minors<- table(demog.person$Household.Id,demog.person$Age<18)[,2]
household.seniors<- table(demog.person$Household.Id,demog.person$Age>=65)[,2]

household.structure<- cbind(household.structure, 
                           minors=household.minors, 
                           seniors=household.seniors )

colnames(household.structure)<- c("HH","children","unknown1","unknown2","minors","seniors")
household.structure<-as.data.frame(household.structure)
household.structure$Household.Id <- rownames(household.structure)

demog.household<- merge(demog.household,household.structure,by="Household.Id")

### resolve true Head of House (HH)

demog.household$filing.status <- NA

demog.household$filing.status[demog.household$HH%in%1 & 
                                (demog.household$minor>0|
                                 demog.household$senior>0|
                                   demog.household$children>0|
                                   demog.household$unknown1>0|
                                   demog.household$unknown2>0) ] <- "Head.of.Household"
demog.household$filing.status[demog.household$HH%in%c(0,1) & demog.household$Household.Size%in%1] <- "Single"
## note all those with demog.household$HH == 0 live in a 
## household size of 1. Thus are single. 


### 
couples <- (demog.household$HH>1 & demog.household$Household.Size>1)

### See
## https://turbotax.intuit.com/tax-tools/tax-tips/Family/When-Married-Filing-Separately-Will-Save-You-Taxes/INF22492.html
### Of the 56 million tax returns married couples filed in 2009, the latest year for which the IRS has published statistics (at the time of writing), 4.3 percent belonged to twosomes who filed separately.

### https://www.irs.gov/uac/soi-tax- stats-individual-statistical- tables-by-filing-status#_grp1 cites a value of 4.9558918%
p.seperatly <- 0.049558918
seperate <- sample(c(FALSE,TRUE), size= sum(couples), 
                   prob=c(1-p.seperatly,p.seperatly),replace=TRUE)

tmp <- demog.household$filing.status[couples]
tmp[!seperate] <-"Married.Filing.Jointly"
tmp[seperate] <- "Married.Filing.Separately"
demog.household$filing.status[couples]<- tmp




dim(demog.household)
head(demog.household)



#############################################################
###                                                       ###
###        Create  Zipcode info             
###                                                       ###
#############################################################


tmp <- demog.household[,c("Home.Location.Id","Household.Size","Workers")]

tmp<- aggregate(tmp[,c("Household.Size","Workers")], 
                 by=list(Home.Location.Id = tmp$Home.Location.Id), FUN= sum)
colnames(tmp) <- c("Location.Id","Household.Size","Workers")
location.Id.lookup <- merge(location.Id.lookup,tmp,by="Location.Id")

tmp <-location.Id.lookup[,c("zipcode","Household.Size","Workers") ]  
tmp$zipcode<- as.factor(tmp$zipcode)
tmp <- aggregate(tmp[,c("Household.Size","Workers") ]  , by=list(zipcode = tmp$zipcode), FUN= sum)
zipcode.table<- tmp[order(tmp$Workers),]

demog.household[,c("lon","lat","zipcode")]<- location.Id.lookup[match(demog.household$Home.Location.Id,
      location.Id.lookup$Location.Id),c("lon","lat","zipcode")]


#############################################################
###                                                       ###
###        Point to HH             
###                                                       ###
#############################################################

demog.person$is.HH <- NA
demog.person$is.HH[demog.person$Relationship.to.HH%in%1]<-1
demog.person$is.HH[demog.person$Relationship.to.HH>1]<-0

tmp <-demog.household$Household.Id[(demog.household$filing.status%in%"Married.Filing.Jointly" &
         demog.household$HH%in%2)]

married.J<- demog.person[demog.person$Household.Id%in%tmp &
                           demog.person$Relationship.to.HH%in%1 ,]

married.J<-married.J[order(married.J$Household.Id),]

## Sanity Check: all(table(married.J$Household.Id)==2)

married.J$is.HH<-rep(sample(c(0,1),2,replace=F),nrow(married.J)/2)

demog.person[match(married.J$Person.Id,demog.person$Person.Id),]<- married.J


tmp <-demog.household$Household.Id[(demog.household$filing.status%in%"Married.Filing.Separately" &
                                      demog.household$HH%in%2)]

married.S<- demog.person[demog.person$Household.Id%in%tmp &
                           demog.person$Relationship.to.HH%in%1 ,]

## Sanity Check: all(table(married.S$Household.Id)==2)

married.S<-married.S[order(married.S$Household.Id),]

married.S$is.HH<-1

demog.person[match(married.S$Person.Id,demog.person$Person.Id),]<- married.S


### @*1: This insets deuplicates in the demog.person because a Household can have multiple headas
### see for example HH.lookup[HH.lookup$Household.Id%in%c("2000005"),]
HH.lookup <-demog.person[demog.person$is.HH%in%1,c("Person.Id","Household.Id")]
colnames(HH.lookup)<- c("HH.Person.Id","Household.Id")
demog.person<- merge(demog.person,HH.lookup, by="Household.Id")

tmp <- demog.household[,c("Household.Id","Income","Prop.Hideable.Income",
                          "filing.status","zipcode")]
demog.person<- merge(demog.person,tmp,by="Household.Id")


### Split the income evenly between heads of household of those married
### but filing seperatly. It only makes economic sense to file seperatly
### when incomes of both are nearly the same Hence the even split.
tmp<-demog.person$filing.status%in%"Married.Filing.Separately"
demog.person$Income[tmp]<- demog.person$Income[tmp]/2


demog.person<- merge(demog.person, 
          demog.household[,c("Household.Id","lon","lat")],by="Household.Id")


#############################################################
###                                                       ###
###        Load Activity            
###                                                       ###
#############################################################

activities.portland.file <- "../../Network Data/activities-portland-1-v2/activities-portland-1-v2.dat"
activities.purpose.lookup.file <- "../../Network Data/activities-portland-1-v2/activity.purpose.lookup.csv"

activity.purpose.lookup<- read.csv(file=activities.purpose.lookup.file,head=TRUE,stringsAsFactors = FALSE)

activity<- read.csv(file=activities.portland.file,head=FALSE,sep=" ",stringsAsFactors = FALSE)
names(activity) <- c("Household.Id","Person.Id","Activity.Id","Purpose.Id","Start.Time","Duration","Location.Id", "SubLocation.Id")

head(activity)
dim(activity)


#############################################################
###                                                       ###
###        Load Contact              
###                                                       ###
#############################################################



contact.portland.file <- "../../Network Data/contact-portland-1-v2/contact-portland-1-v2.dat"

contact<- read.csv(file=contact.portland.file,head=FALSE,sep=" ",stringsAsFactors = FALSE)
names(contact)<- c("Person.Id.1","Purpose.Id.1","Person.Id.2","Purpose.Id.2", "Contact.Duration")
## Contact Duration - Time in seconds of the contact over 24 hour period

dim(contact)
head(contact)

##Set Seed 
set.seed(55279) ### first seed given by Chris Marcum
set.seed(55333) 

#save.image("../Construct the Population/Portland.Network.RData")

#load("../Construct the Population/Portland.Network.RData")


#############################################################
###                                                       ###
###        Filter off conections              
###                                                       ###
#############################################################

## See http://stats.stackexchange.com/questions/128647/techniques-for-sampling-graphs-possibly-implemented-in-r-packages
## See https://arxiv.org/ftp/arxiv/papers/1308/1308.5865.pdf

contact.new <- contact

### filter off conections based on age
to.rm <- demog.person$Person.Id[(demog.person$Age<18 | demog.person$Age>70)]
to.rm <-  (contact.new$Person.Id.1%in%to.rm  | contact.new$Person.Id.2%in%to.rm)
contact.new <- contact.new[!(to.rm),] 

### filter off conections based on non-workers
# to.rm <- demog.person$Person.Id[(demog.person$Worker==2)]
# to.rm <-  (contact.new$Person.Id.1%in%to.rm  | contact.new$Person.Id.2%in%to.rm)
# contact.new <- contact.new[!(to.rm),] 

### filter off non head of houshold interactions.
to.keep <- demog.person$Person.Id[demog.person$Relationship.to.HH%in%1]
to.keep <-  (contact.new$Person.Id.1%in%to.keep  & contact.new$Person.Id.2%in%to.keep)
contact.new <- contact.new[to.keep,] 


###Now Relationship.to.HH==1 include both husband and wife. However is.HH clearly
### identifies which of the two is the real HH. 
### We want to remove the non real HH. But before that we want to get all edges of the 
### non real HH and pass them over to the real HH. 

non.HH <- demog.person[demog.person$Relationship.to.HH%in%1 &
                                   demog.person$is.HH%in%0, c("Household.Id","Person.Id","HH.Person.Id")]




contact.new$Person.Id.1 <-replace.via.look.up(contact.new$Person.Id.1,non.HH,rows=c("Person.Id","HH.Person.Id"))
contact.new$Person.Id.2 <-replace.via.look.up(contact.new$Person.Id.2,non.HH,rows=c("Person.Id","HH.Person.Id"))


contact.new<- contact.new[contact.new$Person.Id.1!=contact.new$Person.Id.2,]

### Filter based on activity type
purpose.to.include<-activity.purpose.lookup$Purpose.Id[activity.purpose.lookup$Purpose%in%c("Home","Work","Visit","Social/Recreation")]
purpose.to.include<- (contact.new$Purpose.Id.1%in%purpose.to.include & contact.new$Purpose.Id.2%in%purpose.to.include)
contact.new <- contact.new[purpose.to.include,]

contact.new$weight <- contact.new$Contact.Duration/(60^2*24)

contact.new <- contact.new[,c("Person.Id.1", "Person.Id.2","weight") ]


num.V <- length(unique(union(contact.new$Person.Id.1,contact.new$Person.Id.2)))
num.E <- nrow(contact.new)
ave.degree <- num.E/num.V
density <- num.E*2/((num.V-1)*num.V)

### @*1: Remove duplicates due to HH.Person.Id process see above @*1
df <- demog.person
df <- df[,!(names(df)%in%"HH.Person.Id")]
df <- df[!duplicated(df), ]
df$Worker <-1
demog.person<- df


#############################################################
###                                                       ###
###        Create igraph object   & Save data           
###                                                       ###
#############################################################


g.large <- graph.data.frame(contact.new, directed=FALSE, vertices=NULL)
### remove single dyads and non-conected
tmp <- clusters(g.large )$membership
tmp <- names(tmp)[tmp==1]
g.large <- induced_subgraph(g.large, tmp)


Person.ID.set <-names(V(g.large))

tmp <- (contact.new$Person.Id.1%in%Person.ID.set |
  contact.new$Person.Id.2%in%Person.ID.set)

contact.new<- contact.new[tmp,]

PN460.demog.person<- demog.person[demog.person$Person.Id%in%Person.ID.set,]
PN460.contact<-contact.new 

saveRDS(PN460.contact ,file="../Construct the Population/PN460.contact.RData")
saveRDS(PN460.demog.person ,file="../Construct the Population/PN460.demog.person.RData")

save.image(file="../Construct the Population/PN460.image.RData")

#############################################################
###                                                       ###
###        Do Mixing Matrix            
###                                                       ###
#############################################################


Person.Id.1_details<- get.portland.network.demo.details(contact.new$Person.Id.1)
Person.Id.2_details<- get.portland.network.demo.details(contact.new$Person.Id.2)

Household.Income.Cat.contact.new <- cbind(
  as.numeric(as.character(Person.Id.1_details$Household.Income.Cat)),
  as.numeric(as.character(Person.Id.2_details$Household.Income.Cat)))

table.Income.Cat.contact.new<- table(Household.Income.Cat.contact.new[,1],Household.Income.Cat.contact.new[,2])
tmp <- apply(table.Income.Cat.contact.new, MARGIN=1, FUN = function(x){return(round(x/sum(x),2))})
table.Income.Cat.contact.new <- t(tmp)


zipcode.contact.new <- cbind(as.character(Person.Id.1_details$zipcode),
                             as.character(Person.Id.2_details$zipcode))

table.zipcode.contact.new<- table(zipcode.contact.new[,1],zipcode.contact.new[,2])
tmp <- apply(table.zipcode.contact.new, MARGIN=1, FUN = function(x){return(round(x/sum(x),3))})
table.zipcode.contact.new <- t(tmp)



#############################################################
###                                                       ###
###        Analyze the  igraph object             
###                                                       ###
#############################################################


dist<- degree_distribution(g.large)
summary(degree(g.large))
text.label <- summary(degree(g.large))
#tmp <- eigen_centrality(g.large)
#summary(tmp$vector)
dist <- cbind(x=c(1:length(dist)), y=dist)
dist<- as.data.frame(dist)

degree.dist.plot <-ggplot(dist) +
  geom_line(aes(x=x, y=y),alpha=1,size=2, color="magenta4")+
  xlab("Degree") +
  ylab("Prob Density")+
  ggtitle(paste("Degree Distribution for the Full Portland Data with",vcount(g.large) ,"nodes and", nrow(contact.new),"edges",sep=" "))+
  xlim(0,120)+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=14 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))+
  annotate("text", label = c(paste(names(text.label),text.label, sep="=")),
           x=100,y=0.02+c(1:6)*0.004, size = 8, colour = "black",angle = 0)

print(degree.dist.plot)
ggsave(degree.dist.plot,
       file=paste(figures.dir,"Porland_Degree_dist_ReducedContacts"
                  ,".pdf",sep=""),  width=10, height=5)

head(contact.new)

Person.ID.set <- unique(c(contact.new$Person.Id.1,contact.new$Person.Id.2 ))

demog.person.sub <- demog.person[demog.person$Person.Id%in%Person.ID.set,]

demog.person.sub <- merge(demog.person.sub, 
                          demog.household[,c("Household.Id","lon","lat")],by="Household.Id" )

text.label <- summary(signif(demog.person.sub$Income,2))

income.plot<-ggplot(demog.person.sub) +
  geom_histogram(aes(x=Income,y=100*..count../sum(..count..)),bins = 100,color="black",fill="magenta4") +
  theme_bw() +
  xlim(0,250000)+
  xlab("Income ($)") +
  ylab("Percent of Households")+
  ggtitle(paste("Income Distribution for the Reduced Portland Data with", vcount(g.large), "nodes and", ecount(g.large),"edges",sep=" "))+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=14 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))+
  annotate("text", label = c(paste(names(text.label),text.label, sep="=")),
           x=100000,y=2+c(1:6)*0.4, size = 8, colour = "black",angle = 0)

print(income.plot)
ggsave(income.plot,
       file=paste(figures.dir,"Porland_HouseHold_Income_dist_ReducedContacts"
                  ,".pdf",sep=""),  width=10, height=5)


#############################################################
###                                                       ###
###        Constructed a connected, representative Subnetwork            
###                                                       ###
#############################################################


source("../Construct the Population/Do.the.rewiring.R")



#############################################################
###                                                       ###
###     Get Number of dependents             
###                                                       ###
#############################################################


library.dir   <- "Survey code/SurveyLibrary/"
library <- file.path(library.dir, "library.R")
source(library)


colnames(demog.household) <- tolower(colnames(demog.household))

PN1.file<- "Network data/PN1.demog.person.RData" 
PN10.file<- "Network data/PN10.demog.person.RData"  
PN460.file<- "Network data/PN460.demog.person.RData"

pop.file.list<- c(PN1=PN1.file,
                    PN10=PN10.file)

for(i in pop.file.list){
  
  pop.data <-readRDS(file=i) 
  colnames(pop.data) <- tolower(colnames(pop.data))

  pop.data <- merge(pop.data, demog.household[,c("household.id","household.size")],by="household.id")
  
  pop.data$num.dependents <- pop.data$household.size
  
  
  tmp<-table(pop.data$household.id)
  pop.data[pop.data$household.id%in%(names(tmp[tmp>1])),"num.dependents"] <- sapply(pop.data[pop.data$household.id%in%(names(tmp[tmp>1])),"household.size"]/2,stoch.round)
  
  pop.data$num.dependents <- pop.data$num.dependents-1
  
  saveRDS(pop.data,file=i) 
}

pop.data.raw <- pop.data
population.data.file = "Inputs/PN10_population_data.csv"
pop.data <- get.population.data(population.data.file)

pop.data <- merge(pop.data, pop.data.raw[,c("person.id","household.size","num.dependents")],by="person.id")

write.csv(pop.data, 
          paste(population.data.file,sep=""), 
          row.names = FALSE)



pop.file.list<- c(PN1=PN1.file,
                  PN10=PN10.file,
                  PN460=PN460.file)

gender.tab<- age.tab<- filing.status.tab <- list()
for(i in pop.file.list){
  
  pop.data <-readRDS(file=i) 
  colnames(pop.data) <- tolower(colnames(pop.data))
  if(any(pop.data$gender==2)) {pop.data$gender= 2-pop.data$gender}
  
  gender.tab[[i]]<-round(100*table(pop.data$gender,pop.data$filing.status)/
                           nrow(pop.data),2)
  rownames(gender.tab[[i]]) <- c("Females","Males")
  tmp<-100*table(pop.data$age)/nrow(pop.data)
  age.tab[[i]]<-round(c(sum(tmp[1:(1+(25-18))]),
  sum(tmp[9:(9+(35-26))]),
  sum(tmp[19:(19+(50-36))]),
  sum(tmp[34:(34+(64-51))]),
  sum(tmp)-sum(tmp[1:47])),2)
}

age.tab<- as.data.frame(do.call("rbind",age.tab))
rownames(age.tab)<- c("PN1","PN10","PN460")
colnames(age.tab)<- c("18-25","25-35","36-50","51-64","65+")

gender.tab<- as.data.frame(do.call("rbind",gender.tab))
rownames(gender.tab)<- c("PN1.Females","PN1.Males","PN10.Females",
                         "PN10.Males","PN460.Females","PN460.Males")

library(xtable)

print(xtable(gender.tab,digits=2,auto=T),file=paste("Writeup/Tables/NetworkTabGender",".tex",sep=""), include.rownames=T,sanitize.text.function = function(x){x})

print(xtable(age.tab,digits=2,auto=T),file=paste("Writeup/Tables/NetworkTabAge",".tex",sep=""), include.rownames=T,sanitize.text.function = function(x){x})

pop.data <-readRDS(file=PN10.file) 


tmp <- split(pop.data,pop.data$filing.status)
tmp <-sapply(tmp,FUN=function(x){round(summaryfunctionFull(x$income),0)})
tmp <- rbind(PN10=round(summaryfunctionFull(pop.data$income),0),t(tmp))


print(xtable(tmp),file=paste("Writeup/Tables/IncomeDistPN10",".tex",sep=""), include.rownames=T)



#############################################################
###                                                       ###
###     Other corrections             
###                                                       ###
#############################################################


data.dir   <- "Tax data/"
tax.rate.income.bracket.file <- paste(data.dir,"US_Income_Tax_Rates_2016",".csv",sep="")
tax.rate.income.bracket <- read.csv(file=tax.rate.income.bracket.file,head=TRUE,stringsAsFactors = FALSE)
tax.rate.income.bracket <- split(tax.rate.income.bracket,tax.rate.income.bracket$filing.status)

DT.tax.rate.income.bracket.file <- paste(data.dir,"US_Income_Tax_Rates_Trump",".csv",sep="")
DT.tax.rate.income.bracket <- read.csv(file=DT.tax.rate.income.bracket.file,head=TRUE,stringsAsFactors = FALSE)
DT.tax.rate.income.bracket <- split(DT.tax.rate.income.bracket,DT.tax.rate.income.bracket$filing.status)

family.income.distrubution<- read.csv(file="Tax data/CPS_2015_family_income.csv",head=T,stringsAsFactors = FALSE)
family.income.distrubution$proprtion<- family.income.distrubution$number/sum(family.income.distrubution$number)


PN1.file<- "Network data/PN1.demog.person.RData" 
PN10.file<- "Network data/PN10.demog.person.RData"  
PN460.file<- "Network data/PN460.demog.person.RData"

pop.file.list<- c(PN1=PN1.file,
                  PN10=PN10.file,
                  PN460=PN460.file)

population.data.file <- PN1.file

for(population.data.file in pop.file.list){
  pop.data <-readRDS(file=population.data.file) 
  #pop.data <- correct.incomes(family.income.distrubution,pop.data)
  
  income.info<- pop.data[,c("income","filing.status")]
  income.info$Income <- income.info$income
  income.info$Income[income.info$Income==0]<-1
  income.tax <- 
    apply(income.info,1,get.effective.tax.rate,tax.rate.income.bracket=tax.rate.income.bracket)
  
  DT.income.tax <- 
    apply(income.info,1,get.effective.tax.rate,tax.rate.income.bracket=DT.tax.rate.income.bracket)
  
  ### change in effective tax rate from status quo to Donald Trump's proposed taxes. 
  print(summary(round(DT.income.tax/ income.info$Income,3)-round(income.tax/ income.info$Income,3) ))
  
  pop.data$tax.rate<-  round(income.tax/ income.info$Income,3)
  
  saveRDS(pop.data,file=population.data.file)
}


population.data.file = "Inputs/PN1_population_data.csv"
write.csv(pop.data, 
          paste(population.data.file,sep=""), 
          row.names = FALSE)

pop.data <- get.population.data(population.data.file)

