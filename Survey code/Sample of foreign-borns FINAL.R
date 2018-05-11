library(foreign)
library(readstata13)
#ms36 data
SampleData<-read.dta13("Survey data/Sample of foreign-borns/ALP_MS36_2016_08_22_11_08_24.dta",
                       generate.factors = T)

#List of active foreign-born panelists
xelacfor <- read.csv("Survey data/Sample of foreign-borns/xelacfor.csv")

#Generated a binary variable for those who are in the list of active foreign-born panelists
SampleData$In <- SampleData$ms36_prim_key %in% xelacfor$prim_key

#Selected those who are not a born US-citizen and who do not usually speak English at home
ForeignData <- SampleData[(SampleData$ms36_b086_ == "5 NO No" & 
                             SampleData$ms36_b054_ == "5 NO No"),]

#Saved the following dataframe in a csv file
attach(ForeignData)
write.csv(data.frame(ms36_prim_key,In, ms36_hispaniclatino, ms36_b085_,ms36_b086_,ms36_b087_,ms36_birthyear),
          "/Users/galiyev/Documents/OJT/Tax evasion/Survey data/Sample of foreign-borns/Reduced Sample 2.csv")
detach(ForeignData)

#In the saved csv file I removed prim_keys that are not in the list of active foreign-born panelist.
#I also removed two panelist whose birth year or citizenship year was not available