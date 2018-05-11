library(foreign)
library(readstata13)
SampleData<-read.dta13("/Users/galiyev/Documents/OJT/Tax evasion/Survey data/Sample of foreign-borns/ALP_MS36_2016_08_22_11_08_24.dta",
                       generate.factors = T)
View(SampleData)

table(SampleData$ms36_b085_, exclude=NULL) # Are you a citizen of the USA?
table(SampleData$ms36_b086_, exclude=NULL) # Were you born a citizen of the USA?
table(SampleData$ms36_b087_, exclude=NULL) # In what year did you become a US citizen?
table(SampleData$ms36_birthyear, exclude=NULL) # Birth year
table(SampleData$ms36_hispaniclatino, exclude=NULL) #Is the respondent Hispanic-Latino?
table(SampleData$ms36_hispaniclatino, SampleData$ms36_b086_, exclude = NULL)


NonHispForeignData <- SampleData[(SampleData$ms36_b086_ == "5 NO No" & SampleData$ms36_hispaniclatino != "1 Yes"),]
table(NonHispForeignData$ms36_hispaniclatino, NonHispForeignData$ms36_b086_, exclude = NULL)


attach(NonHispForeignData)
write.csv(data.frame(ms36_prim_key, ms36_b085_,ms36_b086_,ms36_b087_,ms36_birthyear),
          "/Users/galiyev/Documents/OJT/Tax evasion/Survey data/Sample of foreign-borns/Reduced Sample.csv")
detach(NonHispForeignData)

#View(SampleData)
attach(SampleData)
varlist <- c("ms36_b085_", "ms36_b086_", "ms36_b087_")
for(i in 1:3){
 z <- varlist[i]
 v <- table(SampleData[,z], exclude = NULL)
 print(v)
}

detach(SampleData)

