get.Swedish.Gini.Transformed.Income<-function(pop.data, weight.se.gini=1,
                                              match.income.or.IT.rev="income",
                                              data.dir= "Tax data/"){
  
  require(ineq)
  require(foreign)
  
  income.info<- pop.data[,c("income","filing.status")]
  
  tax.rate.income.bracket.file <- paste(data.dir,"US_Income_Tax_Rates_2016",".csv",sep="")
  tax.rate.income.bracket <- read.csv(file=tax.rate.income.bracket.file,
                                      head=TRUE,stringsAsFactors = FALSE)
  tax.rate.income.bracket <- split(tax.rate.income.bracket,tax.rate.income.bracket$filing.status)
  
  
  ### data from ZA6770: International Social Survey Programme: 
  ### Work Orientations IV - ISSP 2015 (ZA6770_v2-1-0.dta.zip)
  ### https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6770 
  ZA6770.data<- suppressWarnings(read.dta(paste(data.dir,"ZA6770_v2-1-0.dta",sep=""),
                                          convert.factors = T))
  
  SE<- ZA6770.data[ZA6770.data$country%in%"SE-Sweden",]
  
  se.income <-SE$SE_INC
  se.income<- as.character(se.income)
  se.income<- se.income[!(se.income%in%"No answer")]
  
  se.income <- sapply(strsplit(se.income, split='.', fixed=TRUE), function(x) (x[1]))
  SEK.dolloar.xchange <- 0.12 ## one SEK equals 0.12 dollars.
  se.income<- as.numeric(se.income)*1000*12*SEK.dolloar.xchange
  
  
  ### However sweden gini is 24.9 
  ### see https://en.wikipedia.org/wiki/List_of_countries_by_income_equality
  ### so lets narrow the data further to get this 

  se.income<- se.income-(se.income-median(se.income))*0.32
  
  ### https://en.wikipedia.org/wiki/Median_income SE/US Median income ratio is 50514/43585=1.16
  ### We instead get median(se.income)/median(old.income)=1.3
  #### However mean are about the same mean(se.income)/mean(old.income) =1.05
  
  old.income <- pop.data$income
  rescale.se.income <- mean(se.income)/mean(old.income)
  se.income <- se.income/rescale.se.income
  
  #set.seed(1000)
  se.income<- sample(se.income,length(old.income),replace=T)
  tie.breaker.income <- runif(length(old.income),-1,1)
  se.income <- se.income+ tie.breaker.income 
  se.income<- sort(se.income)
  se.income[order(old.income)]<- se.income
  
  #ineq(se.income,type="Gini")
  
  new.income <-  round(weight.se.gini*se.income+(1-weight.se.gini)*old.income,0)
  
  
  # scaling.factor <- sum(old.income*eff.tax)/sum(new.income*eff.tax)
  # new.income <- scaling.factor*new.income
  
  income.info$income <- new.income
  income.info$Income <- income.info$income
  income.info$Income[income.info$Income==0]<-1
  income.tax <- 
    apply(income.info,1,get.effective.tax.rate,tax.rate.income.bracket=tax.rate.income.bracket)
  tax.rate<-  round(income.tax/ income.info$Income,3)
  
  ### Ideally the income tax revenues after this trasformation is the same.
  ### the error in total income is 100*(1-sum(new.income)/sum(old.income))~2%
  ### Error in mean tax rate is  mean(tax.rate-pop.data$tax.rate)*100 =0.34%
  ### Error in mean tax rate is  100*(1-sum(tax.rate*new.income)/sum(pop.data$tax.rate*old.income)) =8%
  
  if(match.income.or.IT.rev=="income"){
    ### rescale income to match total income of pop.data
     new.income <- new.income/(sum(new.income)/sum(old.income))
  }else{
    ### rescale income to match total income tax revenues collected by pop.data
     new.income <- new.income/(sum(tax.rate*new.income)/sum(pop.data$tax.rate*old.income))
  }
  
  pop.data$income <-  new.income
  pop.data$tax.rate <- tax.rate
  
  return(pop.data)
}
