correct.incomes<- function(family.income.distrubution,pop.data,targets = c(71000,34940,117795,64819,35876,0.66)){
  
  ### some targets come from https://www.fool.com/retirement/2016/10/30/heres-the-average-american-household-income-how-do.aspx
  ### and https://www.census.gov/prod/2013pubs/p20-570.pdf
  
  boh<-as.numeric(family.income.distrubution$number*family.income.distrubution$Mean.Income)
  
  mean.income<- sum(boh)/sum(family.income.distrubution$number)
  target <- targets[1]
  
  factor <- target/mean.income
  
  tmp.list<- list()
  x <- family.income.distrubution
  for(i in 1:nrow(x)){
    tmp<- rbind(c(LB=x$H.income.LB[i],UB=x$Mean.Income[i],proportion=x$proprtion[i]/2),
                c(LB=x$Mean.Income[i],UB= x$H.income.UB[i], proportion=x$proprtion[i]/2))
    tmp.list[[i]]<- tmp
  }
  x<- as.data.frame(do.call("rbind",tmp.list))
  
  N<- nrow(pop.data)
  
  
  y<-sample(1:nrow(x), N,replace=T,prob=x$proportion)
  
  z<- NULL
  for(i in 1:length(y)){
    z<-c(z,factor *runif(1,x$LB[y[i]],x$UB[y[i]]))
  }
  z<- z*target/mean(z)
  z<- round(sort(z),0)

  new.incomes <- pop.data$income
  new.incomes[order(pop.data$income)] <-z
  
  z <- data.frame(filing.status=pop.data[,c("filing.status")],income=new.incomes) 
  
  m1 <- mean(z$income[z$filing.status%in%"Single"]) ## $34,940
  m2 <- mean(z$income[z$filing.status%in%"Married.Filing.Jointly"]) ## $117,795
  m3 <- mean(z$income[z$filing.status%in%"Married.Filing.Separately"]) ## $64,819
  m4 <- mean(z$income[z$filing.status%in%"Head.of.Household"]) ## $35,876
  m5 <- targets[6]
  
  
  group <- (1:N)[z$filing.status%in%"Head.of.Household"]
  tmp <- order(z$income[group],decreasing = T)
  for(tt in 1:length(tmp)){
    if(mean(z$income[z$filing.status%in%"Head.of.Household"])>targets[5]){
      y<-group[tmp[tt]]
      if(mean(z$income[z$filing.status%in%"Married.Filing.Separately"])<targets[4]){
        z$filing.status[y]<-"Married.Filing.Separately" }else{
          z$filing.status[y]<-"Married.Filing.Jointly"
        }
    }
  }
  
  
  group <- (1:N)[z$filing.status%in%"Single" & z$income>mean(z$income[z$filing.status%in%"Married.Filing.Jointly"])]
  tmp <- order(z$income[group],decreasing = T)
  for(tt in 1:length(tmp)){
    y<-group[tmp[tt]]
    if(mean(z$income[z$filing.status%in%"Married.Filing.Separately"])<targets[4] |
       mean(z$income[z$filing.status%in%"Married.Filing.Jointly"])<targets[3]){
      z$filing.status[y]<-"Married.Filing.Separately" }else{
        z$filing.status[y]<-"Married.Filing.Jointly"
      }
  }
  
  
  m1 <- mean(z$income[z$filing.status%in%"Single"]) ## $34,940
  m2 <- mean(z$income[z$filing.status%in%"Married.Filing.Jointly"]) ## $117,795
  m3 <- mean(z$income[z$filing.status%in%"Married.Filing.Separately"]) ## $64,819
  
  
  
  for(tt in 1:10000){
    i<- sample(1:N,1)
    prob.marr <- sum(z$filing.status%in%c("Married.Filing.Jointly","Married.Filing.Separately"))/N
    if(!(z$filing.status[i]%in%c("Head.of.Household","Single")) & prob.marr>m5){
      tmp <- z$filing.status[i]
      z$filing.status[i]<-"Single"
      
      a<-mean(z$income[z$filing.status%in%"Single"])>m1 & mean(z$income[z$filing.status%in%"Single"])<targets[2] 
      b<-mean(z$income[z$filing.status%in%"Married.Filing.Jointly"])>m2 & z$income[i]<targets[3]
      c<-mean(z$income[z$filing.status%in%"Married.Filing.Separately"])>m3 & z$income[i]<targets[4]
      
      if(!(a & (b |c))){
        z$filing.status[i]<-tmp
      }else{
        m1<- mean(z$income[z$filing.status%in%"Single"])
        m2<- mean(z$income[z$filing.status%in%"Married.Filing.Jointly"])
        m3<- mean(z$income[z$filing.status%in%"Married.Filing.Separately"])
      }
    }
  }
  
  
  for(i in c("Married.Filing.Jointly","Married.Filing.Separately","Single","Head.of.Household")){
    print(i)
    print(summary(new.incomes[z$filing.status%in%i]))
  }
  
  pop.data[,c("filing.status","income")] <- z
  
  return(pop.data)

}
