get.qP.lognormal.fit.plm<-function(z,c1.field ="c1.guess.majority.int", c2=0.7,ET.slope=1, model ="within"){
  # options(warn=1)
   z<- df.hypo
   c1.field ="c1.guess.majority.int"
   c2=0.7
   ET.slope=ET.slope.c1.tilde
  
  z<-z[z$control%in%c("baseline","auditrate","penaltyrate"),
       c("prim_key","control","question","perceivedtaxrate",
         "c1.guessed","c1.guess.majority.int","tax.rate.threshold.tri",
         "perceivedauditrate",
         "perceivedpenaltyrate","perceivedevasionrate")]
  
  colnames(z)[colnames(z)%in%"perceivedevasionrate"] <- "E"
  colnames(z)[colnames(z)%in%"perceivedtaxrate"] <- "T"
  colnames(z)[colnames(z)%in%"perceivedauditrate"] <- "q"
  colnames(z)[colnames(z)%in%"perceivedpenaltyrate"] <- "P"
  
  summaryfunctionFull(z$E) 
  
  z[,!(colnames(z)%in%(colnames(z)[1:3]))]<-z[,!(colnames(z)%in%(colnames(z)[1:3]))]/100
  z[is.na(z[,c1.field]),c1.field] <- 
    mean(z[,c1.field],na.rm=T)
  
  filter0.num.rec <-length(unique(z$prim_key))
  
  ### What we need here is how the tax rate T* where everyone is compliant changes with 
  ### the product of the audit rate and penalty rate (i.e., qP). At qP=0 we have T*(0)=c1
  ### we assume that as qP-> 1 we have T*(qP->1) -> c2.
  ### So the survey data gives us T*(0) (i.e., c1).
  ### Now ideally we would have a set of T*(qP) points where qP>0.
  ### in order for this to be these points would require evasion rate to stay at 0%.
  ### However in our data we have asked to estimate the evasion rate amongst people like you
  ### under hypothetical scenarios of different tax, audit and penalty rates.
  ### Thus given an audit and penalty rate and a response for the evasion rate (E) >0 
  ### the tax rate is not quite T*(qP) since the evasion rate needs to be at 0. 
  
  ### for example say q=10% and P=10% and T= 25% and E=1%. We see that T is very close to T*.
  ### since we previously found that the elasticity of E wrt T is close to 1 we could assume that
  ### T* = 25% *(1-0.01) at qP=0.01 or assume T*=25% at qP=0.01*(1+0.01).
  ### The latter however is not backed by an elasticity of 1 finding - and so we choose the former. 
  ### for in this case the resetting factor f =0.01 and T*=(1-f)*25%
  
  
  ### Therefore here we create a dataframe column Delta.T = (T*-c1)/(c2-c1)  where T*=(1-f)*T
  ### and column qP
  
  z$qP <- z$q*z$P   #### *(1+z$E)  
  #z$Tstar <- z$T*(1-z$E)
  z$Tstar <- z$T-z$E/ET.slope 
  
  ### T* can not be greater than c2. If so we need to filter off these records.
  
  z<- z[z$Tstar<c2,]
  filter1.num.rec <-length(unique(z$prim_key))
  
  z$Delta.T <- (pmax(z$Tstar -z[,c1.field],0)/
                  pmax(c2-z[,c1.field],0))
  z<- z[, c("prim_key","Delta.T","qP")]
  
  z$qP[z$Delta.T==0] <- 0 
  
  summaryfunctionFull(z$Delta.T)
  summaryfunctionFull(z$qP)
 
  #### Run a panel level linear model
  rec <- z
  rec<- rec[rec$Delta.T>0 & rec$qP>0,]  
  
  tmp <-data.frame(prim_key=unique(rec$prim_key),Delta.T=0.99,qP=0.99)
  
  rec <- rbind(rec,tmp)
  
  rec$X <- log(rec$qP)
  rec$Y <- erf.inv(2*rec$Delta.T - 1)
  
  
  #write.csv(rec, paste(survey.data.dir,"data.qP.lognormal.fit.plm",".csv",sep=""),  row.names = F)
  
  
  plm.model <- plm(Y~X,
                   data = rec, 
                   effect = "individual", 
                   model = model, 
                   index = "prim_key")
  

    
  if(model%in%"within"){  
    tmp <- summary(plm.model)$coefficients
    k<-c(tmp[,"Estimate"]- 0.675*sqrt(nrow(rec))*tmp[,"Std. Error"],
      tmp[,"Estimate"],
      tmp[,"Estimate"]+ 0.675*sqrt(nrow(rec))*tmp[,"Std. Error"])
    c<- summaryfunctionFull(fixef(plm.model))[c("1st Qu.",   "Median" , "3rd Qu.")]
  }
  if(model%in%"random"){
    
    tmp <- summary(plm.model)$coefficients
    k<-c(tmp["X","Estimate"]- 0.675*sqrt(nrow(rec))*tmp["X","Std. Error"],
         tmp["X","Estimate"],
         tmp["X","Estimate"]+ 0.675*sqrt(nrow(rec))*tmp["X","Std. Error"])
    c<- c(tmp["(Intercept)","Estimate"]- 0.675*sqrt(nrow(rec))*tmp["(Intercept)","Std. Error"],
          tmp["(Intercept)","Estimate"],
          tmp["(Intercept)","Estimate"]+ 0.675*sqrt(nrow(rec))*tmp["(Intercept)","Std. Error"])
  }
  
  s <- k*sqrt(2)
  m <- exp(-c/k)
  
  R<-data.frame(rbind(s=s,m=m))
  
  colnames(R)<-c("1st Qu.",   "Median" , "3rd Qu.")
  
  
  print(R)

  return(R)
  
  }

  
  
  
  