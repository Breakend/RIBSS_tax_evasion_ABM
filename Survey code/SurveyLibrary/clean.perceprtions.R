clean.perceprtions<-function(x,apply.cuttoff=TRUE){
  
  cutoff.list<- list()
  quantile.list <- list()
    
  x$perceivedunderreporttaxhigher[x$prim_key%in%"12024686:1"]<- 0### this respondent gave "0o" as a response here
    
    per.aud.rate <- as.numeric(x[,"perceivedauditrate"])
    per.aud.rate.mag <- x[,"perceivedauditratemagnifier"]
    per.aud.rate <- ALP.magnifier.fill(per.aud.rate, per.aud.rate.mag )
    
    q<-quantile(per.aud.rate,prob=c(0:20)/20,na.rm=T)
    cutoff<-min(q["95%"],30)
    tmp<-rev(which(abs(q-cutoff)==min(abs(q-cutoff))))[1]
    cutoff<-q[tmp]
    if(!apply.cuttoff){cutoff<-q["100%"]}
    per.aud.rate[per.aud.rate>cutoff] <- cutoff
    
    x[,"perceivedauditrate"] <- per.aud.rate
    cutoff.list[["perceivedauditrate"]] <- c(names( cutoff),cutoff)
    quantile.list[["perceivedauditrate"]] <- q 
    
    ### Question 9:  Get Perceived audit rate if taxpayer is under-reporting
    auditrandom<- as.factor(x$auditrandom)
    levels(auditrandom)<- c("30%","60%","90%")
    under.report.per.aud.rate <- as.factor(x[,"perceivedarunderreport"])
    levels(under.report.per.aud.rate) <- c("higher","lower","same")
    
    per.ARUP <- as.numeric(x[,"perceivedaruprob"])
    per.ARUP.mag <- x[,"perceivedarumagnifier"]
    per.ARUP <- ALP.magnifier.fill(per.ARUP, per.ARUP.mag )
    if(!apply.cuttoff){cutoff<-q["100%"]}
    per.ARUP[per.aud.rate>cutoff] <- cutoff
    
    q<-quantile(per.ARUP/per.aud.rate,prob=c(0:20)/20,na.rm=T)
    cutoff<-min(q["95%"],20)
    tmp<-rev(which(abs(q-cutoff)==min(abs(q-cutoff))))[1]
    cutoff<-q[tmp]
    per.ARUP[per.ARUP/per.aud.rate>cutoff] <- cutoff
    if(!apply.cuttoff){cutoff<-q["100%"]}
    
    x[,"auditrandom"] <- auditrandom
    x[,"perceivedarunderreport"] <- under.report.per.aud.rate 
    x[,"perceivedaruprob"]<- per.ARUP
    cutoff.list[["perceivedaruprob.factor"]] <- c(names( cutoff),cutoff)
    quantile.list[["perceivedaruprob.factor"]] <- q 
    
    
    ### Question 10:  Bomb-Crater
    BC <- as.factor(x[,"bombcrater"])
    levels(BC) <- c("higher","lower","same")
    
    per.BCA <- as.numeric(x[,"bombcrateramount"])
    per.BCA.mag <- x[,"bombcrateramountmagnifier"]
    per.BCA <- ALP.magnifier.fill(per.BCA, per.BCA.mag )
    if(!apply.cuttoff){cutoff<-q["100%"]}
    per.BCA[per.aud.rate>cutoff] <- cutoff
    
    q<-quantile(per.BCA/per.aud.rate,prob=c(0:20)/20,na.rm=T)
    cutoff<-min(q["95%"],20)
    tmp<-rev(which(abs(q-cutoff)==min(abs(q-cutoff))))[1]
    cutoff<-q[tmp]
    if(!apply.cuttoff){cutoff<-q["100%"]}
    per.BCA[per.BCA/per.aud.rate>cutoff] <- cutoff
    
    
    x[,"bombcrater"] <- BC 
    x[,"bombcrateramount"] <- per.BCA 
    cutoff.list[["bombcrateramount.factor"]] <- c(names( cutoff),cutoff)
    quantile.list[["bombcrateramount.factor"]] <- q
    
    ### Question 11:  Get Perceived penalty
    if(max(x[,"perceivedpenaltyrate"],na.rm=T)==15000){
    per.pen.rate<- x[,"perceivedpenaltyrate"]/10 ## was out of $1000.thus divide by10 for %
    }else{
      stop("penalty something wrong")
    }
    
    q<-quantile(per.pen.rate,prob=c(0:20)/20,na.rm=T)
    cutoff<-min(q["95%"],150)
    tmp<-rev(which(abs(q-cutoff)==min(abs(q-cutoff))))[1]
    cutoff<-q[tmp]
    if(!apply.cuttoff){cutoff<-q["100%"]}
    per.pen.rate[per.pen.rate>cutoff] <-  cutoff
    
    x[,"perceivedpenaltyrate"] <- per.pen.rate
    cutoff.list[["perceivedpenaltyrate"]] <- c(names( cutoff),cutoff)
    quantile.list[["perceivedpenaltyrate"]] <- q
    
    ### Question 12:  Get Perceived effective tax rate
    per.tax.rate <- x[,"perceivedtaxrate"]
    
    q<-quantile(per.tax.rate,prob=c(0:20)/20,na.rm=T)
    cutoff<-min(q["95%"],60)
    tmp<-rev(which(abs(q-cutoff)==min(abs(q-cutoff))))[1]
    cutoff<-q[tmp]
    if(!apply.cuttoff){cutoff<-q["100%"]}
    per.tax.rate[per.tax.rate>cutoff] <-  cutoff
    
    x[,"perceivedtaxrate"] <- per.tax.rate
    cutoff.list[["perceivedtaxrate"]] <- c(names( cutoff),cutoff)
    quantile.list[["perceivedtaxrate"]] <- q
    
    ### Question 16:  People caught by the IRS 
    per.caught <- x[,"perceivedcaught"]
    q<-quantile(per.caught,prob=c(0:20)/20,na.rm=T)
    cutoff<-min(q["95%"],30)
    tmp<-rev(which(abs(q-cutoff)==min(abs(q-cutoff))))[1]
    cutoff<-q[tmp]
    if(!apply.cuttoff){cutoff<-q["100%"]}
    per.caught[ per.caught>cutoff] <-  cutoff
    per.caught <- pmin(per.caught,per.aud.rate)
    
    x[,"perceivedcaught"] <- per.caught
    cutoff.list[["perceivedcaught"]] <- c(names( cutoff),cutoff)
    quantile.list[["perceivedcaught"]] <- q
    
    
    quantile <-do.call("rbind",  quantile.list)
    quantile<- as.data.frame(quantile)
    
    tmp <-do.call("rbind", cutoff.list)
    tmp<- as.data.frame(tmp)
    colnames(tmp)<- c("cutoff quantile", "cutoff value")
    tmp[,2] <- round(as.numeric(as.character(tmp[,2])),2)
    r<- list(SD.cleaned=x,cutoff.list=tmp, quantile=quantile)
    return(r)
}
    

    
    
    
    