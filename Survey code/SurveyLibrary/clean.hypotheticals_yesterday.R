clean.hypotheticals_yesterday<-function(x, short.format = TRUE, grace.factor=0.05){
  
  tab <- list(NULL)
  
  if(short.format){
    
    x<-x[,c("prim_key", 
                  "perceivedtaxrate", 
                  "perceivedpenaltyrate",
                  "perceivedauditrate",
                  "perceivedevasionrate",
                  "perceivedunderreporttaxlower",
                  "perceivedunderreporttaxmuchlower",
                  "perceivedunderreporttaxhigher",
                  "perceivedunderreporttaxmuchhigher",
                  "perceivedunderreportpenaltylower",
                  "perceivedunderreportpenaltymuchlower",
                  "perceivedunderreportpenaltyhigher",
                  "perceivedunderreportpenaltymuchhigher",
                  "perceivedunderreportaudithigher",
                  "perceivedunderreportauditmuchhigher")]
#                   "tax.rate.threshold.min",
#                   "tax.rate.threshold.max",
#                   "c1.guessed")]
    
    
    x[x==""]<-NA
    
    df.ori<- x
    
    ### Correct the skip logic: In the survey if respondent gave 0 for perceivedunderreportpenaltyhigher
    ### s/he was not asked perceivedunderreportpenaltymuchhigher. Thus these are NA in the data,
    ### but these NA need to be changed to 0. Also invert the order if inconsistent
    
    tmp <- (x$perceivedunderreportpenaltyhigher==0) & is.na(x$perceivedunderreportpenaltymuchhigher)
    x$perceivedunderreportpenaltymuchhigher[tmp] <- 0
    
    a <- x$perceivedunderreportpenaltyhigher
    b <- x$perceivedunderreportpenaltymuchhigher
    need.to.invert <- (a < b)
    need.to.invert[is.na(need.to.invert)] <-F
    x$perceivedunderreportpenaltymuchhigher[need.to.invert] <- a[need.to.invert]
    x$perceivedunderreportpenaltyhigher[need.to.invert] <- b[need.to.invert]
     
    
    ### Correct the skip logic 2:  In the survey if respondent gave 0 for perceivedunderreportaudithigher
    ### s/he was not asked perceivedunderreportauditmuchhigher. Thus these are NA in the data, 
    ### but these NA need to be changed to 0.
    
    tmp <- (x$perceivedunderreportaudithigher==0) & is.na(x$perceivedunderreportauditmuchhigher)
    x$perceivedunderreportauditmuchhigher[tmp] <- 0
    
    a <- x$perceivedunderreportaudithigher
    b <- x$perceivedunderreportauditmuchhigher
    need.to.invert <- (a < b)
    need.to.invert[is.na(need.to.invert)] <-F
    x$perceivedunderreportauditmuchhigher[need.to.invert] <- a[need.to.invert]
    x$perceivedunderreportaudithigher[need.to.invert] <- b[need.to.invert]
    
    ### Correct the skip logic 3:  In the survey if respondent gave 0 for perceivedunderreportpenaltymuchlower
    ### s/he was not asked perceivedunderreportpenaltylower. Thus these are NA in the data, 
    ### but these NA need to be changed to 0.
    
    tmp <- (x$perceivedunderreportpenaltymuchlower==0) & is.na(x$perceivedunderreportpenaltylower)
    x$perceivedunderreportpenaltylower[tmp] <- 0
    
    a <- x$perceivedunderreportpenaltylower
    b <- x$perceivedunderreportpenaltymuchlower
    need.to.invert <- (a > b)
    need.to.invert[is.na(need.to.invert)] <-F
    x$perceivedunderreportpenaltymuchlower[need.to.invert] <- a[need.to.invert]
    x$perceivedunderreportpenaltylower[need.to.invert] <- b[need.to.invert]
    
    
    x[,-1] <- as.data.frame(apply(x[,-1],2,as.numeric))
    
    names.hypo.data.fields<- colnames(x)[!(colnames(x)%in%c("prim_key", 
                                              "perceivedtaxrate", 
                                              "perceivedpenaltyrate",
                                              "perceivedauditrate"))]
    
    tmp <- 0
    for(ii in names.hypo.data.fields){
      tmp <- tmp+ sum(!is.na(x[,ii])) 
    }
    tab[["full data"]]<-  c(respondents =nrow(x),data.points= tmp)
    
    x<- x[!is.na(x$prim_key),]
    
    has.baseline <- !is.na(x$perceivedevasionrate) & 
                    !is.na(x$perceivedauditrate) &
                    !is.na(x$perceivedpenaltyrate) &
                    !is.na(x$perceivedtaxrate) 
  
    
    x<- x[has.baseline,]
    
    
    tmp <- 0
    for(ii in names.hypo.data.fields){
      tmp <- tmp+ sum(!is.na(x[,ii])) 
    }
    tab[["have baseline values"]]<-  c(respondents =nrow(x),data.points= tmp)
    
    #tmp <- sum(!is.na(x$perceivedunderreportpenaltymuchhigher)& !is.na(x$perceivedunderreportpenaltyhigher) & 
     #            !is.na(x$perceivedunderreportaudithigher)& !is.na(x$perceivedunderreportauditmuchhigher) )

    tax.threshold <- quantile(x$perceivedtaxrate,prob=1,na.rm=T) #0.75
    x <- x[x$perceivedtaxrate <= tax.threshold,] 
    audit.threshold <- quantile(x$perceivedauditrate,prob=0.75,na.rm=T) #0.75
    x <- x[x$perceivedauditrate <=audit.threshold ,]
    penalty.threshold <- quantile(x$perceivedpenaltyrate,prob=1,na.rm=T) #0.75
    x <- x[x$perceivedpenaltyrate <= penalty.threshold,] 
    
    x<- x[!is.na(x$prim_key),]
    
    tmp <- 0
    for(ii in names.hypo.data.fields){
      tmp <- tmp+ sum(!is.na(x[,ii])) 
    }
    tmp.lab <- paste("have baseline audit rate below ",audit.threshold ,"%",sep="")
    tab[[tmp.lab]]<-  c(respondents =nrow(x),data.points= tmp)
    
    tmp <- sum(!is.na(x$perceivedunderreportpenaltymuchhigher)& !is.na(x$perceivedunderreportpenaltyhigher) & 
                 !is.na(x$perceivedunderreportaudithigher)& !is.na(x$perceivedunderreportauditmuchhigher) )
    
    
    # tmp  <- (x$perceivedevasionrate>x$perceivedunderreporttaxhigher) 
    # people that think evasion decreases with taxes
    # tmp  <- tmp & (x$perceivedevasionrate<=(1+grace.factor)*x$perceivedunderreporttaxhigher)
    
    if (grace.factor<0.01) {grace.factor <- 0.01}
    
    a <- round(x$perceivedevasionrate/(100*grace.factor),0)
    b <- round(x$perceivedunderreporttaxhigher/(100*grace.factor),0)
    tx.U  <- a<=b
    tx.U[is.na(tx.U)] <- F 
    
    a <- round(x$perceivedevasionrate/(100*grace.factor),0)
    b <- round(x$perceivedunderreporttaxlower/(100*grace.factor),0)
    tx.L <- a>=b
    tx.L[is.na(tx.L)] <- F 
    
    a <- round(x$perceivedevasionrate/(100*grace.factor),0)
    b <- round(x$perceivedunderreportaudithigher/(100*grace.factor),0)
    au.U  <- a>=b
    au.U[is.na(au.U)] <- F 
    
    a <- round(x$perceivedevasionrate/(100*grace.factor),0)
    b <- round(x$perceivedunderreportpenaltyhigher/(100*grace.factor),0)
    pe.U  <- a>=b
    pe.U[is.na(pe.U)] <- F
    
    a <- round(x$perceivedevasionrate/(100*grace.factor),0)
    b <- round(x$perceivedunderreportpenaltylower/(100*grace.factor),0)
    pe.L <- a<=b
    pe.L[is.na(pe.L)] <- F 
    
    
    x[!tx.U,c("perceivedunderreporttaxhigher","perceivedunderreporttaxmuchhigher")] <-NA 
    x[!tx.L,c("perceivedunderreporttaxlower","perceivedunderreporttaxmuchlower")] <-NA 
    
    x[!au.U,c("perceivedunderreportaudithigher","perceivedunderreportauditmuchhigher")] <-NA 
    
    x[!pe.U,c("perceivedunderreportpenaltyhigher","perceivedunderreportpenaltymuchhigher")] <-NA 
    x[!pe.L,c("perceivedunderreportpenaltylower","perceivedunderreportpenaltymuchlower")] <-NA 
    
    x<- x[!is.na(x$prim_key),]
    
    tmp <- 0
    for(ii in names.hypo.data.fields){
      tmp <- tmp+ sum(!is.na(x[,ii])) 
    }
    tab[["Pass the consistency sanity check"]]<-  c(respondents =nrow(x),data.points= tmp)
    
    
    tmp <-      !is.na(x$perceivedunderreportaudithigher)& !is.na(x$perceivedunderreportauditmuchhigher) 
    y <- x[tmp,]
    tmp <- 0
    for(ii in names.hypo.data.fields){
      tmp <- tmp+ sum(!is.na(y[,ii])) 
    }
    tab[["and answered the audit higher questions"]]<-  c(respondents =nrow(y),data.points= tmp)
    
    
    tmp <- !is.na(x$perceivedunderreportpenaltymuchhigher)& !is.na(x$perceivedunderreportpenaltyhigher) & 
                 !is.na(x$perceivedunderreportaudithigher)& !is.na(x$perceivedunderreportauditmuchhigher) 
    y <- x[tmp,]
    tmp <- 0
    for(ii in names.hypo.data.fields){
      tmp <- tmp+ sum(!is.na(y[,ii])) 
    }
    tab[["and answered both the audit and penalty higher questions"]]<-  c(respondents =nrow(y),data.points= tmp)
    
    tab <- do.call("rbind",tab)
    
    tab<- as.data.frame(cbind(rownames(tab),tab))
    colnames(tab)<- c("Filtering","number of respondents", "number of data points")

  }else{
    bl <- x$control%in%"baseline"
    tx <- x$control%in%"taxrate"
    au <- x$control%in%"auditrate"
    pe <- x$control%in%"penaltyrate"
    
    mhi <- grepl("muchhigher",x$question)
    mlo <- grepl("muchlower",x$question)
    
    hi <- grepl("higher",x$question) & !mhi
    lo <- grepl("lower",x$question) & !mlo
    
    y<-x$perceivedevasionrate
    
    con <- list()
    con[[1]] <- (y[bl]<=y[tx&hi] & y[tx&hi]<=y[tx&mhi])
    con[[2]] <- (y[bl]>=y[au&hi] & y[au&hi]>=y[au&mhi])
    con[[3]] <- (y[bl]>=y[pe&hi] & y[pe&hi]>=y[pe&mhi])
    
    con[[4]] <- (y[bl]>=y[tx&lo] & y[tx&lo]>=y[tx&mlo])  
    con[[5]] <-(y[bl]<=y[au&lo] & y[au&lo]<=y[au&mlo])
    con[[6]] <- (y[bl]<=y[pe&lo] & y[pe&lo]<=y[pe&mlo])
    
    ent<-list()
    ent[[1]] <- bl|(tx&hi)|(tx&mhi)
    ent[[2]] <- bl|(au&hi)|(au&mhi)
    ent[[3]] <- bl|(pe&hi)|(pe&mhi)  
    
    ent[[4]] <- bl|(tx&lo)|(tx&mlo)
    ent[[5]] <- bl|(au&lo)|(au&mlo)
    ent[[6]] <- bl|(pe&lo)|(pe&mlo)  
    
    
    for(i in 1:6){
      if(length(con[[i]])<1) {con[[i]]<-T}
      ent[[i]] <- con[[i]]& ent[[i]]
    }
    ent<-do.call("rbind",ent)
    ent<-as.logical(apply(ent,2,max))
    x <- x[ent,]
    
    if(nrow(x)<1) {x<-NULL}
  }
  
  df.out<- x[!is.na(x$prim_key),]
  return(list(df.out=df.out,tab=tab, df.ori=df.ori))
}