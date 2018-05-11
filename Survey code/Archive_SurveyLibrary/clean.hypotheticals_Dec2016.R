clean.hypotheticals_Dec2016<-function(x, short.format = TRUE, grace.factor=0.05, allow.reversion= TRUE){
  
  #x <-SD.clean
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
    if(allow.reversion){
      print("Invert perceivedunderreportpenaltyhigher ")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreportpenaltymuchhigher[need.to.invert] <- a[need.to.invert]
      x$perceivedunderreportpenaltyhigher[need.to.invert] <- b[need.to.invert]
    }else{
      print("No Inversion allowed: removing perceivedunderreportpenaltyhigher records that need it")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreportpenaltymuchhigher[need.to.invert] <- NA
      x$perceivedunderreportpenaltyhigher[need.to.invert] <- NA
    }
    
    ### Correct the skip logic 2:  In the survey if respondent gave 0 for 
    ### perceivedunderreportpenaltymuchlower
    ### s/he was not asked perceivedunderreportpenaltylower. Thus these are NA in the data, 
    ### but these NA need to be changed to 0.
    
    tmp <- (x$perceivedunderreportpenaltymuchlower==0) & is.na(x$perceivedunderreportpenaltylower)
    x$perceivedunderreportpenaltylower[tmp] <- 0
    
    a <- x$perceivedunderreportpenaltylower
    b <- x$perceivedunderreportpenaltymuchlower
    need.to.invert <- (a > b)
    if(allow.reversion){
      print("Invert perceivedunderreportpenaltymuchlower ")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreportpenaltymuchlower[need.to.invert] <- a[need.to.invert]
      x$perceivedunderreportpenaltylower[need.to.invert] <- b[need.to.invert]
    }else{
      print("No Inversion allowed: removing perceivedunderreportpenaltymuchlower records that need it")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreportpenaltymuchlower[need.to.invert] <- NA
      x$perceivedunderreportpenaltylower[need.to.invert] <- NA
    }
     
    
    ### Correct the skip logic 3:  In the survey if respondent gave 0 for perceivedunderreportaudithigher
    ### s/he was not asked perceivedunderreportauditmuchhigher. Thus these are NA in the data, 
    ### but these NA need to be changed to 0.
    
    tmp <- (x$perceivedunderreportaudithigher==0) & is.na(x$perceivedunderreportauditmuchhigher)
    x$perceivedunderreportauditmuchhigher[tmp] <- 0
    
    a <- x$perceivedunderreportaudithigher
    b <- x$perceivedunderreportauditmuchhigher
    need.to.invert <- (a < b)
    if(allow.reversion){
      print("Invert perceivedunderreportaudithigher ")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreportauditmuchhigher[need.to.invert] <- a[need.to.invert]
      x$perceivedunderreportaudithigher[need.to.invert] <- b[need.to.invert]
    }else{
      print("No Inversion allowed: removing perceivedunderreportaudithigher records that need it")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreportauditmuchhigher[need.to.invert] <- NA
      x$perceivedunderreportaudithigher[need.to.invert] <- NA
    }
    
    
    ### Correct the percievedtaxlower
    ### comment this section out for AP numbers in taxes. Difference in records in only 6 
  
    a <- x$perceivedunderreporttaxlower
    b <- x$perceivedunderreporttaxmuchlower
    need.to.invert <- (a < b)
    if(allow.reversion){
      print("Invert perceivedunderreporttaxlower")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreporttaxmuchlower[need.to.invert] <- a[need.to.invert]
      x$perceivedunderreporttaxlower[need.to.invert] <- b[need.to.invert]
    }else{
      print("No Inversion allowed: removing perceivedunderreporttaxlower & perceivedunderreporttaxmuchlower records that need it")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreporttaxmuchlower[need.to.invert] <- NA
      x$perceivedunderreporttaxlower[need.to.invert] <- NA
    }
    
    ### Correct the percievedtaxhigher
    ### comment this section out for AP numbers in taxes. Difference in records in only 6
    
    a <- x$perceivedunderreporttaxhigher
    b <- x$perceivedunderreporttaxmuchhigher
    need.to.invert <- (a > b)
    if(allow.reversion){
      print("Invert perceivedunderreporttaxhigher")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreporttaxmuchhigher[need.to.invert] <- a[need.to.invert]
      x$perceivedunderreporttaxhigher[need.to.invert] <- b[need.to.invert]
    }else{
      print("No Inversion allowed: removing perceivedunderreporttaxhigher & perceivedunderreporttaxmuchhigher records that need it")
      print(table(need.to.invert, useNA="always"))
      need.to.invert[is.na(need.to.invert)] <-F
      x$perceivedunderreporttaxmuchhigher[need.to.invert] <- NA
      x$perceivedunderreporttaxhigher[need.to.invert] <- NA
    }
    
    
    
    
    x[,-1] <- as.data.frame(apply(x[,-1],2,as.numeric))
    
    names.hypo.data.fields<- colnames(x)[!(colnames(x)%in%c("prim_key", 
                                              "perceivedtaxrate", 
                                              "perceivedpenaltyrate",
                                              "perceivedauditrate"))]
    
    tmp <- 0
    for(ii in names.hypo.data.fields){
      print(paste(sum(!is.na(x[,ii])),"=# of obs in field", ii, sep=" "))
      #print(table(x[,ii]>=0,useNA="always"))
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
      print(paste(sum(!is.na(x[,ii])),"=# of obs in field", ii, sep=" "))
      tmp <- tmp+ sum(!is.na(x[,ii])) 
    }
    tab[["have baseline values"]]<-  c(respondents =nrow(x),data.points= tmp)
    
    #tmp <- sum(!is.na(x$perceivedunderreportpenaltymuchhigher)& !is.na(x$perceivedunderreportpenaltyhigher) & 
     #            !is.na(x$perceivedunderreportaudithigher)& !is.na(x$perceivedunderreportauditmuchhigher) )

    tax.threshold <- quantile(x$perceivedtaxrate,prob=1,na.rm=T) #0.75
    x <- x[x$perceivedtaxrate <= tax.threshold,] 
    audit.threshold <- quantile(x$perceivedauditrate,prob=0.75,na.rm=T) #0.75
    x[x$perceivedauditrate >audit.threshold, c("perceivedunderreportaudithigher",
                                                "perceivedunderreportauditmuchhigher")] <- NA
    #x <- x[x$perceivedauditrate <=audit.threshold,]  ### remove to get AP sample numbers 
    penalty.threshold <- quantile(x$perceivedpenaltyrate,prob=1,na.rm=T) #0.75
    x[x$perceivedpenaltyrate > penalty.threshold,c("perceivedunderreportpenaltyhigher",
                                                    "perceivedunderreportpenaltymuchhigher")] <- NA
    #x <- x[x$perceivedpenaltyrate <= penalty.threshold,] ### remove to get AP sample numbers 
    
    x<- x[!is.na(x$prim_key),]
    
    
    tmp <- 0
    for(ii in names.hypo.data.fields[1:5]){
      tmp <- tmp+ sum(!is.na(x[,ii])) 
    }
    print(paste("tax hypo respondents= ", nrow(x), " observations = ", tmp ))
    
    sub.tax<-rowSums(x[,names.hypo.data.fields[1:5]],na.rm=T)>0
    tmp <- 0
    for(ii in names.hypo.data.fields[1:5]){
      tmp <- tmp+ sum(!is.na(x[sub.tax,ii])) 
    }
    print(paste("tax hypo respondents with evasions rates >0 = ", sum(sub.tax), " observations = ", tmp ))

    
    tmp <- 0
    for(ii in names.hypo.data.fields){
      tmp <- tmp+ sum(!is.na(x[,ii])) 
    }
    tmp.lab <- paste("have baseline audit rate below ",audit.threshold ,"%",sep="")
    tab[[tmp.lab]]<-  c(respondents = sum(x$perceivedauditrate <=audit.threshold),data.points= tmp)
    
    tmp <- sum(!is.na(x$perceivedunderreportpenaltymuchhigher)& !is.na(x$perceivedunderreportpenaltyhigher) & 
                 !is.na(x$perceivedunderreportaudithigher)& !is.na(x$perceivedunderreportauditmuchhigher) )
    
    
    # tmp  <- (x$perceivedevasionrate>x$perceivedunderreporttaxhigher) 
    # people that think evasion decreases with taxes
    # tmp  <- tmp & (x$perceivedevasionrate<=(1+grace.factor)*x$perceivedunderreporttaxhigher)
    
    if (grace.factor<0.01) {grace.factor <- 0.01}
    
    
    # with lower tax rate, should see lower evasion, so these differences should be positive
    tax_blminuslower <- x$perceivedevasionrate - x$perceivedunderreporttaxlower
    tax_blminusmuchlower <- x$perceivedevasionrate - x$perceivedunderreporttaxmuchlower
    tax_lowerminusmuchlower <- x$perceivedunderreporttaxlower - x$perceivedunderreporttaxmuchlower
    
    # with higher tax rate, should see higher evasion, so these differences should be negative
    tax_blminushigher <- x$perceivedevasionrate - x$perceivedunderreporttaxhigher
    tax_blminusmuchhigher <- x$perceivedevasionrate - x$perceivedunderreporttaxmuchhigher
    tax_higherminusmuchhigher <- x$perceivedunderreporttaxhigher - x$perceivedunderreporttaxmuchhigher
    
    # create flags for inconsistent values, taking into account 5% grace parameter
    
    tax_incon_bl_l <- tax_blminuslower < -grace.factor*100
    tax_incon_bl_l[is.na(tax_incon_bl_l)] <- F
    x$perceivedunderreporttaxlower[tax_incon_bl_l] <- NA
    
    tax_incon_bl_ml <- tax_blminusmuchlower < -grace.factor*100
    tax_incon_bl_ml[is.na(tax_incon_bl_ml)] <- F
    x$perceivedunderreporttaxmuchlower[tax_incon_bl_ml] <- NA
    
    tax_incon_bl_h <- tax_blminushigher > grace.factor*100
    tax_incon_bl_h[is.na(tax_incon_bl_h)] <- F
    x$perceivedunderreporttaxhigher[tax_incon_bl_h] <- NA
    
    tax_incon_bl_mh <-  tax_blminusmuchhigher > grace.factor*100
    tax_incon_bl_mh[is.na(tax_incon_bl_mh)] <- F
    x$perceivedunderreporttaxmuchhigher[tax_incon_bl_mh] <- NA
  
    
    sub.tax<-rowSums(x[,names.hypo.data.fields[1:5]],na.rm=T)>0
    tmp <- 0
    for(ii in names.hypo.data.fields[1:5]){
      tmp <- tmp+ sum(!is.na(x[sub.tax,ii])) 
    }
    print(paste("tax hypo respondents with evasions rates >0 passing consistency check  = ", sum(sub.tax), " observations = ", tmp ))
    
    
    # a <- round(x$perceivedevasionrate/(100*grace.factor),0)
    # b <- round(x$perceivedunderreporttaxhigher/(100*grace.factor),0)
    # tx.U  <- a<=b
    # tx.U[is.na(tx.U)] <- F 
    # 
    # a <- round(x$perceivedevasionrate/(100*grace.factor),0)
    # b <- round(x$perceivedunderreporttaxlower/(100*grace.factor),0)
    # tx.L <- a>=b
    # tx.L[is.na(tx.L)] <- F 
    # 
    # x[!tx.U,c("perceivedunderreporttaxhigher","perceivedunderreporttaxmuchhigher")] <-NA 
    # x[!tx.L,c("perceivedunderreporttaxlower","perceivedunderreporttaxmuchlower")] <-NA 
    # 
    # sub.tax<-rowSums(x[,names.hypo.data.fields[1:5]],na.rm=T)>0
    # tmp <- 0
    # for(ii in names.hypo.data.fields[1:5]){
    #   tmp <- tmp+ sum(!is.na(x[sub.tax,ii])) 
    # }
    # print(paste("tax hypo respondents with evasions rates >0 = ", sum(sub.tax), " observations = ", tmp ))
    # 
    
    
    
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