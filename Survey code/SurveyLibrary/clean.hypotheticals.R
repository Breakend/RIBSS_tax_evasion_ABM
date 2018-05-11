clean.hypotheticals<-function(x, short.format = TRUE, grace.factor=0.05, allow.reversion= TRUE){
  
  #x <-SD.clean
  tab <- list(NULL)
  
  ########################################
  ###                                  ###
  ###    Get the data and relevant fields
  ###                                  ###
  ########################################
  
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
  
  x[x==""]<-NA
  
  df.ori<- x
  
  
  names.hypo.data.fields<- colnames(x)[!(colnames(x)%in%
                              c("prim_key","perceivedtaxrate",
                                "perceivedpenaltyrate",
                                "perceivedauditrate"))]
  
  
  ########################################
  ###                                  ###
  ###    Correct based on skip logic
  ###                                  ###
  ########################################
  
  
  tmp <- 0
  for(ii in names.hypo.data.fields){
    #print(paste(sum(!is.na(x[,ii])),"=# of obs in field", ii, sep=" "))
    #print(table(x[,ii]>=0,useNA="always"))
    tmp <- tmp+ sum(!is.na(x[,ii])) 
  }
  tab[["full data raw"]]<-  c(respondents =nrow(x),data.points= tmp)
  #print(tab[["full data raw"]])  ## AP: Full sample 1030  6460 (of 7210 possible)
  
  ### Correct the skip logic: In the survey if respondent gave 0 for perceivedunderreportpenaltyhigher
  ### s/he was not asked perceivedunderreportpenaltymuchhigher. Thus these are NA in the data,
  ### but these NA need to be changed to 0. Also invert the order if inconsistent
  
  tmp <- (x$perceivedunderreportpenaltyhigher==0) & 
    is.na(x$perceivedunderreportpenaltymuchhigher)
  tmp[is.na(tmp)] <- F 
  x$perceivedunderreportpenaltymuchhigher[tmp] <- 0
  
  ### Correct the skip logic 2:  In the survey if respondent gave 0 for 
  ### perceivedunderreportpenaltymuchlower
  ### s/he was not asked perceivedunderreportpenaltylower. Thus these are NA in the data, 
  ### but these NA need to be changed to 0.
  
  tmp <- (x$perceivedunderreportpenaltymuchlower==0) & 
    is.na(x$perceivedunderreportpenaltylower)
  tmp[is.na(tmp)] <- F 
  x$perceivedunderreportpenaltylower[tmp] <- 0
  
  
  ### Correct the skip logic 3:  In the survey if respondent gave 0 for perceivedunderreportaudithigher
  ### s/he was not asked perceivedunderreportauditmuchhigher. Thus these are NA in the data, 
  ### but these NA need to be changed to 0.
  
  tmp<- (x$perceivedevasionrate==0)
  tmp[is.na(tmp)] <- F 
  sum(tmp)
  x$perceivedunderreportaudithigher[tmp] <- 0
  
  
  tmp <- (x$perceivedunderreportaudithigher==0) & 
    is.na(x$perceivedunderreportauditmuchhigher)
  tmp[is.na(tmp)] <- F 
  x$perceivedunderreportauditmuchhigher[tmp] <- 0
  
  x[,-1] <- as.data.frame(apply(x[,-1],2,as.numeric))
  
  tmp <- 0
  for(ii in names.hypo.data.fields){
    #print(paste(sum(!is.na(x[,ii])),"=# of obs in field", ii, sep=" "))
    ##print(table(x[,ii]>=0,useNA="always"))
    tmp <- tmp+ sum(!is.na(x[,ii])) 
  }
  tab[["full data with inserted implied zeros"]]<-  c(respondents =nrow(x),data.points= tmp)
  #print( tab[["full data"]])  ### target AP  + Replacing NAs with 0 for logical skips 1030 & 6812
  
  
  tmp <- 0
  for(ii in names.hypo.data.fields){
    #print(paste(sum(!is.na(x[,ii])),"=# of obs in field", ii, sep=" "))
    ##print(table(x[,ii]>=0,useNA="always"))
    tmp <- tmp+ sum(!is.na(x[,ii])) 
  }
  tab[["full data"]]<-  c(respondents =nrow(x),data.points= tmp)
  #print( tab[["full data"]])  ### target AP  + Replacing NAs with 0 for logical skips 1030 & 6812
  
  
  ########################################
  ###                                  ###
  ###    Select those with baseline values
  ###                                  ###
  ########################################
  
  has.baseline <- !is.na(x$perceivedevasionrate) & 
    !is.na(x$perceivedauditrate) &
    !is.na(x$perceivedpenaltyrate) &
    !is.na(x$perceivedtaxrate) 
  
  
  x<- x[has.baseline,]
  
  
  tmp <- 0
  for(ii in names.hypo.data.fields){
    #print(paste(sum(!is.na(x[,ii])),"=# of obs in field", ii, sep=" "))
    tmp <- tmp+ sum(!is.na(x[,ii])) 
  }
  tab[["have baseline values"]]<-  c(respondents =nrow(x),data.points= tmp)
  #print( tab[["have baseline values"]])  
  
  
  ########################################
  ###                                  ###
  ###    Remove those with pereieved audit rate >30%
  ###                                  ###
  ########################################
  
  
  audit.threshold <- quantile(x$perceivedauditrate,prob=0.75,na.rm=T) #0.75
  x<- x[x$perceivedauditrate <=audit.threshold,]
  tax.threshold <- quantile(x$perceivedtaxrate,prob=1,na.rm=T) #0.75
  x <- x[x$perceivedtaxrate <= tax.threshold,] 
  #x <- x[x$perceivedauditrate <=audit.threshold,]  ### remove to get AP sample numbers 
  penalty.threshold <- quantile(x$perceivedpenaltyrate,prob=1,na.rm=T) #0.75
  x[x$perceivedpenaltyrate > penalty.threshold,c("perceivedunderreportpenaltyhigher",
                                                 "perceivedunderreportpenaltymuchhigher")] <- NA
  #x <- x[x$perceivedpenaltyrate <= penalty.threshold,] ### remove to get AP sample numbers 
  
  tmp <- 0
  for(ii in names.hypo.data.fields){
    tmp <- tmp+ sum(!is.na(x[,ii])) 
  }
  x<- x[!is.na(x$prim_key),]
  
  tmp.lab <- paste("have baseline audit rate below ",audit.threshold ,"%",sep="")
  tab[[tmp.lab]]<-  c(respondents = nrow(x),data.points= tmp)
  
  
  ########################################
  ###                                  ###
  ###    Remove or Revert Inconsistent responses
  ###                                  ###
  ########################################
  
  
  if(allow.reversion){
    
    #Invert perceivedunderreportpenaltyhigher
    a <- x$perceivedunderreportpenaltyhigher
    b <- x$perceivedunderreportpenaltymuchhigher
    need.to.invert <- (a < b)
    print("Invert perceivedunderreportpenaltyhigher ")
    print(table(need.to.invert, useNA="always"))
    need.to.invert[is.na(need.to.invert)] <-F
    x$perceivedunderreportpenaltymuchhigher[need.to.invert] <- a[need.to.invert]
    x$perceivedunderreportpenaltyhigher[need.to.invert] <- b[need.to.invert]
    
    #Invert perceivedunderreportpenaltymuchlower
    a <- x$perceivedunderreportpenaltylower
    b <- x$perceivedunderreportpenaltymuchlower
    need.to.invert <- (a > b)
    print("Invert perceivedunderreportpenaltymuchlower ")
    print(table(need.to.invert, useNA="always"))
    need.to.invert[is.na(need.to.invert)] <-F
    x$perceivedunderreportpenaltymuchlower[need.to.invert] <- a[need.to.invert]
    x$perceivedunderreportpenaltylower[need.to.invert] <- b[need.to.invert]
    
    #Invert perceivedunderreportaudithigher
    a <- x$perceivedunderreportaudithigher
    b <- x$perceivedunderreportauditmuchhigher
    need.to.invert <- (a < b)
    print("Invert perceivedunderreportaudithigher ")
    print(table(need.to.invert, useNA="always"))
    need.to.invert[is.na(need.to.invert)] <-F
    x$perceivedunderreportauditmuchhigher[need.to.invert] <- a[need.to.invert]
    x$perceivedunderreportaudithigher[need.to.invert] <- b[need.to.invert]
    
    #Invert perceivedunderreporttaxlower
    a <- x$perceivedunderreporttaxlower
    b <- x$perceivedunderreporttaxmuchlower
    need.to.invert <- (a < b)
    print("Invert perceivedunderreporttaxlower")
    print(table(need.to.invert, useNA="always"))
    need.to.invert[is.na(need.to.invert)] <-F
    x$perceivedunderreporttaxmuchlower[need.to.invert] <- a[need.to.invert]
    x$perceivedunderreporttaxlower[need.to.invert] <- b[need.to.invert]
    
    #Invert perceivedunderreporttaxhigher
    a <- x$perceivedunderreporttaxhigher
    b <- x$perceivedunderreporttaxmuchhigher
    need.to.invert <- (a > b)
    print("Invert perceivedunderreporttaxhigher")
    print(table(need.to.invert, useNA="always"))
    need.to.invert[is.na(need.to.invert)] <-F
    x$perceivedunderreporttaxmuchhigher[need.to.invert] <- a[need.to.invert]
    x$perceivedunderreporttaxhigher[need.to.invert] <- b[need.to.invert]
    
  }
  # }else{
  #   
  #   a <- x$perceivedunderreportpenaltyhigher
  #   b <- x$perceivedunderreportpenaltymuchhigher
  #   need.to.invert <- (a < b)
  #   print("No Inversion allowed: removing perceivedunderreportpenaltyhigher records that need it")
  #   print(table(need.to.invert, useNA="always"))
  #   need.to.invert[is.na(need.to.invert)] <-F
  #   x$perceivedunderreportpenaltymuchhigher[need.to.invert] <- NA
  #   x$perceivedunderreportpenaltyhigher[need.to.invert] <- NA
  #   
  #   a <- x$perceivedunderreportpenaltylower
  #   b <- x$perceivedunderreportpenaltymuchlower
  #   need.to.invert <- (a > b)
  #   print("No Inversion allowed: removing perceivedunderreportpenaltymuchlower records that need it")
  #   print(table(need.to.invert, useNA="always"))
  #   need.to.invert[is.na(need.to.invert)] <-F
  #   x$perceivedunderreportpenaltymuchlower[need.to.invert] <- NA
  #   x$perceivedunderreportpenaltylower[need.to.invert] <- NA
  #   
  #   a <- x$perceivedunderreportaudithigher
  #   b <- x$perceivedunderreportauditmuchhigher
  #   need.to.invert <- (a < b)
  #   print("No Inversion allowed: removing perceivedunderreportaudithigher records that need it")
  #   print(table(need.to.invert, useNA="always"))
  #   need.to.invert[is.na(need.to.invert)] <-F
  #   x$perceivedunderreportauditmuchhigher[need.to.invert] <- NA
  #   x$perceivedunderreportaudithigher[need.to.invert] <- NA
  #   
  #   a <- x$perceivedunderreporttaxlower
  #   b <- x$perceivedunderreporttaxmuchlower
  #   need.to.invert <- (a < b)
  #   print("No Inversion allowed: removing perceivedunderreporttaxlower & perceivedunderreporttaxmuchlower records that need it")
  #   print(table(need.to.invert, useNA="always"))
  #   need.to.invert[is.na(need.to.invert)] <-F
  #   x$perceivedunderreporttaxmuchlower[need.to.invert] <- NA
  #   x$perceivedunderreporttaxlower[need.to.invert] <- NA
  #   
  #   a <- x$perceivedunderreporttaxhigher
  #   b <- x$perceivedunderreporttaxmuchhigher
  #   need.to.invert <- (a > b)
  #   print("No Inversion allowed: removing perceivedunderreporttaxhigher & perceivedunderreporttaxmuchhigher records that need it")
  #   print(table(need.to.invert, useNA="always"))
  #   need.to.invert[is.na(need.to.invert)] <-F
  #   x$perceivedunderreporttaxmuchhigher[need.to.invert] <- NA
  #   x$perceivedunderreporttaxhigher[need.to.invert] <- NA
  #   
  #   tmp <- 0
  #   for(ii in names.hypo.data.fields){
  #     tmp <- tmp+ sum(!is.na(x[,ii])) 
  #   }
  #   tmp.lab <- paste("Remove inconsistent respondents in the scenarios",sep="")
  #   tab[[tmp.lab]]<-  c(respondents = nrow(x),data.points= tmp)
  # }
  
  
  ########################################
  ###                                  ###
  ###   Apply Consistency Sanity Check and  Grace to penalty and audit responses
  ###                                  ###
  ########################################
  
  if (grace.factor<0.01) {grace.factor <- 0.01}
  
  # Conduct consistency checks:
  # with higher audit rate, should see lower evasion, so these differences should be positive
  audit_blminushigher <- x$perceivedevasionrate - x$perceivedunderreportaudithigher
  audit_blminusmuchhigher <- x$perceivedevasionrate - x$perceivedunderreportauditmuchhigher
  audit_higherminusmuchhigher <- x$perceivedunderreportaudithigher - x$perceivedunderreportauditmuchhigher
  
  audit_incon_bl_h <- audit_blminushigher < -grace.factor*100 
  audit_incon_bl_h[is.na(audit_incon_bl_h)] <- F
  x$perceivedunderreportaudithigher[audit_incon_bl_h] <- NA
  
  audit_incon_bl_mh <-  (audit_blminusmuchhigher < -grace.factor*100) | 
    (audit_higherminusmuchhigher< -grace.factor*100)
  audit_incon_bl_mh[is.na(audit_incon_bl_mh)] <- F
  x$perceivedunderreportauditmuchhigher[audit_incon_bl_mh] <- NA
  
  # with lower penalty rate, should see higher evasion, so these differences should be negative
  penalty_blminuslower <- x$perceivedevasionrate - x$perceivedunderreportpenaltylower
  penalty_blminusmuchlower <- x$perceivedevasionrate - x$perceivedunderreportpenaltymuchlower
  penalty_lowerminusmuchlower <- x$perceivedunderreportpenaltylower - x$perceivedunderreportpenaltymuchlower
  
  penalty_incon_bl_l <- penalty_blminuslower > grace.factor*100
  penalty_incon_bl_l[is.na(penalty_incon_bl_l)] <- F
  x$perceivedunderreportpenaltylower[penalty_incon_bl_l] <- NA
  
  penalty_incon_bl_ml <- (penalty_blminusmuchlower > grace.factor*100) | 
    (penalty_lowerminusmuchlower> grace.factor*100 )
  penalty_incon_bl_ml[is.na(penalty_incon_bl_ml)] <- F
  x$perceivedunderreportpenaltymuchlower[penalty_incon_bl_ml] <- NA
  
  # with higher penalty rate, should see lower  evasion, so these differences should be positive
  penalty_blminushigher <- x$perceivedevasionrate - x$perceivedunderreportpenaltyhigher
  penalty_blminusmuchhigher <- x$perceivedevasionrate - x$perceivedunderreportpenaltymuchhigher
  penalty_higherminusmuchhigher <- x$perceivedunderreportpenaltyhigher - x$perceivedunderreportpenaltymuchhigher
  
  penalty_incon_bl_h <- penalty_blminushigher < -grace.factor*100
  penalty_incon_bl_h[is.na(penalty_incon_bl_h)] <- F
  x$perceivedunderreportpenaltyhigher[penalty_incon_bl_h] <- NA
  
  penalty_incon_bl_mh <-  (penalty_blminusmuchhigher < -grace.factor*100) |
    (penalty_higherminusmuchhigher < -grace.factor*100)
  penalty_incon_bl_mh[is.na(penalty_incon_bl_mh)] <- F
  x$perceivedunderreportpenaltymuchhigher[penalty_incon_bl_mh] <- NA
  
  
  
  x<- x[!is.na(x$prim_key),]
  
  tmp <- 0
  for(ii in names.hypo.data.fields){
    tmp <- tmp+ sum(!is.na(x[,ii])) 
  }
  tab[["Pass the deterrence consistency sanity check"]]<-  c(respondents =nrow(x),data.points= tmp)
  
  
  ########################################
  ###                                  ###
  ###   Apply Consistency Sanity Check and  Grace to tax hypo responses
  ###                                  ###
  ########################################
  
  
  tmp <- 0
  for(ii in names.hypo.data.fields[1:5]){
    tmp <- tmp+ sum(!is.na(x[,ii])) 
  }
  #print(paste("tax hypo respondents= ", nrow(x), " observations = ", tmp ))
  
  sub.tax<-rowSums(x[,names.hypo.data.fields[1:5]],na.rm=T)>0
  tmp <- 0
  for(ii in names.hypo.data.fields[1:5]){
    tmp <- tmp+ sum(!is.na(x[sub.tax,ii])) 
  }
  #print(paste("tax hypo respondents with evasions rates >0 = ", sum(sub.tax), " observations = ", tmp ))
  
  
  
  
  #with lower tax rate, should see lower evasion, so these differences should be positive
  tax_blminuslower <- x$perceivedevasionrate - x$perceivedunderreporttaxlower
  tax_blminusmuchlower <- x$perceivedevasionrate - x$perceivedunderreporttaxmuchlower
  tax_lowerminusmuchlower <- x$perceivedunderreporttaxlower - x$perceivedunderreporttaxmuchlower
  
  tax_incon_bl_l <- tax_blminuslower < -grace.factor*100
  tax_incon_bl_l[is.na(tax_incon_bl_l)] <- F
  x$perceivedunderreporttaxlower[tax_incon_bl_l] <- NA
  
  tax_incon_bl_ml <- (tax_blminusmuchlower < -grace.factor*100) |
    (tax_lowerminusmuchlower < -grace.factor*100)
  tax_incon_bl_ml[is.na(tax_incon_bl_ml)] <- F
  x$perceivedunderreporttaxmuchlower[tax_incon_bl_ml] <- NA
  
  #with higher tax rate, should see higher evasion, so these differences should be negative
  tax_blminushigher <- x$perceivedevasionrate - x$perceivedunderreporttaxhigher
  tax_blminusmuchhigher <- x$perceivedevasionrate - x$perceivedunderreporttaxmuchhigher
  tax_higherminusmuchhigher <- x$perceivedunderreporttaxhigher - x$perceivedunderreporttaxmuchhigher
  
  tax_incon_bl_h <- (tax_blminushigher > grace.factor*100)
  tax_incon_bl_h[is.na(tax_incon_bl_h)] <- F
  x$perceivedunderreporttaxhigher[tax_incon_bl_h] <- NA
  
  tax_incon_bl_mh <-  (tax_blminusmuchhigher > grace.factor*100) |
    (tax_higherminusmuchhigher > grace.factor*100)
  tax_incon_bl_mh[is.na(tax_incon_bl_mh)] <- F
  x$perceivedunderreporttaxmuchhigher[tax_incon_bl_mh] <- NA
  
  
  sub.tax<-rowSums(x[,names.hypo.data.fields[1:5]],na.rm=T)>0
  tmp <- 0
  for(ii in names.hypo.data.fields[1:5]){
    tmp <- tmp+ sum(!is.na(x[sub.tax,ii])) 
  }
  #print(paste("tax hypo respondents with evasions rates >0 passing consistency check  = ", sum(sub.tax), " observations = ", tmp ))
  
  
  tmp <- 0
  for(ii in names.hypo.data.fields){
    tmp <- tmp+ sum(!is.na(x[,ii])) 
  }
  tab[["Pass the tax consistency sanity check"]]<-  c(respondents =nrow(x),data.points= tmp)
  
  
  
  
  tmp <-  !is.na(x$perceivedunderreportaudithigher)& !is.na(x$perceivedunderreportauditmuchhigher) 
  y <- x[tmp,]
  tmp <- 0
  for(ii in names.hypo.data.fields){
    tmp <- tmp+ sum(!is.na(y[,ii])) 
  }
  tab[["and answered the audit higher and much higher questions"]]<-  c(respondents =nrow(y),data.points= tmp)
  
  
  tmp <- !is.na(x$perceivedunderreportpenaltymuchhigher)& !is.na(x$perceivedunderreportpenaltyhigher) & 
    !is.na(x$perceivedunderreportaudithigher)& !is.na(x$perceivedunderreportauditmuchhigher) 
  y <- x[tmp,]
  tmp <- 0
  for(ii in names.hypo.data.fields){
    tmp <- tmp+ sum(!is.na(y[,ii])) 
  }
  tab[["and answered both the audit and penalty higher and much higher questions"]]<-  c(respondents =nrow(y),data.points= tmp)
  
  
  ########################################
  ###                                  ###
  ###   Print table
  ###                                  ###
  ########################################
  
  tab <- do.call("rbind",tab)
  
  tab<- as.data.frame(cbind(rownames(tab),tab))
  colnames(tab)<- c("Filtering","number of respondents", 
                    "number of data points")
  
  #print(tab)
  
  df.out<- x[!is.na(x$prim_key),]
  return(list(df.out=df.out,tab=tab, df.ori=df.ori))
}