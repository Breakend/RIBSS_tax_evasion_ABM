correct.c1.with.hypotheticals<- function(df,ET.slope =3){
  
  df <- df[df$control%in%c("baseline","taxrate"),
                c("prim_key","control","question",
                  "perceivedtaxrate","perceivedevasionrate",
                  "tax.rate.threshold.min","tax.rate.threshold.max",
                  "c1.guessed")]
  
  check1 <- unique(df[,c("prim_key","c1.guessed")])
  
  df.split <- split(df,df$prim_key)
  
  df.split <-lapply(df.split,FUN=function(x){
    er.min<-which.min(x$perceivedevasionrate[x$perceivedevasionrate>0])
    er.min<- x$perceivedevasionrate[x$perceivedevasionrate>0][er.min]
    i<-match(er.min,x$perceivedevasionrate)
    a<-x$perceivedtaxrate[i]
    b<-unique(x$c1.guessed)
    c1.tilde.values <- 1000#max(0,x$perceivedtaxrate[i]-x$perceivedevasionrate[i]/ET.slope)
    x$c1.guessed<- min(a,b,c1.tilde.values)
    return(x)
  })
  
  df <- do.call("rbind",df.split)
  check2 <- unique(df[,c("prim_key","c1.guessed")])
  
  count<-(check1$c1.guessed==check2$c1.guessed)
  print(paste("changes =", sum(!count,na.rm = T)/length(count), sep=" "))
  
  return(df)
  
}
