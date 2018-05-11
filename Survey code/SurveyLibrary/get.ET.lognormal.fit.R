get.ET.lognormal.fit<-function(z,c2=0.7){
  options(warn=0)
  #z<- df.hypo
  z<-z[z$control%in%c("baseline","taxrate"),
       c("prim_key","control","question","perceivedtaxrate","perceivedevasionrate")]
  
  z <- z[z$perceivedtaxrate>0,]
 
  colnames(z)[colnames(z)%in%"perceivedevasionrate"] <- "E"
  colnames(z)[colnames(z)%in%"perceivedtaxrate"] <- "T"
  
  summaryfunctionFull(z$T) 
  summaryfunctionFull(z$E) 
  
  z[,!(colnames(z)%in%(colnames(z)[1:3]))]<-z[,!(colnames(z)%in%(colnames(z)[1:3]))]/100
  
  filter0.num.rec <-length(unique(z$prim_key))
  
  df.split <- split(z,z$prim_key)
  
  
  r.list<- list()
  for(jj in names(df.split)){
    rec<- df.split[[jj]]
    
    rec <- rec[rec$T<c2,]
    rec$E[rec$E>0.95] <- 0.95
    
    if(nrow(rec)>1){ 
      
      x <- c(rec$T,c2)
      y <- c(rec$E,0.975)
      
     #x <- sort(c(rec$T,c2))
     #y <- sort(c(rec$E,0.99))
      
      X <- log(x)
      Y <- erf.inv(2*y - 1)
      
      #X[is.infinite(X)]<- sign(X[is.infinite(X)])*(1*10^2)
      #Y[is.infinite(Y)]<- sign(Y[is.infinite(Y)])*(1*10^2)
      
      X[is.infinite(X)]<- log(0.001)
      #Y[is.infinite(X)]<- erf.inv(2*0.001-1)
      Y[is.infinite(Y)]<- erf.inv(2*0.001-1)
      
      reg <- lm(Y~X,data.frame(X=X,Y=Y))
      
      tmp <- summary.lm(reg)$coefficients 
      
      c <- summary.lm(reg)$coefficients["(Intercept)","Estimate"]
      k <- summary.lm(reg)$coefficients["X","Estimate"]
      s <- k*sqrt(2)
      m <- exp(-c/k)
      g <- s/(m*sqrt(2*pi))
      
      r.list[[jj]]<- c(s=s,m=m, k=k, c=c, g=g)
    }else{
      r.list[[jj]]<-NULL
    }
  }
  
  r.list<- do.call("rbind",r.list)
  
  rm <- apply(r.list,1,FUN=function(x){any(is.na(x))})
  r.list<- r.list[!rm, ]
  
  filter2.num.rec <-nrow(r.list)
  
  print(paste("number of records at differenr stages of this analyses", filter0.num.rec ,
              filter2.num.rec, sep= " "))
  
  r.list<- as.data.frame(r.list) ##[r.list[,2]<=2,]
  options(warn=1)
  return(r.list)
}