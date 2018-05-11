get.qP.lognormal.fit<-function(z){
  z<-z[z$control%in%c("baseline","auditrate","penaltyrate"),
       c("prim_key","control","question","perceivedtaxrate",
         "c1.guessed",
         "perceivedauditrate",
         "perceivedpenaltyrate","perceivedevasionrate")]
  z[,!(colnames(z)%in%(colnames(z)[1:3]))]<-z[,!(colnames(z)%in%(colnames(z)[1:3]))]/100
  
  z$qP <- z$perceivedauditrate*z$perceivedpenaltyrate
  z<- z[, c("prim_key","perceivedtaxrate","c1.guessed","qP","perceivedevasionrate")]
  colnames(z)[colnames(z)%in%"perceivedevasionrate"] <- "E"
  
#   
#   z$Delta.T <- pmax(z$perceivedtaxrate-z$c1.guessed,0)/
#     pmax(0.7-z$c1.guessed,0)
# 
#   z<- z[,c("qP","E","Delta.T")]
#   
#   yy <- z[z$E==0 & z$Delta.T>0,]
#   #z<- z[z$Delta.T>0,]
#   
#   z<- yy
#   z$E <- z$Delta.T
  
 # z$E<- 1-z$E
  
  df.split <- split(z,z$prim_key)
  
  #rec<- df.split[["9117483:1"]]
  
  r.list<- list()
  for(jj in names(df.split)){
    rec<- df.split[[jj]]
    x<- c(rec$E,0.99)
    y <- c(rec$qP,0.99)
    
    Y <- erf.inv(2*x - 1)
    X <- log(y)
    
    X[is.infinite(X)]<- -100
    Y[is.infinite(Y)]<- -100
    
    reg <- lm(Y~X,data.frame(X=X,Y=Y))
    
    tmp <- summary.lm(reg)$coefficients 
    
    c <- summary.lm(reg)$coefficients["(Intercept)","Estimate"]
    k <- summary.lm(reg)$coefficients["X","Estimate"]
    s <- k*sqrt(2)
    m <- exp(-c/k)
    
    r.list[[jj]]<- c(s=s,m=m)
  }
  
  r.list<- do.call("rbind",r.list)
  rm <- apply(r.list,1,FUN=function(x){any(is.na(x))})
  r.list<- r.list[!rm, ]
  
  r.list<- r.list[r.list[,2]<=2,]
  
  r<- rbind(s=c(summary(r.list[,1]),N=nrow(r.list)),
            m=c(summary(r.list[,2]),N=nrow(r.list)))
  
  return(r)
}