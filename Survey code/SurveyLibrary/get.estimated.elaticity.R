get.estimated.elasticity <- function(z,control,relative=F){
  
  rate.lb <-paste("perceived",control,sep="")
  
  if(control%in%"taxrate") relative<-T
  
  df <- z[z$control%in%c("baseline",control),
          c("prim_key","control","question",
            rate.lb,"perceivedevasionrate")]
  df.split <- split(df,df$prim_key)
  
  
  r.list<- list()
  for(jj in names(df.split)){
    x<- df.split[[jj]]
    
    if(!is.null(x)){
      if(length(x[,rate.lb])>2){
        
        # if(relative){
        #   i<-which.min(x[,rate.lb])
        #   X<- log(x[,rate.lb]-x[i,rate.lb])
        #   Y<-log(x$perceivedevasionrate-x$perceivedevasionrate[i])
        # }
        
        X <- log(x[,rate.lb])
        Y <- log(x$perceivedevasionrate)
        
        X[is.infinite(X)]<- -100
        Y[is.infinite(Y)]<- -100
        r <-lm(Y~X)
        if(relative){
          r.no.intercept <- lm(Y~X-1)
          r.list[[jj]]<-c(r$coefficients,X2=r.no.intercept$coefficients)
        }else{
          r.list[[jj]]<-r$coefficients
        }
      }}
  }
  
  r.list<- do.call("rbind",r.list)
  rm <- apply(r.list,1,FUN=function(x){any(is.na(x))})
  r.list<- r.list[!rm, ]
  
  #if(relative)  {r.list<- r.list[r.list[,2]>=0,]} ### select for positve elasticities. 
  #if(!relative) {r.list<- r.list[r.list[,2]<=0,]} ### select for negative elasticities. 
  if(relative){
  r <- rbind(intercept=c(summaryfunctionFull(r.list[,1])),
             slope=c(summaryfunctionFull(r.list[,2])),
             slope0=c(summaryfunctionFull(r.list[,3])))
  }else{
    r <- rbind(intercept=c(summaryfunctionFull(r.list[,1])),
               slope=c(summaryfunctionFull(r.list[,2]) ))
  }
  return(r)
}