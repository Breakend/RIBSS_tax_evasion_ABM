get.hypoteticals.perceivedevasionrate <- function(x,field.to.transform = "perceivedtaxrate", mul=c(1,1.5,2), 
                                                  fields = c("perceivedevasionrate",
                                                             "perceivedunderreporttaxhigher", 
                                                             "perceivedunderreporttaxmuchhigher"),
                                                  control ="taxrate",
                                                  decreasing = FALSE, bound.at.100=TRUE){
  
  #x<- df.xta
  x[,fields]<-as.data.frame(t(apply(x[,fields],1,FUN=function(z){
    tmp <-order(z, decreasing=decreasing)
    return(z[tmp])
  })))
  
  x$NEWperceivedevasionrate<- NA
  z<- list()
  for(i in 1:length(mul)){
    y<- x
    
    f <- fields[i]
    m <- mul[i]
    
    y[,field.to.transform]  <- m*y[,field.to.transform] 
    if(bound.at.100)  y[,field.to.transform]  <- pmin(100, y[,field.to.transform])
   
    y$NEWperceivedevasionrate <- y[,f] 
  
    y<- y[!is.na(y$NEWperceivedevasionrate),] ## may need to comment
   
    z[[i]] <- y[,c("prim_key",field.to.transform,"NEWperceivedevasionrate")]
    z[[i]]$question <- f
  }
  z<- do.call(rbind,z)
  names(z)[3]<- "perceivedevasionrate"
  z$control <- control
  z<- z[order(z$prim_key),]
  
  return(z)
}