ALP.fill.out.missing.variables.for.regression<-function(xdf){
  
  
  xdf.numeric <- sapply(xdf,is.numeric) 
  indicator.NAs<-sapply(xdf[xdf.numeric],is.na)
  field.with.NAs<-sapply(as.data.frame(indicator.NAs),any)
  num.names.field.with.NAs <- names(field.with.NAs)[field.with.NAs]
  indicator.NAs <- as.data.frame(indicator.NAs[,field.with.NAs])
  colnames(indicator.NAs) <- names(field.with.NAs[field.with.NAs])
  
  if(any(field.with.NAs)){
    means<-colMeans(xdf[,xdf.numeric],na.rm=T)
    for(i in names(means)){
      is.missing <- is.na(xdf[,i])
      xdf[is.missing,i] <- means[i] 
    }
  }
  
  xdf.logical <-  sapply(xdf,is.logical)
  log.indicator.NAs<-sapply(xdf[xdf.logical],is.na)
  field.with.NAs<-sapply(as.data.frame(log.indicator.NAs),any)
  num.names.field.with.NAs <- names(field.with.NAs)[field.with.NAs]
  log.indicator.NAs <- as.data.frame(log.indicator.NAs[,field.with.NAs])
  colnames(log.indicator.NAs) <- names(field.with.NAs[field.with.NAs])
  
  if(any(field.with.NAs)){
    default <- FALSE
    for(i in colnames( log.indicator.NAs)){
      is.missing <- is.na(xdf[,i])
      xdf[is.missing,i] <- default
    }
  }
  
  cat.indicator.NAs<-sapply(xdf[,!xdf.numeric & !xdf.logical ],is.na)
  field.with.NAs<-sapply(as.data.frame(cat.indicator.NAs),any)
  cat.names.field.with.NAs <- names(field.with.NAs)[field.with.NAs]
  cat.indicator.NAs <- as.data.frame(cat.indicator.NAs[,field.with.NAs])
  colnames(cat.indicator.NAs) <- names(field.with.NAs[field.with.NAs])
  
  if(any(field.with.NAs)){
    for(i in cat.names.field.with.NAs){
      max.level<-which.is.max(table(xdf[,i]))
      max.level<-names(table(xdf[,i])[max.level])
      xdf[cat.indicator.NAs[,i],i] <- max.level
    }
  }
  
  cat.indicator.NAs<- as.data.frame(cat.indicator.NAs[,cat.names.field.with.NAs])
  names(cat.indicator.NAs) <- cat.names.field.with.NAs
  
  indicator.NAs <- cbind(indicator.NAs,log.indicator.NAs,cat.indicator.NAs)
  
  colnames(indicator.NAs) <- paste(colnames(indicator.NAs),"NA.Indicator",sep="__")
  indicator.NAs<- as.data.frame(indicator.NAs)
  indicator.NAs$prim_key <- xdf$prim_key
  
  xdf <- merge(xdf,indicator.NAs,by="prim_key")
  
  return(xdf)
}