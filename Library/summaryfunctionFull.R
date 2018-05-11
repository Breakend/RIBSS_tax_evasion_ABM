summaryfunctionFull<- function (x,nainclude=F,weights=NULL){
  require(Hmisc)
  
  if( is.numeric(x)!=TRUE) {
    stop("summaryfunctionFull: Supplied X is not numeric")
    }
  if( !is.null(weights) & (length(weights)!=length(x) | is.numeric(weights)!=TRUE)) {
    stop("summaryfunctionFull: Supplied weights is not numeric or mismatch length")
    }
  
  if(is.null(weights)){
    NAs <- sum(is.na(x))
    N <- length(x)- NAs
    x<- x[!is.na(x)]
    mysummary = data.frame(
      "Min." =as.numeric( min(x)),
      "1st Qu." = quantile(x)[2],
      "Median" = median(x),
      "Mean" = mean(x),
      "sd" = sd(x),
      "3rd Qu." = quantile(x)[4],
      "Max." = max(x),
      "NA's" = NAs,
      "N" = N,
      row.names=""
    )
  }else{
    #x<- SD.clean$perceivedauditrate
    NAs <- sum(is.na(x))
    N <- length(x)- NAs
    weights<- weights[!is.na(x)]
    x<- x[!is.na(x)]
    summary <- wtd.quantile(x, weights=weights, na.rm=TRUE)
    mean<-wtd.mean(x, weights=weights)
    sd<-sqrt(wtd.var(x, weights=weights))
    mysummary = data.frame(t(c(summary[1:3],mean,sd,summary[4:5],
      "NA's" = NAs,
      "N" = N
    )),
    row.names="")
  }
  names(mysummary) = c("Min.","1st Qu.","Median","Mean","sd","3rd Qu.","Max.","NA's","N")
  if(!nainclude){mysummary<-mysummary[!( names(mysummary)%in%"NA's")]}
  return( mysummary )
}