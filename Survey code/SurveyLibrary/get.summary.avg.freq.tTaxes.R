get.summary.avg.freq.tTaxes<-function(x){
  
  out.all<- list()
  for(j in rownames(avg.freq.tTaxes)){
  z<-x[j,2:7]
  y<- c(5,2,1,1/2,1/12,0)
  N<- 10^5
  out <- NULL
  for(i in 1:length(z)){
    t <- y[i]
    out<- c(out,rep(t,round(N*z[i],0)))
  }
  out.all[[j]] <- summaryfunctionFull(out)
 }

  out<- as.data.frame(do.call("rbind",out.all))
  out <- cbind(rownames(out),out)
  colnames(out)[1] <- "Respondent subsample"
  out<- out[,!(colnames(out)%in%"NA's")]
  
  return(out)
}