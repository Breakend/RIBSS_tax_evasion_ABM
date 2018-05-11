get.ALP.avg.rate.tTaxes <-function(x,subset, show.dist.instead=F){
  ftt<- colMeans(x[subset ,-c(1:3)],na.rm=T)[1:6]
  freq.of.talk.taxes<-ftt/sum(ftt)
  sd.fft <- sd(x[,5],na.rm=T)/sum(ftt)
  
  ti0<-c(5,2,1,1/2,1/12,1/24)
  ti1 <-ti0- c(-2.5,diff(ti0)/2)
  ti2 <-ti0+ c(diff(ti0),0)/2
  
  
  ### Find Mode and [LB,UB] - giving the number 
  ### of times per year talking taxes 
  tab<-list()
  tab[["LB"]]<-round(1/as.numeric(freq.of.talk.taxes%*%(ti1)),2)
  tab[["Mean"]]<-round(1/as.numeric(freq.of.talk.taxes%*%(ti0)),2)
  tab[["UB"]]<-round(1/as.numeric(freq.of.talk.taxes%*%(ti2)),2)
  avg.rate.tTaxes<-do.call("c",tab) ## rate per year
  
  
  if(show.dist.instead){
    x<-x[subset,]
    x$n.alters.tTaxes[is.na(x$n.alters.tTaxes)]<-0
    y<- x[x$n.alters.tTaxes>0,]
    z<- y$n.alters.tTaxes
    y<- y[,-c(1:3)]
    y <- y[,1:6]/100
    
    ti0<-c(5,2,1,1/2,1/12,1/24)
    xx <- t(t(z*y))
    colSums(xx)
    
    xx<- melt(colSums(xx))
    avg.rate.tTaxes<- as.data.frame(100*t(xx)/sum(xx))
  }
  
  return(avg.rate.tTaxes)
}