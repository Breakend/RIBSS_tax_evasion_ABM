do.ALP.c1.analysis<-function(puap,c2=0.7){
  
  require(plyr)
  
  colnames(puap) <- c("1","2.5","5","10","15","20","25","30")
  
  filter.off.NA<-apply(puap,1, FUN=function(x){all(!is.na(x))})
  puap<- puap[filter.off.NA,]
  
  non.problematic.puap <-apply(puap,1,test.monotonic.increasing.sequence)
  problematic.puap<- !non.problematic.puap
  table(non.problematic.puap)
  
  easily.corrected<- apply(puap[problematic.puap,],1,test.monotonic.decreasing.sequence)
  
  ### correct the easily.corrected
  tmp <- puap[problematic.puap,]
  tmp <- tmp[easily.corrected,]
  tmp <- 3-tmp
  puap[rownames(tmp),] <- tmp
  
  ### remove the not.easily.corrected
  tmp <- puap[problematic.puap,]
  tmp <- tmp[!easily.corrected,]
  puap <- puap[!(rownames(puap)%in%rownames(tmp)),]
  
  puap<- puap-1
  
  tax.rates.puap<-as.numeric(colnames(puap))
  position.of.max.entry<-apply(puap,1,FUN=function(x){
    l <- length(x)
    p<-as.numeric(which.max(x))
    q<-length(tax.rates.puap[as.logical(x)])>0
    if(q){r<-p}else{r<- l+1}
    return(r)
  }) 
  
  those.in.the.range <- (position.of.max.entry<=ncol(puap))
  
  puap$position.of.max.entry<- ncol(puap)+1
  puap$position.of.max.entry[those.in.the.range] <- position.of.max.entry[those.in.the.range]
  
  tax.rates.puap<- c(tax.rates.puap,100)
  puap$tax.rate.threshold.max<-tax.rates.puap[puap$position.of.max.entry]
  puap$tax.rate.threshold.min<-tax.rates.puap[pmax(puap$position.of.max.entry-1,1)]
  tmp <- (puap$tax.rate.threshold.min==puap$tax.rate.threshold.max)
  puap$tax.rate.threshold.min[tmp] <- 0
  
  puap.range<- puap[,c("tax.rate.threshold.min","tax.rate.threshold.max")]
  puap.range$c1.guessed <- puap.range$tax.rate.threshold.max
  
  tmp <- names(table(puap.range[,2]))
  #tmp[tmp%in%as.character(100*c2)]<- "100" 
  tmp <-paste(names(table(puap.range[,1])),tmp,sep="-")
  tab<-table(puap.range[,2])
  names(tab) <- tmp
  c1.counts.table <- t(tab)
  
  
  tmp<-puap.range$tax.rate.threshold.max<100*c2
  
  #beta.guess <-  rbeta(sum(!tmp),1,1.5)
  ll.range <- c(1:100)/100
  beta.guess <- dbeta(ll.range,1,1.8)
  beta.guess <- round(sum(!tmp)*beta.guess/100,0)
  if(sum(beta.guess)<sum(!tmp)){beta.guess[1:(sum(!tmp)-sum(beta.guess))]<-
    beta.guess[1:(sum(!tmp)-sum(beta.guess))]+1 }
  
  dat <- data.frame(value <- ll.range, times = beta.guess)
  beta.guess <- ddply(dat,.(value),function(x){data.frame(value = rep(x$value,times = x$times))})
  

  x.shift <- (30+25)/2
  puap.range[!tmp,3]<- (100*c2-x.shift)*beta.guess+x.shift
  
  puap.range[tmp,3] <- (puap.range[tmp,1]+puap.range[tmp,2])/2
  
  puap.range[,4] <-   puap.range[,3]
  puap.range[!tmp,4] <-  puap.range[!tmp,2]
  
  mean.puap.range<- list(NULL)
  mean.puap.range[["Raw data"]] <- c(mean=mean(puap.range[,4]), 
                                     mean.under30=mean(puap.range[tmp,4]), 
                                     mean.over30 =  mean(puap.range[!tmp,4]), N = nrow(puap.range) )
  
  mean.puap.range[["Redistributed data"]]<- c(mean=mean(puap.range[,3]),
                                          mean.under30=mean(puap.range[tmp,4]),
                                          mean.over30 = mean(puap.range[!tmp,3]), N = nrow(puap.range))

  
  adjust<- 2
  puap.dens<- get.ALP.c1.density(puap.range[,3],adjust=adjust)
  lab <- paste("Fit to the Redistributed data with adjustment factor=",as.character(adjust),sep="")
  mean.puap.range[[lab]]<- puap.dens$means
  mean.puap.range<- do.call("rbind", mean.puap.range)
  mean.puap.range<-round(mean.puap.range,2)
  
  dat1<- data.frame(X= puap.range[,4],X.fit=puap.range[,3])
  fit1<- data.frame(x=puap.dens$dens$x,y=puap.dens$dens$y/sum(puap.dens$dens$y))
  
 p1<- ggplot(dat1) + 
    geom_histogram(aes(x=X.fit, y = ..density..), 
                   binwidth = 5, fill="darkgreen",color="black",alpha=0.5) + 
    geom_histogram(aes(x=X, y = ..density..), 
                   binwidth = 5, fill="cornflowerblue",color="black") + 
    #geom_density(color="red",size=2)+
    geom_line(data=fit1, aes(x=x,y=y),color="red",size=2 )+
    theme_bw() +
    guides(fill = guide_legend(title=""))+
    ylab("Density")+
    xlab(expression(paste(c[1], " value",sep=" ")))+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=16,angle=0 ), 
          axis.text.y=element_text(size=16 ) ,
          title = element_text( size=18 ) ,
          strip.text=element_text( size=16 ) ,
          axis.title.x = element_text( size=18 ) ,
          axis.title.y = element_text( size=18 ),
          legend.text = element_text( size = 14))+
    geom_vline(xintercept = 30,  linetype="dotted") +
    geom_vline(xintercept = c2*100,  linetype="dotted") +
    annotate("text", label = "Redistribute those \n in the (30,100]", 
             x=50,y=0.03, size = 10, colour = "darkgreen",angle = 0)
  
  colnames(mean.puap.range) <-c("Mean","Mean under 30%", "Mean over 30%", "N")
  puap.range<- puap.range[,1:3]
  r <- list(fit=fit1,
            c1.counts.table=c1.counts.table,
            puap.range=puap.range,
            mean.puap.range=mean.puap.range,
            plot=p1)
  
  return(r)
}