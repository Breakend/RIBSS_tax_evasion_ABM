correct.fit.c1.with.hypotheticals<- function(x,adjust=2,c2=0.70){
  
  tmp <- x$c1.guessed
  tmp2<- x$tax.rate.threshold.max
  tmp2<- tmp2[!is.na(tmp)]
  tmp <- tmp[!is.na(tmp)]
  puap.dens<- get.ALP.c1.density(tmp,adjust=adjust)
  lab <- paste("fitted.adjust.",as.character(adjust),sep="")
  tmp2 <- as.factor(tmp2)
  levels(tmp2) <- c("0-1%","1-2.5%","2.5-5%","5-10%","10-15%","15-20%","20-25%","25-30%",">30%")
  
  dat1<- data.frame(X= tmp, Y=as.factor(tmp2))
  fit1<- data.frame(x=puap.dens$dens$x,y=puap.dens$dens$y/sum(puap.dens$dens$y))
  
  
  p1<- ggplot(dat1) + 
    geom_histogram(aes(x=X, y = ..density.., fill=Y), 
                   binwidth = 5,color="cornflowerblue",color="black") + 
    #geom_density(color="red",size=2)+
    geom_line(data=fit1, aes(x=x,y=y),color="red",size=2 )+
    theme_bw() +
    guides(fill = guide_legend(title=""))+
    ylab("Density")+
    xlab("c1 value")+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=16,angle=0 ), 
          axis.text.y=element_text(size=16 ) ,
          title = element_text( size=18 ) ,
          strip.text=element_text( size=16 ) ,
          axis.title.x = element_text( size=18 ) ,
          axis.title.y = element_text( size=18 ),
          legend.text = element_text( size = 14))
  
  p2<-ggplot(dat1) + 
    geom_histogram(aes(x=X, y = ..count../(5*sum(..count..)), fill=Y), 
                   binwidth = 5,color="black") + 
    #geom_density(color="red",size=2)+
    geom_line(data=fit1, aes(x=x,y=y),color="red",size=2 )+
    theme_bw() +
    guides(fill = guide_legend(title=""))+
    ylab("")+
    xlab("")+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=16,angle=0 ), 
          axis.text.y=element_text(size=16 ) ,
          title = element_text( size=18 ) ,
          strip.text=element_text( size=16 ) ,
          axis.title.x = element_text( size=18 ) ,
          axis.title.y = element_text( size=18 ),
          legend.text = element_text( size = 14))
  
  r <- list(fit=fit1,
            plot=p1,
            means=puap.dens$means)
}