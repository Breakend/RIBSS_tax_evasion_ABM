do.ALP.per.rate.analysis<- function(per.rate,lab=NULL,scale.up=10, 
                                    n.bins=10, xmax=100,text.size=22){
  
  require(scales)
  x<-per.rate
  x<-x[!is.na(x)]
  
  dens.x<- density(x, adjust=2,from=0,to=100,n=100)
  summary.x<-round(summary(x),2)
  fit.x<- data.frame(x=dens.x$x,y=dens.x$y/sum(dens.x$y))
  
  dat<- data.frame(X= x)
  plot.x<- ggplot(dat, aes(x = X)) + 
    geom_histogram(aes(y = ..density..), 
                   bins=n.bins, fill="cornflowerblue",color="black") + 
    #geom_density(color="red",size=2)+
    geom_line(data=fit.x, aes(x=x,y=scale.up*y/3),color="red",size=2 )+
    theme_bw() +
    guides(fill = guide_legend(title=""))+
    ylab("Density")+
    xlab(lab)+
    xlim(0,xmax)+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=text.size,angle=0 ), 
          axis.text.y=element_text(size=text.size ) ,
          title = element_text( size=text.size ) ,
          strip.text=element_text( size=text.size ) ,
          axis.title.x = element_text( size=text.size ) ,
          axis.title.y = element_text( size=text.size ),
          legend.text = element_text( size = 14))
  
  lx<- log(x,10)
  dens.lx<- density(lx, adjust=2,n=100)
  summary.lx<-round(summary(lx),2)
  fit.lx<- data.frame(x=dens.lx$x,y=dens.lx$y/sum(dens.lx$y))
  
  lab.new <- paste("log10", lab, sep=" ")
  dat.lx<- data.frame(X= lx)
  plot.lx<- ggplot(dat.lx, aes(x = X)) + 
    geom_histogram(aes(y = ..density..), 
                   bins = n.bins, fill="red",color="black") + 
    #geom_density(color="red",size=2)+
    geom_line(data=fit.lx, aes(x=x,y=2*scale.up*y),color="cornflowerblue",size=2 )+
    theme_bw() +
    guides(fill = guide_legend(title=""))+
    ylab("Density")+
    xlab(lab.new)+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=text.size,angle=0 ), 
          axis.text.y=element_text(size=text.size ) ,
          title = element_text( size=text.size ) ,
          strip.text=element_text( size=text.size ) ,
          axis.title.x = element_text( size=text.size ) ,
          axis.title.y = element_text( size=text.size ),
          legend.text = element_text( size = 14))
  
  fit.lx.tmp <- fit.lx
  fit.lx.tmp$y<- fit.lx.tmp$y*nrow(dat.lx)
  
  
  fit.lx.tmp <- fit.lx
  fit.lx.tmp$y<- fit.lx.tmp$y*nrow(dat.lx)
  
  fit.x.tmp <- fit.lx.tmp
  fit.x.tmp$x <- 10^fit.x.tmp$x
  
  p1<- ggplot(dat, aes(x = X)) + 
    geom_histogram(aes(y = ..count..), 
                   bins = n.bins, fill="red",color="black") + 
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    geom_line(data=fit.x.tmp, aes(x=x,y=y),color="cornflowerblue",size=2 )+
    #geom_density(color="red",size=2)+
    theme_bw() +
    guides(fill = guide_legend(title=""))+
    ylab("Count")+
    xlab(lab)+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=text.size,angle=0 ), 
          axis.text.y=element_text(size=text.size ) ,
          title = element_text( size=text.size ) ,
          strip.text=element_text( size=text.size ) ,
          axis.title.x = element_text( size=text.size ) ,
          axis.title.y = element_text( size=text.size ),
          legend.text = element_text( size = 14))
  
  
  p2<- ggplot(dat, aes(x = X)) + 
    geom_histogram(aes(y = ..count..), 
                   bins = n.bins, fill="red",color="black") + 
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    geom_line(data=fit.x.tmp, aes(x=x,y=y),color="cornflowerblue",size=2 )+
    theme_bw() +   
    guides(fill = guide_legend(title=""))+
    ylab("Count")+
    xlab(lab)+
    #ggtitle(tab.label) +
    theme(axis.text.x=element_text(size=text.size,angle=0 ), 
          axis.text.y=element_text(size=text.size ) ,
          title = element_text( size=text.size ) ,
          strip.text=element_text( size=text.size ) ,
          axis.title.x = element_text( size=text.size ) ,
          axis.title.y = element_text( size=text.size ),
          legend.text = element_text( size = 14))
  
  
  
  r <- list(fit.rate=fit.x,fit.log.rate=fit.lx,summary.rate=summary.x, summary.log.rate=summary.lx,
            plot.rate=plot.x,plot.log.rate=plot.lx, p1=p1,p2=p2)
  
  
  return(r)
}


