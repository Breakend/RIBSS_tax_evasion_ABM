#get.ET.lognormal.fit.plm<-function(z,c2=0.7){
 # options(warn=0)
  
  c2<-0.7
  z<- df.hypo.tmp
  model.type  <- "within"
  
  z<-z[z$control%in%c("baseline","taxrate"),]

 
  colnames(z)[colnames(z)%in%"perceivedevasionrate"] <- "E"
  colnames(z)[colnames(z)%in%"perceivedtaxrate"] <- "T"
  
  z[,c("E","T")]<- z[,c("E","T")]/100
  
  z <- z[z$T>0,]
  z <- z[z$T<c2,]
  
  summaryfunctionFull(z$T) 
  summaryfunctionFull(z$E) 

  filter0.num.rec <-length(unique(z$prim_key))
  
  
  z$X<- NA
  z$Y <- NA
  
  df.split <- split(z,z$prim_key)
  
  z.new<- list()
  for(jj in names(df.split)){
    rec<- df.split[[jj]]
    rec <- rec[rec$T<c2,]
    rec$E[rec$E>0.95] <- 0.95
    if(nrow(rec)>1){ 
      
      rec<-rbind(rec[1,],rec)
      rec[1,"control"] <- "ficticious"
      rec[1,"T"] <- c2
      rec[1,"E"] <- 0.975
      
      
      X <- log(rec$T)
      Y <- erf.inv(2*rec$E - 1)
      
      X[is.infinite(X)]<- log(0.0001)
      Y[is.infinite(Y)]<- erf.inv(2*0.0001-1)
      
      rec$X <- X
      rec$Y <- Y
      
      z.new[[jj]]<- rec
    }else{
      z.new[[jj]]<-NULL
    }
  }
  
  z.new<- do.call("rbind",z.new)
  
  plm.model <- plm(Y~X +perceivedauditrate + perceivedpenaltyrate+ gender +calcage+income,
                   data = z.new, 
                   effect = "individual", 
                   model = model.type, 
                   index = "prim_key")
  
  summary(plm.model)
  
  if(model.type%in%"random"){
    
    c <- summary(plm.model)$coefficients["(Intercept)","Estimate"]
    k <- summary(plm.model)$coefficients["X","Estimate"]
    
    s <- k*sqrt(2)
    m <- exp(-c/k)
    g <- s/(m*sqrt(2*pi))
    
    print(paste("m=",round(m,2),"s=",round(s,2),"and g=", round(g,2), sep=" "))
    
    ET.LMU.plm<- data.frame()
    ET.LMU.plm["m","Mode"] <- m
    ET.LMU.plm["s","Mode"] <- s
    ET.LMU.plm["g","Mode"] <- g
    ["int","Mode"] <- 0.5-g*m
    
    c.delta <- 0.675*summary(plm.model)$coefficients["(Intercept)",
                                                     "Std. Error"]*sqrt(length(unique(z.new$prim_key)))
    k.delta <- 0.675*summary(plm.model)$coefficients["X","Std. Error"]*sqrt(length(unique(z.new$prim_key)))
    
    m<-exp(-(c-c.delta)/(k-k.delta))
    s<- (k-k.delta)*sqrt(2)
    g <- s/(m*sqrt(2*pi))
    
    ET.LMU.plm["m","Lower"] <- m
    ET.LMU.plm["s","Lower"] <- s
    ET.LMU.plm["g","Lower"] <- g
    ET.LMU.plm["int","Lower"] <- 0.5-g*m
    
    
    m<-exp(-(c+c.delta)/(k-k.delta))
    s<- (k+k.delta)*sqrt(2)
    g <- s/(m*sqrt(2*pi))
    
    ET.LMU.plm["m","Upper"] <- m
    ET.LMU.plm["s","Upper"] <- s
    ET.LMU.plm["g","Upper"] <- g
    ET.LMU.plm["int","Upper"] <- 0.5-g*m
    
  }else{

    summaryfunctionFull(fixef(plm.model))
    
    intercepts <- summaryfunctionFull(fixef(plm.model))
    c <- intercepts$Median
    k <- summary(plm.model)$coefficients["X","Estimate"]
    k.delta <- 0.675*summary(plm.model)$coefficients["X","Std. Error"]*sqrt(length(fixef(plm.model)))
    #k.delta <- summary(plm.model)$coefficients["X","Std. Error"]
    
    m<-exp(-intercepts$`1st Qu.`/(k-k.delta))
    s<- (k-k.delta)*sqrt(2)
    g <- s/(m*sqrt(2*pi))
    print(paste("m=",round(m,2),"s=",round(s,2),"and g=", round(g,2), sep=" "))
    
    ET.LMU.plm<- data.frame()
    
    ET.LMU.plm["m","Lower"] <- m * m.range.factor[,"Lower"]
    ET.LMU.plm["s","Lower"] <- s
    ET.LMU.plm["g","Lower"] <- g
    ET.LMU.plm["int","Lower"] <- 0.5-g*m * m.range.factor[,"Lower"]
    
    
    m<-exp(-intercepts$`3rd Qu.`/(k+k.delta))
    s<- (k+k.delta)*sqrt(2)
    g <- s/(m*sqrt(2*pi))
    
    ET.LMU.plm["m","Upper"] <- m *m.range.factor[,"Upper"]
    ET.LMU.plm["s","Upper"] <- s
    ET.LMU.plm["g","Upper"] <- g
    ET.LMU.plm["int","Upper"] <- 0.5-g*m *m.range.factor[,"Upper"]
    
    print(paste("m=",round(m,2),"s=",round(s,2),"and g=", round(g,2), sep=" "))
    
    s <- k*sqrt(2)
    m <- exp(-c/k)
    g <- s/(m*sqrt(2*pi))
    
    print(paste("m=",round(m,2),"s=",round(s,2),"and g=", round(g,2), sep=" "))
    
    ET.LMU.plm["m","Mode"] <- m * m.range.factor[,"Mode"]
    ET.LMU.plm["s","Mode"] <- s
    ET.LMU.plm["g","Mode"] <- g
    ET.LMU.plm["int","Mode"] <- 0.5-g*m * m.range.factor[,"Mode"]
    
  }
  
  tax.vals <- seq(0,1,0.0001)
  ET.dat <- as.data.frame(cbind( x=tax.vals,y=cdf.lognormal(x=tax.vals, log(ET.LMU.plm["m","Mode"]),1/ET.LMU.plm["s","Mode"])))
  ET.datL <- as.data.frame(cbind( x=tax.vals,
                                  y=cdf.lognormal(x=tax.vals, log(ET.LMU.plm["m","Lower"]),1/ET.LMU.plm["s","Lower"])))
  ET.datU <- as.data.frame(cbind(x=tax.vals,
                                 y=cdf.lognormal(x=tax.vals, log(ET.LMU.plm["m","Upper"]),1/ET.LMU.plm["s","Upper"])))
  
  
  p1<-ggplot(ET.dat) +
    geom_line(aes(x=x, y=y),size=2,color="red")+
    geom_line(data= ET.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
    geom_line(data= ET.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
    xlim(0,1)+ylim(0,1)+
    xlab("Perceived Effective Tax Rate") +
    ylab("PER people like you \n with perceived deterrance")+
    #ggtitle(expression(paste("Multiplicative factor in the ",tilde(c)[1], " equation",sep=" ")))+
    theme(axis.text.x=element_text(size=16 ), 
          axis.text.y=element_text(size=16 ) ,
          title = element_text( size=18 ) ,
          strip.text=element_text( size=12 ) ,
          axis.title.x = element_text( size=18 ) ,
          axis.title.y = element_text( size=18 ),
          legend.text = element_text( size = 12))+
    geom_vline(xintercept = m,  linetype="dotted") +
    geom_hline(yintercept = 0.5,  linetype="dotted")+
    geom_abline(intercept = ET.LMU.plm["int","Mode"], slope = ET.LMU.plm["g","Mode"],
                size=1,color="darkgreen", alpha=0.5)+
    annotate("text",label="m", x=m,y=-0.0, size = 10, colour = "black", parse=TRUE)+
    annotate("text",label = "tilde(c)[1]^{(i)}",parse = TRUE,
             x=-ET.LMU.plm["int","Mode"]/ET.LMU.plm["g","Mode"]-0.03,y=0.1, size = 10, colour = "darkgreen", parse=TRUE)+
    geom_vline(xintercept = -ET.LMU.plm["int","Mode"]/ET.LMU.plm["g","Mode"], colour = "darkgreen",  linetype="dotted") 
  print(p1)

  #options(warn=1)
  #return(r.list)
#}