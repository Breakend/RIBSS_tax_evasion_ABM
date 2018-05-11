#### Do logit S-curve


logit.ET.LMU <- ET.LMU
colnames(logit.ET.LMU ) <- c("1st Qu.", "Median" ,"3rd Qu.")
rownames(logit.ET.LMU) <- c("m","g")


### c2 =0.7
logit.ET.LMU["m",] <- c(0.312,0.396,0.483)  ### these numbers come from the Stata analysis.
logit.ET.LMU["g",] <- c(2.30,2.86,3.66)

### c2=1
#logit.ET.LMU["m",] <- c(0.349,0.461,0.569)
#logit.ET.LMU["g",] <- c(1.79,2.16,2.65)

m <- logit.ET.LMU["m","Median"]
g <- logit.ET.LMU["g","Median"]

tax.vals <- seq(0,1,0.0001)
ET.dat <- as.data.frame(cbind( x=tax.vals,y=1/(1+exp(-4*logit.ET.LMU["g","Median"]*(tax.vals-logit.ET.LMU["m","Median"]))) ))
ET.datL <- as.data.frame(cbind( x=tax.vals,
                                y=1/(1+exp(-4*logit.ET.LMU["g","1st Qu."]*(tax.vals-logit.ET.LMU["m","1st Qu."]))) ))
ET.datU <- as.data.frame(cbind(x=tax.vals,
                               y=1/(1+exp(-4*logit.ET.LMU["g","3rd Qu."]*(tax.vals-logit.ET.LMU["m","3rd Qu."]))) ))

ET.intercept <- 0.5-g*m
ET.slope <- g

p1<-ggplot(ET.dat) +
  geom_line(aes(x=x, y=y),size=2,color="navy")+
  geom_line(data= ET.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  geom_line(data= ET.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.2,  linetype="dotted")+
  xlim(0,1)+ylim(0,1)+
  xlab("Perceived Effective Tax Rate") +
  ylab("PER people like you \n with perceived deterrence")+
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
  geom_abline(intercept = ET.intercept, slope = ET.slope,size=1,color="darkgreen", alpha=0.5)+
  annotate("text",label="m", x=m,y=-0.0, size = 10, colour = "black", parse=TRUE)+
  annotate("text",label = "tilde(c)[1]^{(i)}",parse = TRUE,
           x=-ET.intercept/ET.slope+0.03,y=0.1, size = 10, colour = "darkgreen")+
  annotate("text",label = "g[m]",parse = TRUE,angle=20,
           x=0.50,y=0.9, size = 10, colour = "darkgreen")+
  geom_vline(xintercept = -ET.intercept/ET.slope, colour = "darkgreen",  linetype="dotted") 
p1


print(xtable(logit.ET.LMU,digits=3),  
      file=paste(survey.output.dir ,"logit_Evasion_Tax_Scurve_parameters",".tex",sep=""))
ggsave(p1,file=paste(survey.output.dir,"logit_Evasion_Tax_Scurve_lognormal_fit",".pdf",sep=""),
       width=8, height=4)



fixed.tax.vals <- c(0.35*c(1,1.25,1.6),0.7)
fixed.ET.dat <- as.data.frame(cbind( x=fixed.tax.vals,y=1/(1+exp(-4*logit.ET.LMU["g","Median"]*(fixed.tax.vals-logit.ET.LMU["m","Median"]))) ))
fixed.ET.dat$color <- 1
boh <- fixed.ET.dat
boh$x <- c(0.35,0.35,0.35,0.35)
boh$y <- fixed.ET.dat$y[1]*c(0.5,0.1,1.5,2)
boh$color <- 2

boh<- rbind(boh,c(0.22,0.5,2))

fixed.ET.dat <- rbind(fixed.ET.dat,boh)
fixed.ET.dat1<-fixed.ET.dat[1:4,]
fixed.ET.dat2<-fixed.ET.dat[5:6,]
fixed.ET.dat3<-fixed.ET.dat[7:8,]
fixed.ET.dat4<-fixed.ET.dat[9,]

ET.intercept2<-ET.intercept+c(fixed.ET.dat2$y-fixed.ET.dat$y[1],fixed.ET.dat3$y-fixed.ET.dat$y[1],-ET.intercept+fixed.ET.dat4$y-ET.slope*fixed.ET.dat4$x)

p2<-ggplot(ET.dat) +
  geom_line(aes(x=x, y=y),size=2,color="navy")+
  geom_line(data= ET.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.1,  linetype="dotted")+
  geom_line(data= ET.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.1,  linetype="dotted")+
  geom_point(data = fixed.ET.dat1,aes(x = x, y = y),size=6,color="navy",alpha=0.7)+
  geom_point(data = fixed.ET.dat2,aes(x = x, y = y),size=6,color="blue",alpha=0.4)+
  geom_point(data = fixed.ET.dat3,aes(x = x, y = y),size=6,color="red",alpha=0.4)+
  geom_point(data = fixed.ET.dat4,aes(x = x, y = y),size=6,color="darkgreen",alpha=0.4)+
  xlim(0,1)+ylim(0,1)+
  xlab("Perceived Effective Tax Rate") +
  ylab("PER people like you \n with perceived deterrence")+
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
  geom_abline(intercept = ET.intercept, slope = ET.slope,size=1,color="darkgreen", alpha=0.5)+
  geom_abline(intercept = ET.intercept2[1], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[2], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[3], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[4], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[5], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_vline(xintercept = -ET.intercept/ET.slope, colour = "darkgreen",  linetype="dotted") +
  annotate("text",label="m", x=m,y=0, size = 7, colour = "black", parse=TRUE)+
  annotate("text",label = "tilde(c)[1]^{(i)}",parse = TRUE,
           x=-ET.intercept/ET.slope+0.03,y=0.05, size = 7, colour = "darkgreen")+
  annotate("text",label = "g[m]",parse = TRUE,angle=40,
           x=0.50,y=0.9, size = 7, colour = "darkgreen")+
  annotate("text",label = "baseline",angle=0,
           x=0.3500-0.08 ,y=0.37139427, size = 5, colour = "navy",alpha=0.7)+
  annotate("text",label = "higher tax",angle=0,
           x=0.4375+0.08 ,y=0.61650976, size = 5, colour = "navy",alpha=0.7)+
  annotate("text",label = "even higher tax",angle=0,
           x=0.5600+0.11 ,y=0.86716943, size = 5, colour = "navy",alpha=0.7)+
  annotate("text",label = "fictitious point",angle=0,
           x=0.7+0.10 ,y=0.9700483-0.05, size = 5, colour = "navy",alpha=0.7)+
  
  annotate("text",label = "higher audit",angle=0,
           x=0.35+0.1 ,y=0.18569714 , size = 5, colour = "blue",alpha=0.7)+
  annotate("text",label = "even higher audit",angle=0,
           x=0.35+0.12 ,y=0.03713943, size = 5, colour = "blue",alpha=0.7)+
  annotate("text",label = "lower penalty",angle=0,
           x=0.35-0.1 ,y=0.5570914  , size = 5, colour = "red",alpha=0.7)+
  annotate("text",label = "even lower penalty",angle=0,
           x=0.35-0.14 ,y=0.7427885, size = 5, colour = "red",alpha=0.7)
p2
ggsave(p2,file=paste(survey.output.dir,"logit_Evasion_Tax_Scurve_lognormal_fit_v2",".pdf",sep=""),
       width=12*0.95, height=5*0.9)






p2<-ggplot(ET.dat) +
  geom_line(aes(x=x, y=y),size=2,color="navy")+
  #geom_line(data= ET.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.1,  linetype="dotted")+
  #geom_line(data= ET.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.1,  linetype="dotted")+
  geom_point(data = fixed.ET.dat1,aes(x = x, y = y),size=6,color="navy",alpha=0.7)+
  geom_point(data = fixed.ET.dat2,aes(x = x, y = y),size=6,color="blue",alpha=0.4)+
  geom_point(data = fixed.ET.dat3,aes(x = x, y = y),size=6,color="red",alpha=0.4)+
  geom_point(data = fixed.ET.dat4,aes(x = x, y = y),size=6,color="darkgreen",alpha=0.4)+
  xlim(0,1)+ylim(0,1)+
  xlab("") +
  ylab("")+
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
  geom_abline(intercept = ET.intercept, slope = ET.slope,size=1,color="darkgreen", alpha=0.5)+
  geom_abline(intercept = ET.intercept2[1], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[2], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[3], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[4], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[5], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_vline(xintercept = -ET.intercept/ET.slope, colour = "darkgreen",  linetype="dotted") 
p2
ggsave(p2,file=paste(survey.output.dir,"logit_Evasion_Tax_Scurve_lognormal_fit_v3",".pdf",sep=""),
       width=8, height=4)


fixed.ET.dat1_v2 <- fixed.ET.dat1
fixed.ET.dat1_v2$y[1:3] <- fixed.ET.dat1_v2$y[1:3] +0.1*0.5*c(-1,1,-1)
fixed.ET.dat1_v2$y[4] <- 0.975

p2<-ggplot(ET.dat) +
  geom_line(aes(x=x, y=y),size=2,color="navy")+
  #geom_line(data= ET.datL , aes(x=x, y=y),size=2,color="blue", alpha=0.1,  linetype="dotted")+
  #geom_line(data= ET.datU , aes(x=x, y=y),size=2,color="blue", alpha=0.1,  linetype="dotted")+
  geom_point(data = fixed.ET.dat1_v2,aes(x = x, y = y),size=6,color="navy",alpha=0.7)+
  geom_point(data = fixed.ET.dat2,aes(x = x, y = y),size=6,color="blue",alpha=0.4)+
  geom_point(data = fixed.ET.dat3,aes(x = x, y = y),size=6,color="red",alpha=0.4)+
  geom_point(data = fixed.ET.dat4,aes(x = x, y = y),size=6,color="darkgreen",alpha=0.4)+
  xlim(0,1)+ylim(0,1)+
  xlab("") +
  ylab("")+
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
  geom_abline(intercept = ET.intercept, slope = ET.slope,size=1,color="darkgreen", alpha=0.5)+
  geom_abline(intercept = ET.intercept2[1], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[2], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[3], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[4], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_abline(intercept = ET.intercept2[5], slope = ET.slope,size=1,color="darkgreen", alpha=0.25)+
  geom_vline(xintercept = -ET.intercept/ET.slope, colour = "darkgreen",  linetype="dotted") 
p2
ggsave(p2,file=paste(survey.output.dir,"logit_Evasion_Tax_Scurve_lognormal_fit_v4",".pdf",sep=""),
       width=8, height=4)


