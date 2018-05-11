lab <- paste("perceivedevasionratepopulation",subset.label,sep="_")
text.label<- summaryfunctionFull(per.eva.rate.pop)
text.label2<- summaryfunctionFull(per.eva.rate)
text.label3<- summaryfunctionFull(per.eva.rate.ME)

out <- rbind(text.label,text.label2,text.label3)
rownames(out) <- c("Population","People like you","People like you if many evade")
out <- data.frame(cbind(rownames(out),out))
colnames(out) <- c("Perceived Evasion Rate",names(text.label))

tmp.tab.out<- xtable(out,auto=T,digits=1)
print(tmp.tab.out,file=paste(survey.output.dir,lab,".tex",sep=""), include.rownames=F)


text.label<-paste(c(paste(names(text.label),text.label, sep="=")),collapse=" ")
text.label2<-paste(c(paste(names(text.label2),text.label2, sep="=")),collapse=" ")
text.label3<-paste(c(paste(names(text.label3),text.label3, sep="=")),collapse=" ")

levels(df$Under_Reporting) <- c("Population","People like you","People like you if many evade")

df.tmp <- df[!(df$Under_Reporting%in%"People like you if many evade"),]

p1<- ggplot()+
  geom_histogram(data=df.tmp, aes(x = percentage, y = 100*(..count..)/sum(..count..),
                                  fill=Under_Reporting), 
                 binwidth=10, color="black", position = "dodge") +
  theme_bw() +
  xlab("Percentage of people that under report")+
  ylab("Percentage of Respondents")+
  #ggtitle(tab.label) +
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_fill_manual(values = c("tomato1","steelblue1","black")) 
#   annotate("text", label = "per.eva.rate.pop",
#            x=0.6,y=330, size = 5, colour = "black",angle = 0)+
#   annotate("text", label = text.label,
#            x=0.6,y=310, size = 5, colour = "black",angle = 0)+
#   annotate("text", label = "per.eva.rate",
#            x=0.6,y=290, size = 5, colour = "black",angle = 0)+
#   annotate("text", label = text.label2, 
#            x=0.6,y=270, size = 5, colour = "black",angle = 0)
print(p1)

ggsave(p1,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)


df.tmp <- df[!(df$Under_Reporting%in%"Population"),]

p2<- ggplot()+
  geom_histogram(data=df.tmp, aes(x = percentage, y = 100*(..count..)/sum(..count..),
                                  fill=Under_Reporting), 
                 binwidth=10, color="black", position = "dodge") +
  theme_bw() +
  xlab("Percentage of people that under report")+
  ylab("Percentage of Respondents")+
  #ggtitle(tab.label) +
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_fill_manual(values = c("steelblue1","purple4","black")) 
print(p2)

ggsave(p2,file=paste(survey.output.dir,lab,"2",".pdf",sep=""),
       width=10, height=5)

df<-cbind(SD.clean[,"prim_key"],dat)

labelR2 <- c(variable ="fitted",R2.eqn(df,2,3,r2.only=F))
labelR2<-as.data.frame(t(labelR2))
colnames(labelR2) <-c("variable","label")

df$Delta <- (df$per.eva.rate.pop-df$per.eva.rate>0)
df$Delta.abs <- abs(df$per.eva.rate.pop-df$per.eva.rate)
p1<-ggplot(df,aes( y=per.eva.rate, x=per.eva.rate.pop,
                   color=Delta,size=Delta.abs)) +
  geom_point()+
  guides(color=FALSE,size=FALSE)+ 
  scale_size(range = c(1, 4))+
  geom_smooth(method=lm, se=FALSE,color="gray40")+
  geom_text(data=labelR2,aes(label=label), parse=T,x=25,y=90,size=7,color="gray40")+
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  xlab("PER in the Population")+
  ylab("PER in People like you")+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_color_manual(values = c("tomato1","steelblue1","black"))
print(p1)
lab<-"per.eva.rate.vs.per.eva.rate.pop"
lab <- gsub("\\.","_",lab)
ggsave(p1,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)

labelR2 <- c(variable ="fitted",R2.eqn(df,3,4,r2.only=F))
labelR2<-as.data.frame(t(labelR2))
colnames(labelR2) <-c("variable","label")

df$Delta <- (df$per.eva.rate-df$per.eva.rate.ME>0)
df$Delta.abs <- abs(df$per.eva.rate-df$per.eva.rate.ME)
p2<-ggplot(df,aes(x=per.eva.rate, y=per.eva.rate.ME,
                  color=Delta,size=Delta.abs)) +
  geom_point()+
  guides(color=FALSE,size=FALSE)+
  scale_size(range = c(1, 4))+
  geom_smooth(method=lm, se=FALSE,color="gray40")+
  geom_text(data=labelR2,aes(label=label), parse=T,x=25,y=90,size=7,color="gray40")+
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  geom_hline(yintercept = 50, linetype = 2)+
  ylab("PER in People like you if many evade")+
  xlab("PER in People like you")+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_color_manual(values = c("tomato1","steelblue1","black"))
print(p2)
lab<-"per.eva.rate.vs.per.eva.rate.ME"
lab <- gsub("\\.","_",lab)
ggsave(p2,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)

df$Delta.pop<- (50-df$per.eva.rate.pop)
df$Delta.nn<- df$per.eva.rate.ME-df$per.eva.rate


df$Delta <- as.numeric(df$Delta.pop-df$Delta.nn>0)
df$Delta[df$Delta.nn<0]<- 2
df$Delta[df$Delta.nn<0 & df$per.eva.rate.pop>50]<- 3
df$Delta[df$Delta.nn>=0 & df$per.eva.rate.pop>50]<- 4
df$Delta <- as.factor(df$Delta)
df$Delta.abs <- abs(df$Delta.nn-df$Delta.pop)

df.tmp <- df#[df$Delta.pop>0,]
labelR2 <- c(variable ="fitted",R2.eqn(df.tmp,2,8,r2.only=F))
labelR2<-as.data.frame(t(labelR2))
colnames(labelR2) <-c("variable","label")

p3<-ggplot(df.tmp,aes(x=per.eva.rate.pop, y=Delta.nn,
                      color=Delta,size=Delta.abs)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE,color="grey40")+ #,linetype = 2
  geom_text(data=labelR2,aes(label=label), parse=T,x=25,y=90,size=7,color="grey40")+
  xlab("PER in the Population")+
  ylab("Delta(PERs in People like you)")+
  guides(color=FALSE,size=FALSE)+
  scale_size(range = c(1, 4))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_vline(xintercept = 50, linetype = 2)+
  geom_abline(intercept = 50, slope = -1, linetype = 2)+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_color_manual(values = c("tomato1","steelblue1","darkolivegreen4","purple4","gray50"))
print(p3)
lab<-"Evasion.Delta.vs.Delta"
lab <- gsub("\\.","_",lab)
ggsave(p3,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)




tmp <- (df$per.eva.rate.pop<=50 & df$per.eva.rate<=df$per.eva.rate.ME +5) |
  (df$per.eva.rate.pop>=50 & df$per.eva.rate>=df$per.eva.rate.ME-5)
tmp[is.na(tmp)]<-F
df.tmp<- df
df.tmp$include <- tmp
df.tmp <- df.tmp[!is.na(df.tmp$per.eva.rate.pop),]

nrow(df.tmp)## number of respondents in this analysis.

p3.1<-ggplot(df.tmp) +
  geom_point(aes(x=Delta.pop, y=Delta.nn,
                 color=Delta,alpha=include),size=2)+
  #geom_text(data=labelR2,aes(label=label), parse=T,x=25,y=90,size=7,color="grey40")+
  xlab("Change in PER in the Population (%)")+
  ylab("Change in PERs in People like you (%)")+
  guides(color=FALSE,size=FALSE,alpha=FALSE)+
  scale_size(range = c(1, 4))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_color_manual(values = c("tomato1","steelblue1","darkolivegreen4","purple4","gray50"))+
  scale_alpha_manual(values = c(0.15,1))

print(p3.1)
lab<-"Evasion.Delta.vs.Delta_v2"
lab <- gsub("\\.","_",lab)
ggsave(p3.1,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)





tmp <- (df$per.eva.rate.pop<=50 & df$per.eva.rate<=df$per.eva.rate.ME +5) |
  (df$per.eva.rate.pop>=50 & df$per.eva.rate>=df$per.eva.rate.ME-5)
tmp[is.na(tmp)]<-F
df.tmp<- df
df.tmp$include <- tmp
df.tmp <- df.tmp[!is.na(df.tmp$per.eva.rate.pop),]

nrow(df.tmp)## number of respondents in this analysis.

df.tmp$Delta[df.tmp$Delta==1] <- 0
df.tmp$Delta[df.tmp$Delta==3] <- 0
df.tmp$Delta[df.tmp$Delta==4] <- 2
df.tmp$Delta[df.tmp$Delta==2] <- 1
df.tmp$Delta[df.tmp$Delta==0] <- 2
df.tmp$Delta[!(df.tmp$include)] <- 4


p3.1<-ggplot(df.tmp) +
  geom_point(aes(x=Delta.pop, y=Delta.nn,
                 color=Delta,alpha=include),size=2)+
  #geom_text(data=labelR2,aes(label=label), parse=T,x=25,y=90,size=7,color="grey40")+
  xlab("Change in PER in the Population (%)")+
  ylab("Change in PERs in People like you (%)")+
  guides(color=FALSE,size=FALSE,alpha=FALSE)+
  scale_size(range = c(1, 4))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_color_manual(values = c("tomato1","steelblue1","gray50"))+
  scale_alpha_manual(values = c(0.4,1))

print(p3.1)
lab<-"Evasion_Delta_vs_Delta_v3"
lab <- gsub("\\.","_",lab)
ggsave(p3.1,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)











tmp <- (df$per.eva.rate.pop<=50 & df$per.eva.rate<=df$per.eva.rate.ME +5) |
  (df$per.eva.rate.pop>=50 & df$per.eva.rate>=df$per.eva.rate.ME-5)
df.tmp <- df[tmp,]
df.tmp <- df.tmp[!is.na(df.tmp$per.eva.rate.pop),]

nrow(df.tmp)## number of respondents in this analysis.


labelR2 <- c(variable ="fitted",R2.eqn(df.tmp,7,8,r2.only=F))
labelR2<-as.data.frame(t(labelR2))
colnames(labelR2) <-c("variable","label")

p3<-ggplot(df.tmp,aes(x=Delta.pop, y=Delta.nn,
                      color=Delta,size=Delta.abs)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE,color="grey40")+ #,linetype = 2
  geom_text(data=labelR2,aes(label=label), parse=T,x=25,y=90,size=7,color="grey40")+
  xlab("Delta(PER in the Population)")+
  ylab("Delta(PERs in People like you)")+
  guides(color=FALSE,size=FALSE)+
  scale_size(range = c(1, 4))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_color_manual(values = c("tomato1","steelblue1","darkolivegreen4","purple4","gray50"))
print(p3)
lab<-"Evasion.Delta.vs.Delta.2"
lab <- gsub("\\.","_",lab)
ggsave(p3,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)



### Further analyze response of Percieved Evasion Rates (PERs)


df.tmp$grad <- (df.tmp$per.eva.rate.ME-df.tmp$per.eva.rate)/(50-df.tmp$per.eva.rate.pop)

df.tmp$grad[is.nan(df.tmp$grad)] <- NA
df.tmp$grad[df.tmp$per.eva.rate.pop==50] <- NA

tmp.out <- summaryfunctionFull(df.tmp$grad )

df.tmp$loggrad <- log10(df.tmp$grad)

p4 <- ggplot()+
  geom_histogram(data=df.tmp, aes(x = loggrad, y = 100*(..count..)/sum(..count..)), 
                 bins=20, color="black",fill="cornflowerblue", position = "dodge") +
  theme_bw() +
  xlab("log10 gradient in the response")+
  ylab("Percentage")+
  #ggtitle(tab.label) +
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))
print(p4)
lab<-"PER.gradient.response"
lab <- gsub("\\.","_",lab)
ggsave(p4,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)


df.tmp$elast <- (df.tmp$per.eva.rate.ME-df.tmp$per.eva.rate)/(50-df.tmp$per.eva.rate.pop)*(50+df.tmp$per.eva.rate.pop)/(df.tmp$per.eva.rate.ME+df.tmp$per.eva.rate)

df.tmp$elast[is.nan(df.tmp$elast)] <- NA
df.tmp$elast[df.tmp$per.eva.rate.pop==50] <- NA

tmp.out2 <- summaryfunctionFull(df.tmp$elast )

tmp.out <- rbind(tmp.out,tmp.out2)

df.tmp$diffdiff <- (df.tmp$per.eva.rate.ME-df.tmp$per.eva.rate)-(50-df.tmp$per.eva.rate.pop)
tmp.out2 <- summaryfunctionFull(df.tmp$diffdiff )
tmp.out <- rbind(tmp.out,tmp.out2)

rownames(tmp.out) <- c("gradient.response","elasticity","difference of differences")

print(xtable(tmp.out ,digits=1),  file=paste(survey.output.dir ,"PER.gradient.response",".tex",sep=""), include.rownames=T)




#### Fit the sigmoid curve 

df.tmp <- df.tmp[!is.na(df.tmp$per.eva.rate.pop),]
df.tmp <- df.tmp[!is.na(df.tmp$per.eva.rate.ME),]
df.tmp <- df.tmp[!is.na(df.tmp$per.eva.rate),]

df.tmp$per.eva.rate.ME[df.tmp$per.eva.rate.pop==50] <-
  df.tmp$per.eva.rate[df.tmp$per.eva.rate.pop==50] 

df.tmp$per.eva.rate.ME[df.tmp$per.eva.rate.ME<=df.tmp$per.eva.rate & df.tmp$per.eva.rate.pop<50] <-
  df.tmp$per.eva.rate[df.tmp$per.eva.rate.ME<=df.tmp$per.eva.rate & df.tmp$per.eva.rate.pop<50]*1.01

df.tmp$per.eva.rate.ME[df.tmp$per.eva.rate.ME>=df.tmp$per.eva.rate & df.tmp$per.eva.rate.pop>50] <-
  df.tmp$per.eva.rate[df.tmp$per.eva.rate.ME>=df.tmp$per.eva.rate & df.tmp$per.eva.rate.pop>50]*0.99

df.tmp$Delta.pop <- 50-df.tmp$per.eva.rate.pop
df.tmp$Delta.nn <- df.tmp$per.eva.rate.ME-df.tmp$per.eva.rate

labelR2 <- c(variable ="fitted",R2.eqn(df.tmp,7,8,r2.only=F))
labelR2<-as.data.frame(t(labelR2))
colnames(labelR2) <-c("variable","label")

p5<-ggplot(df.tmp,aes(x=Delta.pop, y=Delta.nn,
                      color=Delta,size=Delta.abs)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE,color="grey40")+ #,linetype = 2
  geom_text(data=labelR2,aes(label=label), parse=T,x=25,y=90,size=7,color="grey40")+
  xlab("Delta(PER in the Population)")+
  ylab("Delta(PERs in People like you)")+
  guides(color=FALSE,size=FALSE)+
  scale_size(range = c(1, 4))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))+
  scale_color_manual(values = c("tomato1","steelblue1","darkolivegreen4","purple4","gray50"))
print(p5)
lab<-"Evasion.Delta.vs.Delta.3"
lab <- gsub("\\.","_",lab)
ggsave(p5,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)



media.PER<-list()
for(jj in 1:nrow(df.tmp)){
  x <- c(df.tmp$per.eva.rate.pop[jj],50)/100
  y <- c(df.tmp$per.eva.rate[jj],df.tmp$per.eva.rate.ME[jj])/100
  
  if(x[1]==x[2]) {
    media.PER[[jj]]<-c(NA,NA)
  }else{
    if(sum(y)==0){y <- x/100}
    
    x<- c(x,0.99)
    y <- c(y,0.99)
  
    X <- log(x)
    Y <- erf.inv(2*y - 1)
    
    X[is.infinite(X)]<- -100
    Y[is.infinite(Y)]<- -100
    
    reg <- lm(Y~X,data.frame(X=X,Y=Y))
    
    tmp <- summary.lm(reg)$coefficients 
    
    c <- summary.lm(reg)$coefficients["(Intercept)","Estimate"]
    k <- summary.lm(reg)$coefficients["X","Estimate"]
    s <- k*sqrt(2)
    m <- exp(-c/k)
    
    media.PER[[jj]]<- c(s=s,m=m)
  }
}
media.PER <- do.call("rbind",media.PER)
summaryfunctionFull(media.PER)

df.tmp <- cbind(df.tmp,media.PER)


media.PER <- rbind(m=c(summaryfunctionFull(df.tmp[df.tmp$elast>0,"m"])),
                   s=c(summaryfunctionFull(df.tmp[df.tmp$elast>0,"s"])))


media.PER.out<- xtable(media.PER,digits=2)
print(media.PER.out,file=paste(survey.output.dir,"media.PER",".tex",sep=""), include.rownames=T)


sub.df<- df.tmp[df.tmp$m>0.3 & df.tmp$m<0.5,]
sub.df<-sub.df[!(is.na(sub.df$m)),]
sub.df<-sub.df[sub.df$elast>0,]
sub.df<-sub.df[sub.df$per.eva.rate.ME!=50,]

tmp<-sub.df$per.eva.rate.pop>sub.df$per.eva.rate & sub.df$per.eva.rate.ME>50
tmp<- sub.df[tmp,][2,c(1:4)] ## we want 12014672:1 


sm.sub <- colMeans(sub.df[,c(13,14)])
sub.df<- sub.df[c(20:30),c(1:4)]
sub.df<- rbind(sub.df,tmp)
sub.df$per.eva.rate.ME.pop<- 50

sub.df1<- sub.df[,c(1:3)]
sub.df2<- sub.df[,c(1,5,4)]
colnames(sub.df2) <- colnames(sub.df1)
sub.df<- rbind(sub.df1,sub.df2)
colnames(sub.df)[1]<- "prim_key"

m<- as.numeric(sm.sub["m"])
s<- as.numeric(sm.sub["s"])
sm.dat <- 100*as.data.frame(cbind( x=c(0:100)/100,
                                   y=cdf.lognormal(x=c(0:100)/100, 
                                                   log(m),1/s)))
m<- as.numeric(media.PER["m","Median"])
s<- as.numeric(media.PER["s","Median"])
sm.dat2 <- 100*as.data.frame(cbind( x=c(0:100)/100,
                                    y=cdf.lognormal(x=c(0:100)/100, 
                                                    log(m),
                                                    1/s) ))

p6<-ggplot(sub.df,aes(x=per.eva.rate.pop, y=per.eva.rate,color=prim_key)) +
  geom_point(alpha=0.6,size=6)+
  guides(color=FALSE,size=FALSE)+
  geom_line(data= sm.dat , aes(x=x, y=y),size=2,color="red", alpha=0.8,  linetype=2)+
  geom_line(data= sm.dat2 , aes(x=x, y=y),size=1,color="blue", alpha=0.35,  linetype=2)+
  scale_size(range = c(1, 4))+
  geom_vline(xintercept = 50, linetype = 2)+
  xlab("PER in the Population")+
  ylab("PER in People like you")+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))
print(p6)
lab<-"PER.sigmoid.example"
lab <- gsub("\\.","_",lab)
ggsave(p6,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)





tmp<- data.frame(x=c(0,100),y=c(0,100))

sub.df2 <-sub.df[sub.df$prim_key%in%"12014522:1",]
sub.df2 <-sub.df[sub.df$prim_key%in%"12014672:1",]

p7<-ggplot() +
  geom_area(data = tmp,aes(x=x,y=y),fill="lightblue",alpha=0.35,size=6) +
  geom_point(data=sub.df,aes(x=per.eva.rate.pop, y=per.eva.rate,color=prim_key),alpha=0.6,size=6)+
  geom_point(data=sub.df2,aes(x=per.eva.rate.pop, y=per.eva.rate,color=prim_key),alpha=1,size=10,shape=21)+
  guides(color=FALSE,size=FALSE)+
  #geom_line(data= sm.dat , aes(x=x, y=y),size=2,color="red", alpha=0.8,  linetype=2)+
  geom_line(data= sm.dat2 , aes(x=x, y=y),size=2,color="red", alpha=0.8,  linetype=2)+
  scale_size(range = c(1, 4))+
  geom_vline(xintercept = 50, size=1,linetype = 2,color="navy", alpha=0.8)+
  geom_vline(xintercept = 50, size=10,color="navy", alpha=0.1)+
  geom_abline(intercept = 0, slope=1,size=1,linetype = 2 ,color="darkolivegreen", alpha=0.8)+
  xlab("PER in the Population %")+
  ylab("PER in People like you %")+
  theme(axis.text.x=element_text(size=18,angle=0 ), 
        axis.text.y=element_text(size=18 ) ,
        title = element_text( size=18 ) ,
        strip.text=element_text( size=18 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 14))
print(p7)
lab<-"PER.sigmoid.example3"
lab <- gsub("\\.","_",lab)
ggsave(p7,file=paste(survey.output.dir,lab,".pdf",sep=""),
       width=10, height=5)
