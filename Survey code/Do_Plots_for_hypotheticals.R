

########################################
###                                  ###
###     Do Plots for hypotheticals   ###
###                                  ###
########################################

text.size <- 22


########################################
### Do plots on tax changes
########################################

tab <- list() 

df <- df.hypo[df.hypo$control%in%c("baseline","taxrate"),c("prim_key","control","question", 
                                                           "perceivedtaxrate","perceivedevasionrate")]
df$question <- as.factor(df$question)
df$ranges <- cut2(df$perceivedtaxrate,
                  cuts=unique(quantile(df$perceivedtaxrate,prob=c(0:10)/10,
                                       na.rm=T)))
#cuts= c(0,10,seq(15,45,5),50,75,100))
df<- df[!is.na(df$ranges),]

coef<- (lm(perceivedevasionrate~perceivedtaxrate-1,df))$coefficients
boh <- lm(perceivedevasionrate~perceivedtaxrate-1,df)
summary(boh)

lm.line  <- data.frame(Tax=seq(0,100,1),Evasion=coef*seq(0,100,1))

tab[["tax hypothetical"]] <- c(respondents = length(unique(df$prim_key)), data.points=  nrow(df) ) 

library(scales)
p<-ggplot(data=df, aes(x=perceivedtaxrate, y=perceivedevasionrate)) +
  stat_density2d(aes(fill=1000*..level..), contour=TRUE, geom="polygon") +
  geom_density2d(colour="white", size=0.7)+
  #geom_point(aes(shape=question),alpha=0.3) +
  geom_line(data=lm.line,aes(x=Tax, y=Evasion),
            color="red",size=1,linetype = "dotdash")+
  xlim(-5,60)+
  ylim(-5,65)+
  xlab('Effective Tax Rate') +
  ylab('Expected % of People Like You \n  that Under-report')+
  theme(axis.text.x=element_text(size=text.size,angle=0 ), 
        axis.text.y=element_text(size=text.size ) ,
        title = element_text( size=text.size ) ,
        strip.text=element_text( size=text.size ) ,
        axis.title.x = element_text( size=text.size ) ,
        axis.title.y = element_text( size=text.size ),
        legend.text = element_text( size = 18))+
  scale_fill_gradient(limits = c(0,2),breaks = c(0:2),name = "", labels = comma) #+
  #annotate("text",label = "y=0.71x  R^2=0.43",x=40,y=60, size = 8, colour = "darkgreen")
print(p)
p<-ggsave(p,file=paste(survey.output.dir,
                       "perceivedevasionrate_changes_in_taxes_density",".pdf",sep=""),
          width=12, height=6)

p<-ggplot(data=df, aes(x=perceivedtaxrate, y=perceivedevasionrate)) +
  stat_density2d(aes(fill=1000*..level..), contour=TRUE, geom="polygon") +
  geom_density2d(colour="white", size=0.7)+
  #geom_point(aes(shape=question),alpha=0.3) +
  geom_line(data=lm.line,aes(x=Tax, y=Evasion),
            color="red",size=1,linetype = "dotdash")+
  xlim(-5,60)+
  ylim(-5,65)+
  xlab('') +
  ylab('')+
  theme(axis.text.x=element_text( size = 18) ,
        axis.text.y=element_text( size = 18) ,
        title = element_blank(  ) ,
        strip.text=element_blank( ) ,
        legend.text = element_text( size = 18),
        plot.margin = unit(c(0.5,1,1,1), "cm"))+
  scale_fill_gradient(limits = c(0,2),breaks = c(0:2),name = "", labels = comma) #+
#annotate("text",label = "y=0.71x  R^2=0.43",x=40,y=60, size = 8, colour = "darkgreen")
print(p)
p<-ggsave(p,file=paste(survey.output.dir,
                       "perceivedevasionrate_changes_in_taxes_density_v2",".pdf",sep=""),
          width=8, height=4)



p<- ggplot(data=df, aes(ranges, perceivedevasionrate))+
  geom_boxplot(fill ="cornflowerblue" ,
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Effective Tax Rate') +
  ylab('Expected % of People Like You \n that Under-Report')+
  theme(axis.text.x=element_text(size=text.size,angle=25 ), 
        axis.text.y=element_text(size=text.size ) ,
        title = element_text( size=text.size ) ,
        strip.text=element_text( size=text.size ) ,
        axis.title.x = element_text( size=text.size ) ,
        axis.title.y = element_text( size=text.size ),
        legend.text = element_text( size = 18))

print(p)
ggsave(p,file=paste(survey.output.dir,"perceivedevasionrate_changes_in_taxes_box",".pdf",sep=""),
       width=12, height=6)



########################################
### Do plots on audit changes
########################################

df <- df.hypo[df.hypo$control%in%c("baseline","auditrate"),c("prim_key","control","question", 
                                                             "perceivedauditrate","perceivedevasionrate")]
df$question <- as.factor(df$question)
df$ranges <- cut2(df$perceivedauditrate,
                  #cuts=unique(quantile(df$perceivedauditrate,prob=c(0:10)/10, na.rm=T)))
                  cuts= c(0,0.01,0.1,1,2,5,
                          unique(quantile(df$perceivedauditrate,prob=c(0:4)/4, na.rm=T))[-1]))
df<- df[!is.na(df$ranges),]

tab[["audit hypothetical"]] <- c(respondents = length(unique(df$prim_key)), data.points=  nrow(df) ) 

coef<- (lm(perceivedevasionrate~perceivedauditrate,df))$coefficients
lm.line  <- data.frame(Audit=seq(0,60,1),Evasion=coef[2]*seq(0,60,1)+coef[1])

p<- ggplot(data=df, aes(x=perceivedauditrate, y=perceivedevasionrate)) +
  stat_density2d(aes(fill=..level..), contour=TRUE, geom="polygon") +
  geom_density2d(colour="white", size=0.7)+
  geom_line(data=lm.line,aes(x=Audit, y=Evasion),
            color="red",size=1,linetype = "dotdash")+
  # geom_point(aes(shape=question),alpha=0.3) +
  xlim(0,70)+
  ylim(0,60)+
  xlab('Audit Rate') +
  ylab('Expected % of People Like You \n that Under-Report')+
  theme(axis.text.x=element_text(size=text.size,angle=0 ), 
        axis.text.y=element_text(size=text.size ) ,
        title = element_text( size=text.size ) ,
        strip.text=element_text( size=text.size ) ,
        axis.title.x = element_text( size=text.size ) ,
        axis.title.y = element_text( size=text.size ),
        legend.text = element_text( size = 18))

print(p)
p<-ggsave(p,file=paste(survey.output.dir,
                       "perceivedevasionrate_changes_in_audit_density",".pdf",sep=""),
          width=12, height=6)



p<- ggplot(data=df, aes(ranges, perceivedevasionrate))+
  geom_boxplot(fill ="cornflowerblue" ,
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Audit Rate') +
  ylab('Expected % of People Like You \n that Under-Report')+
  theme(axis.text.x=element_text(size=text.size,angle=25 ), 
        axis.text.y=element_text(size=text.size ) ,
        title = element_text( size=text.size ) ,
        strip.text=element_text( size=text.size ) ,
        axis.title.x = element_text( size=text.size ) ,
        axis.title.y = element_text( size=text.size ),
        legend.text = element_text( size = 18))

print(p)
ggsave(p,file=paste(survey.output.dir,"perceivedevasionrate_changes_in_audit_box",".pdf",sep=""),
       width=12, height=6)

########################################
### Do plots on penalty changes
########################################

df <- df.hypo[df.hypo$control%in%c("baseline","penaltyrate"),c("prim_key","control","question", "perceivedpenaltyrate","perceivedevasionrate")]
df$question <- as.factor(df$question)
df$ranges <- cut2(df$perceivedpenaltyrate,
                  cuts=unique(quantile(df$perceivedpenaltyrate,prob=c(0:10)/10, na.rm=T)))
#cuts= c(seq(0,100,10), 10000))
df<- df[!is.na(df$ranges),]

tab[["penalty hypothetical"]] <- c(respondents = length(unique(df$prim_key)), data.points=  nrow(df) )  


#df<-df[!grepl("lower",df$question),]

coef<- (lm(perceivedevasionrate~perceivedpenaltyrate,df))$coefficients
lm.line  <- data.frame(Penalty=seq(0,60,1),Evasion=coef[2]*seq(0,60,1)+coef[1])

p<- ggplot(data=df, aes(x=perceivedpenaltyrate, y=perceivedevasionrate)) +
  stat_density2d(aes(fill=..level..), contour=TRUE, geom="polygon") +
  geom_density2d(colour="white", size=0.7)+
  geom_line(data=lm.line,aes(x=Penalty, y=Evasion),
            color="red",size=1,linetype = "dotdash")+
  # geom_point(aes(shape=question),alpha=0.3) +
  xlim(0,60)+
  ylim(0,80)+
  xlab('Penalty Rate') +
  ylab('Expected % of People Like You \n that Under-Report')+
  theme(axis.text.x=element_text(size=text.size,angle=0 ), 
        axis.text.y=element_text(size=text.size ) ,
        title = element_text( size=text.size ) ,
        strip.text=element_text( size=text.size ) ,
        axis.title.x = element_text( size=text.size ) ,
        axis.title.y = element_text( size=text.size ),
        legend.text = element_text( size = 18))
print(p)

p<-ggsave(p,file=paste(survey.output.dir,
                       "perceivedevasionrate_changes_in_penalty_density",".pdf",sep=""),
          width=12, height=6)



p<- ggplot(data=df, aes(ranges, perceivedevasionrate))+
  geom_boxplot(fill ="cornflowerblue" ,
               outlier.colour =NULL,outlier.size = 0.2) +
  #geom_jitter(aes(color= question),alpha=0.3 )
  xlab('Penalty Rate') +
  ylab('Expected % of People Like You \n that Under-Report')+
  theme(axis.text.x=element_text(size=text.size,angle=25 ), 
        axis.text.y=element_text(size=text.size ) ,
        title = element_text( size=text.size ) ,
        strip.text=element_text( size=text.size ) ,
        axis.title.x = element_text( size=text.size ) ,
        axis.title.y = element_text( size=text.size ),
        legend.text = element_text( size = 18))
print(p)
ggsave(p,file=paste(survey.output.dir,"perceivedevasionrate_changes_in_penalty_box",".pdf",sep=""),
       width=12, height=6)




########################################
### Audit Penalty Rate symmentry 
########################################

df<-df.hypo[df.hypo$control%in%c("baseline","auditrate","penaltyrate"),
            c("prim_key","control","question", "perceivedauditrate","perceivedpenaltyrate","perceivedevasionrate")]


df<-df[!grepl("lower",df$question),]

id.audit <- df[df$control%in%c("auditrate"),]
id.audit <- unique(id.audit$prim_key)
id.penalty <- df[df$control%in%c("penaltyrate"),]
id.penalty <- unique(id.penalty$prim_key)

id.intersect <- intersect(id.audit,id.penalty)
length(id.intersect)

boh<- df[df$prim_key%in%id.intersect,]
boh.split<- split(boh,boh$prim_key)
df.split<- boh.split

tab[["Pq response"]] <- c(respondents = length(boh.split), data.points= nrow(boh) ) 

table(df$question)

# df.split<- split(df,df$prim_key)
# df.split<-lapply(df.split,FUN=function(x){
#   z<- max(x$perceivedevasionrate)
#   if( nrow(x)<5 | 
#       x[x$control%in%"baseline","perceivedevasionrate"]<z
#       ) {x<-NULL}
#   return(x)
# })
tmp <- do.call("rbind",df.split)
dim(tmp)
length(unique(tmp$prim_key))


pq.symm<-lapply(df.split,FUN=function(x){
  if(!is.null(x)){
    y <- x$perceivedevasionrate
    delta<- 0.1
  #  p <- c(delta+(y[1]-y[3])*3/4,delta+(y[2]-y[3])*2/3)
    p <- c(delta+(y[2]-y[3])*1/2, +(y[1]-y[3]))
    q <- c(delta+(y[4]-y[3]),delta+(y[5]-y[3]))
    #p<-pmin(rev(diff(x[c(3:1),"perceivedevasionrate"])),-1)
    #q<-pmin(diff(x[c(3:5),"perceivedevasionrate"]),-1)
    if(!is.na(prod(p))) {if(prod(p)==0) p<- q}
    r<- (p/q)
  }else{r<-NULL}
  return(r)
})
pq.symm<-do.call("c",pq.symm)
#pq.symm[pq.symm>10]<- 10
pq.symm[pq.symm< 0]<- 0

num.obs.pq.symm<- length(pq.symm)/2

pq.symm.tab<- list()
pq.symm.tab[["pq.symm"]] <-summary(pq.symm)
pq.symm.tab<- as.data.frame(do.call("rbind",pq.symm.tab))
print(xtable(pq.symm.tab),  file=paste(survey.output.dir ,"pq_symm",".tex",sep=""))

dat.lx<- data.frame(X= log10(pq.symm))
p<- ggplot(dat.lx, aes(x = X)) + 
  geom_histogram(aes(y = ..count..), 
                 bins = text.size, fill="red",color="black") + 
  theme_bw() +
  guides(fill = guide_legend(title=""))+
  ylab("Frequency")+
  xlab("log10(Pq response)")+
  #ggtitle(tab.label) +
  theme(axis.text.x=element_text(size=text.size,angle=0 ), 
        axis.text.y=element_text(size=text.size ) ,
        title = element_text( size=text.size ) ,
        strip.text=element_text( size=text.size ) ,
        axis.title.x = element_text( size=text.size ) ,
        axis.title.y = element_text( size=text.size ),
        legend.text = element_text( size = 18))

print(p)
ggsave(p,file=paste(survey.output.dir,"log10pqSymm",".pdf",sep=""),
       width=10, height=5)

pq.symm.summary<- summaryfunctionFull(pq.symm)

#pq.symm.summary$N <- pq.symm.summary$N/2

tmp.tab.out<- xtable(pq.symm.summary,digits=1)
tab.label <- "PqSymm_summary"
print(tmp.tab.out,  
      file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F)

tab <- do.call("rbind",tab)

tab<- as.data.frame(cbind(rownames(tab),tab))
colnames(tab)<- c("Analysis","number of respondents", "number of data points")
tmp.tab.out<- xtable(tab,digits=1)
tab.label <- "hypo_number_respondents_in_analysis"
print(tmp.tab.out,  
      file=paste(survey.output.dir,tab.label,".tex",sep=""), include.rownames=F)
