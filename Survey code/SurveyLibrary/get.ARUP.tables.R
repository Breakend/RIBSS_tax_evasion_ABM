get.ARUP.tables<- function(z){

df <- z[,c("perceivedauditrate","auditrandom","perceivedarunderreport","perceivedaruprob")]
df[df$perceivedarunderreport%in%"same","perceivedaruprob"] <- df[df$perceivedarunderreport%in%"same","perceivedauditrate"]
df[df$perceivedarunderreport%in%"lower","perceivedaruprob"] <- pmin( 
  df[df$perceivedarunderreport%in%"lower","perceivedauditrate"],
  df[df$perceivedarunderreport%in%"lower","perceivedaruprob"])
df$ARUP.factor <- pmax(0.0001,df$perceivedaruprob)/pmax(0.0001, df$perceivedauditrate)


df.split <- split(df, df$perceivedarunderreport)
ARUP.tab1 <-do.call("rbind",lapply(df.split,FUN=function(x){
  r<-summary(x$ARUP.factor)
  if(!any(is.na(x$ARUP.factor))){r<-c(r,0)}
  return(r)
}))
ARUP.tab1 <- as.data.frame(ARUP.tab1)
ARUP.tab1$N <-table(z$perceivedarunderreport)
ARUP.tab1$N.prop <-round(ARUP.tab1$N /sum(ARUP.tab1$N ),2)

ARUP.tab1.q <-do.call("rbind",lapply(df.split,FUN=function(x){
  r<-quantile(x$ARUP.factor,prob=c(15:20)/20,na.rm=T)
  return(r)
}))


df.split <- split(df, df$auditrandom)
ARUP.tab2 <-do.call("rbind",lapply(df.split,FUN=function(x){
  r<-summary(x$ARUP.factor)
  if(!any(is.na(x$ARUP.factor))){r<-c(r,0)}
  return(r)
}))
ARUP.tab2 <- as.data.frame(ARUP.tab2)
ARUP.tab2$N <-table(z$auditrandom)
ARUP.tab2$N.prop <-round(ARUP.tab2$N /sum(ARUP.tab2$N ),2)

ARUP.tab2.q <-do.call("rbind",lapply(df.split,FUN=function(x){
  r<-quantile(x$ARUP.factor,prob=c(15:20)/20,na.rm=T)
  return(r)
}))

r<- list(ARUP.tab1=ARUP.tab1,ARUP.tab2=ARUP.tab2, ARUP.tab1.q=ARUP.tab1.q, ARUP.tab2.q=ARUP.tab2.q)
return(r)
}