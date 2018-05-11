get.BC.tables<- function(z){

df <- z[,c("perceivedauditrate","bombcrater","bombcrateramount")]
df[df$bombcrater%in%"same","bombcrateramount"] <- df[df$bombcrater%in%"same","perceivedauditrate"]
df[df$bombcrater%in%"lower","bombcrateramount"] <- pmin( 
  df[df$bombcrater%in%"lower","perceivedauditrate"],
  df[df$bombcrater%in%"lower","bombcrateramount"])
df$BC.factor <- pmax(0.0001,df$bombcrateramount)/pmax(0.0001, df$perceivedauditrate)


df.split <- split(df, df$bombcrater)
BC.tab1 <-do.call("rbind",lapply(df.split,FUN=function(x){
  r<-summary(x$BC.factor)
  if(!any(is.na(x$BC.factor))){r<-c(r,0)}
  return(r)
}))
BC.tab1 <- as.data.frame(BC.tab1)
BC.tab1$N <-table(z$bombcrater)
BC.tab1$N.prop <-round(BC.tab1$N /sum(BC.tab1$N ),2)

BC.tab1.q <-do.call("rbind",lapply(df.split,FUN=function(x){
  r<-quantile(x$BC.factor,prob=c(15:20)/20,na.rm=T)
  return(r)
}))


r<- list(BC.tab1=BC.tab1, BC.tab1.q=BC.tab1.q)
return(r)
}