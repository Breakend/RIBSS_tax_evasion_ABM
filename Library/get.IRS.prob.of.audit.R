get.IRS.prob.of.audit<- function(pop.data,state.at.time.t,table.audit.rates, 
                                 additional.criteria=NULL,
                                 use.IRS.tables=TRUE){

  N<- nrow(pop.data)
  rownames(table.audit.rates) <- 1:nrow(table.audit.rates)
  table.audit.rates$group <- rownames(table.audit.rates)
  
  df <- merge(pop.data[,c("tax.ids","person.id","income","prop.hideable.income")] 
  ,state.at.time.t[,c("tax.ids","report.abs")],by="tax.ids")
  
  df$group <- findInterval(df$report.abs,c(table.audit.rates$Lower.Income,Inf))
  tab <-(table(df$group)/N)
  tab <- data.frame(group=names(tab),prop=as.numeric(tab))
  tab <- merge(tab,table.audit.rates[,c("group",
                                        "Coverage.Factor.wrt.Overall")],by.x="group")

  if(use.IRS.tables){
    df$prob.df<-tab$Coverage.Factor.wrt.Overall[match(df$group,tab$group)]
    df$prob.df<- df$prob.df/sum(df$prob.df)
  }else{
    df$prob.df<- 1/N
  }


  if(is.null(additional.criteria)){
    df$ac<- (df$income)*(df$prop.hideable.income)^1 
    ### the high power puts more emphasis on the self-employed 
    ### or those with significant potential hideable-income.
    ### multiplication by income assumes that the IRS stil has a hunch/idea
    ### of what the true income of the tax.payer maybe, even when they report
    ### less (i.e., report.abs< income)
  }else{
    df$ac<-additional.criteria
  }
  
  mean.group.ac<- setNames(aggregate(df$ac, by=list(group=df$group), FUN=mean), 
                           c("group", "mean.group.ac"))
  
  df <- merge(df,mean.group.ac,by="group")
  
  ac.factor<-(df$ac/df$mean.group.ac)
  
  df$prob <- df$prob.df*ac.factor
  df$prob <- df$prob/sum(df$prob)
  
  ### Rearrange the order of the prob as they were provided by person.id as an input.
  df<-df[match(state.at.time.t$tax.ids,df$tax.ids),]

  return(df$prob)
}



#aoo<-setNames(aggregate(df$prob.df, by=list(group=df$group), FUN=mean), c("group", "mean"))
#aoo$aoo <- aoo$mean/mean(aoo$mean)

#df$prob2 <- df$income*df$prop.hideable.income/sum(df$income*df$prop.hideable.income)
#df$prob<- df$prob2#(df$prob+df$prob2)/2

### Sanity
# cor(df$prob,df$income)
# cor(df$prob,df$prop.hideable.income)
# aoo <- table(df$prob.df/sum(df$prob.df),df$group)
# correct.table <- colSums(aoo *as.numeric(rownames(aoo)))
# 
# 
# sanity.check.length <- 10^5
# ueh <- sample(N, sanity.check.length, replace=T, prob= df$prob)
# ueh <- df[ueh,]
# comparison.table <- table(ueh$group)/sanity.check.length
# 
# print(correct.table)
# print(comparison.table)
# 
