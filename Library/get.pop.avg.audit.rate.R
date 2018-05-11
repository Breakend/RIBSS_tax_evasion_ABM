get.pop.avg.audit.rate <- function(pop.data,table.audit.rates,round.to=3, target.audit.rate=NULL) {
  
  N<- nrow(pop.data)
  rownames(table.audit.rates) <- 1:nrow(table.audit.rates)
  table.audit.rates$group <- rownames(table.audit.rates)
  
  df <- merge(pop.data[,c("tax.ids","person.id","income","prop.hideable.income")] 
              ,state.at.time.t[,c("tax.ids","report.abs")],by="tax.ids")
  
  df$group <- findInterval(df$report.abs,c(table.audit.rates$Lower.Income,Inf))
  tab <-(table(df$group)/N)
  tab <- data.frame(group=names(tab),prop=as.numeric(tab))
  
  
  
  train.data <- merge(tab,table.audit.rates[,c("group","Lower.Income","Examination.Coverage",
                                               "Coverage.Factor.wrt.Overall")],by.x="group")
  
  predict.data <- merge(tab,table.audit.rates[,c("group","Lower.Income","Examination.Coverage",
                                                 "Coverage.Factor.wrt.Overall")],by.y="group")
  
  predict.data <- table.audit.rates[(nrow(train.data)):nrow(table.audit.rates),]
  predict.data$dep <- log(predict.data$Lower.Income)
  
  train.data$ind <- log(train.data$prop)
  train.data$dep <- log(train.data$Lower.Income)
  model<-lm(ind~ dep, data = train.data[-1,])
  new.prop <- exp(predict(object=model,  newdata=predict.data))
  new.prop <- as.numeric(new.prop*train.data$prop[nrow(train.data)]/sum(new.prop))
  
  predict.data <- as.data.frame(cbind(prop=c(train.data$prop[-nrow(train.data)],new.prop),
                                      Examination.Coverage=table.audit.rates$Examination.Coverage))
  
  audit.rate.pop.avg<-sum(predict.data$prop *predict.data$Examination.Coverage)/100
  
  R <- round(audit.rate.pop.avg,round.to)
  
  ### if target.audit.rate is given as an input then change the audit tables to reflect this average. 
  if(!is.null(target.audit.rate)){
    m.factor<- target.audit.rate/audit.rate.pop.avg
    table.audit.rates$Examination.Coverage<- 100*table.audit.rates$Coverage.Factor.wrt.Overall* target.audit.rate
    R <- table.audit.rates
  }
  
  return (R)
}




