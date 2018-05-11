
### Note this function will give the overall audit rate (not audir percentage) from the table.audit.rates where the audit rates in the table are given as a percentage. If the table.audit.rates is missing then it will use the current.audit.rate (again not a percentage) to find the table.audit.rates where the audit rates are given as percentages. 
get.effective.audit.rate <- function(pop.data,IRS.table.audit.rates,table.audit.rates=NULL,current.audit.rate=NULL){
  
  if((is.null(table.audit.rates) & is.null(current.audit.rate)) |
     (!is.null(table.audit.rates) & !is.null(current.audit.rate))){
    stop("get.effective.audit.rate: need to provide only one input that is not null")
  }
  
  N<- nrow(pop.data)
  
  if(is.null(table.audit.rates)){
    table.audit.rates <- IRS.table.audit.rates
    rownames(table.audit.rates) <- 1:nrow(table.audit.rates)
    table.audit.rates$group <- rownames(table.audit.rates)
    
    df <- pop.data[,c("tax.ids","person.id","income","prop.hideable.income")] 
    
    df$group <- findInterval(df$income,c(table.audit.rates$Lower.Income,Inf))
    tab <-(table(df$group)/N)
    tab <- data.frame(group=names(tab),prop=as.numeric(tab))
    tab <- merge(tab,table.audit.rates[,c("group",
                                          "Examination.Coverage")],by.x="group")
    
    rescale<-100*current.audit.rate/as.numeric(tab$prop%*%tab$Examination.Coverage)
    
    tab$Examination.Coverage <- rescale*tab$Examination.Coverage
    eff.audit.rate <- as.numeric(tab$prop%*%tab$Examination.Coverage)
     
    table.audit.rates$Examination.Coverage[match(tab$group,table.audit.rates$group)] <- tab$Examination.Coverage
    R <- table.audit.rates[, names(table.audit.rates) != 'group']
  }
  
  if(is.null(current.audit.rate)){

    rownames(table.audit.rates) <- 1:nrow(table.audit.rates)
    table.audit.rates$group <- rownames(table.audit.rates)
    
    df <- pop.data[,c("tax.ids","person.id","income","prop.hideable.income")] 
    
    df$group <- findInterval(df$income,c(table.audit.rates$Lower.Income,Inf))
    tab <-(table(df$group)/N)
    tab <- data.frame(group=names(tab),prop=as.numeric(tab))
    tab <- merge(tab,table.audit.rates[,c("group",
                                          "Examination.Coverage")],by.x="group")
    
    eff.audit.rate <- as.numeric(tab$prop%*%tab$Examination.Coverage)/100
    R <-  eff.audit.rate
  }
  
  return(R)
}
