get.effective.tax.rate<- function(x, tax.rate.income.bracket){
  
  ## trib = tax rate income bracket table
  
  inc<- as.numeric(x["Income"])
  filing.status <- as.character(x["filing.status"])
  
  trib <- tax.rate.income.bracket[[filing.status]]
  
  if(is.na(inc)){
    income.tax<-NA
  }else{
    a<- pmax(inc-trib$min,0)
    b<- c(trib$max[1],diff(trib$max))
    
    c<- apply(cbind(a,b),1,min)
    
    income.tax<- as.numeric(trib$tax.rate%*%c)
  }
  return(income.tax)
}