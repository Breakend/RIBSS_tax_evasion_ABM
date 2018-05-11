aggrgate.audit.costs<- function(pop.data,ids.by.audit.type,tab.audit.costs){
  
  ### tab.audit.costs <- read.csv("Inputs/Audit.Costs.csv")
  a.corresp<- findInterval(pop.data$income[ids.by.audit.type$ids.correpondence.audits],c(tab.audit.costs$Lower.Income,Inf))
  a.corresp<- tab.audit.costs$MeanCostCorresAudit[a.corresp]
  
  a.field<- findInterval(pop.data$income[ids.by.audit.type$ids.field.audits],c(tab.audit.costs$Lower.Income,Inf))
  a.field <- tab.audit.costs$MeanCostFieldAudit[a.field]
  
  R <- c(a.corresp,a.field)
  R<- c(total=sum(R, na.rm = T),mean= mean(R, na.rm = T))
  return(R)
}