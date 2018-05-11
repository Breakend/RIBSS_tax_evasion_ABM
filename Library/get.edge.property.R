get.edge.property <- function(time.frame.att,segments.table.edges){
  
  compliant <- time.frame.att$tax.ids[time.frame.att$compliant]
  non.compliant <- time.frame.att$tax.ids[!(time.frame.att$compliant)]
  
  type0<-(segments.table.edges$A.ID%in%compliant) & 
    (segments.table.edges$B.ID%in%compliant)
  
  type1<-(segments.table.edges$A.ID%in%non.compliant) & 
    (segments.table.edges$B.ID%in%compliant)
  
  type1<-  type1 | ((segments.table.edges$A.ID%in%compliant) & 
                      (segments.table.edges$B.ID%in%non.compliant))
  
  type2<-(segments.table.edges$A.ID%in%non.compliant) & 
    (segments.table.edges$B.ID%in%non.compliant)
  
  segments.table.edges$edge.property<- NA
  segments.table.edges$edge.property[type0] <-  "assor.compliant"  
  segments.table.edges$edge.property[type1] <-  "disassortative" 
  segments.table.edges$edge.property[type2] <-  "assor.evaders" 
  
  segments.table.edges$edge.property<- as.factor(segments.table.edges$edge.property)
  
  return(segments.table.edges)
}