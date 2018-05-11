get.audits.by.correpondence<-function(target.audit,pop.data,prop.of.correpondence.audit=0.75){
  tmp <- match(target.audit,pop.data$tax.ids)
  tmp<-order(pop.data$prop.hideable.income[tmp]*pop.data$income[tmp])
  ids.correpondence.audits<-(target.audit[tmp])[1:floor(length(target.audit)*
                                                          prop.of.correpondence.audit)]
  ids.field.audits<- target.audit[!(target.audit%in%ids.correpondence.audits)]
  R<-list(ids.correpondence.audits=ids.correpondence.audits,
          ids.field.audits=ids.field.audits)
  return(R)
}