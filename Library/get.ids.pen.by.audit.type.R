get.ids.pen.by.audit.type<- function(ids.by.audit.type,audited.non.compliant, tax.ids){
  ids.pen.by.audit.type <- ids.by.audit.type
  ids.pen.by.audit.type$ids.correpondence.audits <- intersect(tax.ids[audited.non.compliant],ids.by.audit.type$ids.correpondence.audits)
  ids.pen.by.audit.type$ids.field.audits <- intersect(tax.ids[audited.non.compliant],ids.by.audit.type$ids.field.audits)
  return(ids.pen.by.audit.type)
}