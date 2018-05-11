get.nearest.neighbours <- function(tax.ids, graph)
{
  missing.tax.ids<- tax.ids[!(tax.ids%in%V(graph))]
  if(length(missing.tax.ids)>0){
    stop("mismatch graph vertices and tax.ids - most likely the graph lost vertices with 0 edges")
    }
 
  nn <- list()
  nn <- sapply(tax.ids,FUN=function(i) {neighbors(graph, i)})
  if("id"%in%vertex_attr_names(graph)){
    nn<- sapply(nn, FUN=function(i) {V(graph)$id[i]})
  }
  names(nn)<- tax.ids
  return(nn)
  #   nn.table <- NULL
  #   for(id in tax.ids)
  #   {
  #     neighbs <- unlist(neighbors(graph, id))
  #     if(length(neighbs)==0) 
  #     {
  #       ntwrk <- NA
  #       browser()
  #     } else
  #     {
  #       ntwrk <- as.character(neighbs)
  #     }
  #     nn.table <- rbind(nn.table, cbind(id=id, nn=ntwrk))
  #   }
  #   
  #   return(list(nn=nn, nn.table=nn.table))
}