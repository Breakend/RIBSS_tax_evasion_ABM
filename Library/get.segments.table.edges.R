get.segments.table.edges<-function(g, dat){
  
  coord<- dat[,c("x","y","tax.ids")]
  
  edges <- as.data.frame(get.edgelist(g))
  colnames(edges)<-c("A.ID","B.ID")
  
  coord.set.A <- coord[as.character(edges$A.ID),c("x","y")]
  colnames(coord.set.A) <- c("A.x","A.y")
  
  coord.set.B <- coord[as.character(edges$B.ID),c("x","y")]
  colnames(coord.set.B) <- c("B.x","B.y")
  
  segments.table.edges<-cbind(edges,coord.set.A,coord.set.B)
  
  
  return(segments.table.edges)
}
