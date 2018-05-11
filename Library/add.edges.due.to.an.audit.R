add.edges.due.to.an.audit<- function(ids,edgelist.g,nn.int, audit.eff=1.282595){
  for(id in ids){
    tmp <- max(length(nn.int[[id]]),1)
    num.edges.to.add.due.to.an.audit<-stoch.round(tmp*(audit.eff-1))
    
    tmp <-edgelist.g[((edgelist.g[,1]%in% id) | (edgelist.g[,2]%in% id)) 
                     & !(edgelist.g$active),]
    
    ### Only add tmp.active edges to first time audited not to those who have been audited previously. 
    if(!any(tmp$tmp.active, na.rm = TRUE) & 
       nrow(tmp)>0 &
       num.edges.to.add.due.to.an.audit>0){
      num.edges.to.add.due.to.an.audit <- min(num.edges.to.add.due.to.an.audit, nrow(tmp))
      
      edges.new <- tmp[sample(rownames(tmp),1),]
      
      edges.new[, "active"] <- TRUE
      edges.new[, "tmp.active"] <- TRUE
      
      edgelist.g[rownames(edges.new), ] <- edges.new
      
      for(i in rownames(edges.new)){
        nn.int[[ edges.new[i,"V1"] ]] <- c(nn.int[[ edges.new[i,"V1"] ]] ,edges.new[i,"V2"])
        nn.int[[ edges.new[i,"V2"] ]] <- c(nn.int[[ edges.new[i,"V2"] ]], edges.new[i,"V1"])
      }
    }
  }
  return(list(edgelist.g=edgelist.g,nn.int=nn.int))
}