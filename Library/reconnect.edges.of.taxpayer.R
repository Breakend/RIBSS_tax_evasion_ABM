reconnect.edges.of.taxpayer <- function(ids,edgelist.g,nn.int){
  ### This function removes all the edges of taxpayer id and for those with 
  ### edgelist.g$tmp.active== FALSE it reconnects them. 
   

  for(id in ids){
    
     tmp <-edgelist.g[((edgelist.g[,1]%in% id) | (edgelist.g[,2]%in% id)) 
                      & edgelist.g$active,]
     
     
     edges.to.destroy <- edgelist.g[((edgelist.g[,1]%in% id) | (edgelist.g[,2]%in% id)) 
                                    & edgelist.g$active
                                    & edgelist.g$tmp.active,]
     edgelist.g[rownames(edges.to.destroy), "active"] <- FALSE
     edgelist.g[rownames(edges.to.destroy), "tmp.active"] <- NA

     
     if(nrow(edges.to.destroy)>0){
       for(i in rownames(edges.to.destroy)){
         tmp <- edges.to.destroy[i,c("V1","V2")]
         nn.int[[tmp$V1]] <- nn.int[[tmp$V1]][!(nn.int[[tmp$V1]]%in%tmp$V2)] 
         nn.int[[tmp$V2]] <- nn.int[[tmp$V2]][!(nn.int[[tmp$V2]]%in%tmp$V1)] 
       }
     }
  
     edges.to.replace <-edgelist.g[((edgelist.g[,1]%in% id) | 
                                      (edgelist.g[,2]%in% id)) & 
                                     edgelist.g$active,]
     edgelist.g[rownames(edges.to.replace),"active"] <- FALSE
     edgelist.g[rownames(edges.to.replace),"tmp.active"] <- NA
     
     
     # print(id)
     # print(nn.int[[id]])
     # print(edges.to.replace)
     # print(" ")
     
     for(i in nn.int[[id]]){
       nn.int[[i]]<- nn.int[[i]][!(nn.int[[i]]%in%id)]
     }
     nn.int[[id]] <- integer(0)
     
     num.type.edges.to.replace<- table(edges.to.replace$type)
     
     
   
     for(type in names(num.type.edges.to.replace)){
       
       if(num.type.edges.to.replace[type]>0){
         
         edges.potential<- edgelist.g[((edgelist.g[,1]%in% id) | 
                                         (edgelist.g[,2]%in% id)) &
                                        edgelist.g[,"type"]%in%type, ] 
         
         tmp <- sample(rownames(edges.potential),size=num.type.edges.to.replace[type] )
         edges.new <- edges.potential[tmp,]
         
         edges.new[, "active"] <- TRUE
         edges.new[, "tmp.active"] <- FALSE
         
         edgelist.g[rownames(edges.new), ] <- edges.new
         
         for(i in rownames(edges.new)){
           nn.int[[ edges.new[i,"V1"] ]] <- c(nn.int[[ edges.new[i,"V1"] ]] ,edges.new[i,"V2"])
           nn.int[[ edges.new[i,"V2"] ]] <- c(nn.int[[ edges.new[i,"V2"] ]], edges.new[i,"V1"])
         }
       }
     }
  }
  #sum(sapply(nn.int,length)<1) ## sanity checks
  #sum(edgelist.g$active) ## sanity checks
  return(list(edgelist.g=edgelist.g,nn.int=nn.int))
 }