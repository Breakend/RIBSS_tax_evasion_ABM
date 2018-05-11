create.interacting.network.on.taxes<-function(g,net.degree.info, pop.data, target.average.degree.tTaxes =2.6266376, do.not.allow.zero.degrees=TRUE){
  
  g <- as.undirected(simplify(g))
  V(g)$id <- 1:vcount(g)
  
  avg.degree.ALP <-  target.average.degree.tTaxes*net.degree.info["k.all"]/net.degree.info["k.all.tT_Mean"]
  
  
  edgelist.g <-as.data.frame(as_edgelist(g))
  rownames(edgelist.g) <- paste(apply(edgelist.g,1,min), apply(edgelist.g,1,max),sep=".") 
  edgelist.g$type <- NA
  edgelist.g$active <- FALSE
  edgelist.g$tmp.active <- NA
  
  ### Create the network of self employed connections with self employed
  tmp <- (edgelist.g[,1]%in%V(g)[pop.data$self.employed]) & 
    (edgelist.g[,2]%in%V(g)[pop.data$self.employed])
  edgelist.g$type[tmp]<- "se.se" 
  edges.se.se<-E(g)[tmp]
  g.se.se <- subgraph.edges(g, edges.se.se)
  
  prob.tmp <- net.degree.info["M.se.se"]*avg.degree.ALP/(2*mean(degree(g.se.se)))
  tmp <- sample(c(FALSE,TRUE),ecount(g.se.se),replace=TRUE,prob=c(1-prob.tmp,prob.tmp))
  edgelist.g$active[edgelist.g$type%in%"se.se"] <- tmp
  
  mean(degree(subgraph.edges(g.se.se, E(g.se.se)[tmp])))
  ### note that the mean(degree(g.se.se.sub)) will be > than our target 
  ### net.degree.info["M.se.se"]*avg.degree.ALP
  ### this is because g.se.se.sub excludes the nodes created with 0 degree. 
  ### if we count those then things work out. 
  
  ### Create the network of non self employed connections with non self employed
  tmp <- (edgelist.g[,1]%in%V(g)[!pop.data$self.employed]) & 
    (edgelist.g[,2]%in%V(g)[!pop.data$self.employed])
  edgelist.g$type[tmp]<- "nse.nse" 
  edges.nse.nse<-E(g)[tmp]
  g.nse.nse <- subgraph.edges(g, edges.nse.nse)
  
  prob.tmp <- net.degree.info["M.nse.nse"]*avg.degree.ALP/(2*mean(degree(g.nse.nse)))
  tmp <- sample(c(FALSE,TRUE),ecount(g.nse.nse),replace=TRUE,prob=c(1-prob.tmp,prob.tmp))
  edgelist.g$active[edgelist.g$type%in%"nse.nse"] <- tmp
  
  mean(degree(subgraph.edges(g.nse.nse, E(g.nse.nse)[tmp])))
  
  ### Create the network of self employed connections with non self employed
  tmp <- (edgelist.g[,1]%in%V(g)[pop.data$self.employed]) & 
    (edgelist.g[,2]%in%V(g)[!pop.data$self.employed]) |
    (edgelist.g[,1]%in%V(g)[!pop.data$self.employed]) & 
    (edgelist.g[,2]%in%V(g)[pop.data$self.employed])
  edgelist.g$type[tmp]<- "se.nse" 
  edges.se.nse<-E(g)[tmp]
  g.se.nse <- subgraph.edges(g, edges.se.nse)
  
  if(all(c("M.se.nse","M.nse.se")%in%names(net.degree.info)) ){
    tmp.M <- mean(net.degree.info[c("M.se.nse","M.nse.se")])
  }else{
    tmp.M <- max(net.degree.info[c("M.se.nse","M.nse.se")],na.rm=T)
  }
  prob.tmp <- tmp.M*avg.degree.ALP/(2*mean(degree(g.se.nse)))
  tmp <- sample(c(FALSE,TRUE),ecount(g.se.nse),replace=TRUE,prob=c(1-prob.tmp,prob.tmp))
  edgelist.g$active[edgelist.g$type%in%"se.nse"] <- tmp
  
  
  ### Update the edgelist
  edgelist.g$tmp.active[edgelist.g$active] <- FALSE
  
  ### get list of nearest neighbours
  nn <- get.nearest.neighbours(pop.data$tax.ids, g)

  ### get the sub graph with the active edges
  g.sub <- subgraph.edges(g, E(g)[edgelist.g$active], delete.vertices = FALSE)

  ### get list of nearest neighbours in the sub graph
  nn.int <- get.nearest.neighbours(pop.data$tax.ids, g.sub)
  
  if( do.not.allow.zero.degrees){
    ids.with.degree.zero<- unlist(lapply(nn.int,length))
    ids.with.degree.zero <- V(g)$id[ids.with.degree.zero==0]
    
    ### make sure we have no individual with degree zero.
    for(i in ids.with.degree.zero){
      tmp <-(edgelist.g$V1%in%i | edgelist.g$V2%in%i)
      tmp<- edgelist.g[tmp,]
      rn <-sample(rownames(tmp),1)
      tmp<-tmp[rn,]
      nn.int[[tmp$V1]] <- c(nn.int[[tmp$V1]] ,tmp$V2)
      nn.int[[tmp$V2]] <- c(nn.int[[tmp$V2]],tmp$V1)
      tmp$active<-TRUE
      tmp$tmp.active<- FALSE
      edgelist.g[rn,]<- tmp
    }
  }
  
  ### Sanity checks
  for(id in 1:vcount(g)){ 
    edges.id <- edgelist.g[((edgelist.g[,1]%in% id) | (edgelist.g[,2]%in% id)) 
                           & edgelist.g$active,]
    list.nn.int <- nn.int[[id]]
    
    if( length(list.nn.int)!= nrow(edges.id)){
      #PK: Removing this for sensitivity analysis
      #This should probably be turned on after finalising important input parameter values.
      #browser()
      #stop("function create.interacting.network.on.taxes: mistmatch nn.int and edgelist.g")
    }
  }
  
  return(list(g=g, g.sub =g.sub, edgelist.g=edgelist.g, nn=nn, nn.int=nn.int))
  
}