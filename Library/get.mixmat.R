# calculate the mixing matrix of in igraph graph object 'mygraph', by some vertex attribute 'attrib'
# can change the default use.density=FALSE to return a matrix with raw number of edges rather than density

get.mixmat <- function(g, attrib, use.density=TRUE, remove.na=TRUE, force.symmetry=TRUE ) {
  
  require(igraph)
  require(Matrix)
  
  # get unique list of characteristics of the attribute
  
  
  attlist <- sort(unique( get.vertex.attribute(g,attrib) )) ### remove sort( ...)
  
  if(any(grepl("\\[",attlist))){
    l<-gsub(",.*","",attlist)
    l <- gsub("\\[","",l)
    ol <- order(as.numeric(l))
    attlist<- attlist[ol]
  }
  
  numatts <- length(attlist)
  
  # build an empty mixing matrix by attribute
  if(remove.na){
    mm <- matrix(nrow=numatts, 
                 ncol=numatts,
                 dimnames=list(attlist,attlist))
  }else{
    na.attlist <- c(attlist,"NA")
    mm <- matrix(nrow=numatts+1, 
                 ncol=numatts+1,
                 dimnames=list(na.attlist,na.attlist))
  }

  
  # calculate edge density for each matrix entry by pairing type
  # lends itself to parallel if available
  el <- get.edgelist(g,names=FALSE)
  for (i in 1:numatts) {
    for (j in 1:numatts) {
      mm[i,j] <- sum(get.vertex.attribute(g, attrib, el[,1] )== attlist[i] &
                       get.vertex.attribute(g, attrib, el[,2] ) == attlist[j],na.rm=T)
    }  
  }
  
  if(remove.na){
      NA.mixing <-sum(is.na(get.vertex.attribute(g, attrib, el[,1])) |
            is.na(get.vertex.attribute(g, attrib, el[,2])),na.rm=T)
      denom <- ecount(g)-NA.mixing
  }else{
      for (j in 1:numatts) {
        mm[numatts+1,j] <- sum(is.na(get.vertex.attribute(g, attrib, el[,1])) &
                         get.vertex.attribute(g, attrib, el[,2] ) == attlist[j],na.rm=T)
        mm[j,numatts+1] <- sum(get.vertex.attribute(g, attrib, el[,1] )== attlist[j] &
                                 is.na(get.vertex.attribute(g, attrib, el[,2])),na.rm=T)
      }  
    mm[numatts+1,numatts+1] <- sum(is.na(get.vertex.attribute(g, attrib, el[,1])) &
                              is.na(get.vertex.attribute(g, attrib, el[,2])),na.rm=T)
    denom <- ecount(g)
  }
  
  if(force.symmetry){
    sum.m <- sum(mm)
    mm <- forceSymmetric(mm)
    mm <- sum.m*as.matrix(mm)/sum(mm)
  }
  
  # convert to proportional mixing matrix if desired (ie by edge density)
  if (use.density) {mm<- mm/denom} 
  return(mm)
}




