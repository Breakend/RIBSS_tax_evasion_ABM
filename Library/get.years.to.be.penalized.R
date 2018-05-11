get.years.to.be.penalized <- function(x,ids.penalized=NULL,eff.back.audits=NULL){
  if(is.vector(x)){
    R<-x
  }else{
    R<-x
    l<- ncol(x)
    ### Eveveryone penalized in present year gets penalized in previous K years
    R[,1:(l-1)]<- !x[,1:(l-1)] & x[,l] ### penalized in year l and not penalized in previous years.
    ### Expect those that received a correpondence.audit
    if(!is.null(ids.penalized$ids.correpondence.audits)){
      R[ids.penalized$ids.correpondence.audits,1:(l-1)] <- FALSE ### if correspondence audit then K=1.
      ### So no back audits and penalties on previous years
    }
    if(!is.null(ids.penalized$ids.field.audits)){
      field.K <- eff.back.audits$K.list[names(eff.back.audits$K.list)%in%"field"]
      names(field.K) <- ids.penalized$ids.field.audits
      for(ii in ids.penalized$ids.field.audits){
        jj <- as.numeric(field.K[as.character(ii)])
        if(jj<l){
          R[ii,1:(l-jj)] <- FALSE
        }
      }
    }
  }
  return(R)
}

# old.get.years.to.be.penalized <- function(x,ids.correpondence.audits=NULL){
#   if(is.vector(x)){
#     R<-x
#   }else{
#     R<-x
#     l<- ncol(x)
#     ### Eveveryone penalized in present year gets penalized in previous K years
#     R[,1:(l-1)]<- !x[,1:(l-1)] & x[,l] ### penalized in year l and not penalized in previous years.
#     ### Expect those that received a correpondence.audit
#     if(!is.null(ids.correpondence.audits)){
#       R[ids.correpondence.audits,1:(l-1)] <- FALSE ### if correspondence audit then K=1.
#       ### So no back audits and penalties on previous years
#     }
#   }
#   return(R)
# }

# ori.eff.back.audits<- eff.back.audits
# ori.ids.penalized <- ids.penalized
