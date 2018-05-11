ALP.fn.get.perc.of.respondents<-function(x,levels,received.question=NULL,label = NULL){
  
  y <- x
  rq <-received.question
  max.level.value <- max(y,na.rm=T)
  
  if(any(is.na(y))){
    y[is.na(y)] <- max.level.value+1
    max.level.value <- max(y,na.rm=T)
    levels <- c(levels, "Skipped question")
    if(!is.null(rq)){
      rq[is.na(rq)] <- FALSE
      y[!rq]<- max.level.value+1
      levels <- c(levels, "Not asked")
    }
    levels <- unique(levels)
  }
  
  l <- length(levels)
  boh<- rep(0,l)
  names(boh)<-1:l
  
  tmp <- table(as.numeric(y),useNA ="ifany")
  boh[names(tmp)]<-tmp
  if(any(is.na(y))){names(boh) <- c(levels,"NA")}else{names(boh) <-levels}
  
  out <- cbind(boh,100*boh/sum(boh))
  out <- as.data.frame(out)
  colnames(out) <- c("Freq.","Perc.")
  if("Not asked"%in%levels){
    boh2<- boh
    boh2["Not asked"]<-NA
    
    out <- cbind(out,100*boh2/sum(boh2,na.rm=T))
    colnames(out) <- c("Freq.","Perc.(all)", "Perc.")
  }
  levels<-rownames(out)
  out <- cbind(levels=levels,out)
  
  if(!is.null(label)){
    colnames(out)[1]<- label
  }
  return(out)
}