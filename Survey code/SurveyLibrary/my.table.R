my.table <- function(x,weight,var1,var2,perc=FALSE,perc.col=FALSE,order=NA){
  if (!is.null(var2)){
    x[,var1] <- as.character(x[,var1])
    x[,var2] <- as.character(x[,var2])
    s <-  sapply(split(x[,weight],list(x[,var1],x[,var2])),sum)
    t <-  table(x[,var1],x[,var2])
    if (!identical(order,NA) & length(order) == ncol(t)) t <- t[,order]
    for (rn in rownames(t)){
      for (cn  in colnames(t)){
        n <- paste(rn,cn,sep=".")
        t[rn,cn] <- s[n]
      }
    }
    if (perc){
      tot <- apply(t,1,sum)
      t <-  t/tot
    }
    if (perc==FALSE & perc.col==TRUE){
      tot <- apply(t,2,sum)
      t <-  t(t(t)/tot)
    }
  } else {
    aux <- x[,var1]
    x[,var1] <- as.character(x[,var1])
    t <-  sapply(split(x[,weight],x[,var1]),sum)
    z <- table(aux)
    if (length(intersect(names(z),names(t)))!= length(t)) stop("mismatch in my.table")
    t <- t[names(z)]
    if (!identical(order,NA) & length(order) == length(t)) t <- t[order]
    t[is.na(t)] <- 0
    if (perc)
        t <- t/sum(t)
  }
  return(t)
}
