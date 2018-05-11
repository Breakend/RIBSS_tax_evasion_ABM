my.table.sum2 <- function(x,var,var1,var2,weight,order.col=NA,order.row=NA){
  if (!is.null(var2)){
    a <-  split(x,list(x[,var1],x[,var2]),drop=TRUE)
    s <-  sapply(a,FUN=function(x,var,weight){z <- sum(x[,var]*x[,weight])},var,weight)
    tab <-  table(x[,var1],x[,var2])
    for (rn in rownames(tab)){
      for (cn  in colnames(tab)){
        n <- paste(rn,cn,sep=".")
        tab[rn,cn] <- s[n]
      }
    }
    if (!identical(order.col,NA)) tab <- tab[,order.col]
    if (!identical(order.row,NA)) tab <- tab[order.row,]
  } else {
        x[,var1] <- as.character(x[,var1])
        s <-  split(x[,c(var,weight)],x[,var1],drop=TRUE)
        tab <-  sapply(s,FUN=function(x,var,weight){z <- sum(x[,var]*x[,weight])},var,weight)
        if (!identical(order.row,NA)) tab <- tab[order.row]
  }
  return(tab)
}
