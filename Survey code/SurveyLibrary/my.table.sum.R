my.table.sum <- function(x,var,var1,var2,weight){

### This a utility that generates a table of weighted sums of the
### variable var along the variables var1 and var2. x is a data frame
### and var,var1,var2,weight are strings that denote some columns of
### x. the table has var1 along the rows and var2 along the
### columns. If var2 is NULL then a one dimensional table is
### generated. This function does not have the options order.col and
### order.row like my.table.mean. They should be added at some point



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
  } else {
        x[,var1] <- as.character(x[,var1])
        s <-  split(x[,c(var,weight)],x[,var1],drop=TRUE)
        tab <-  sapply(s,FUN=function(x,var,weight){z <- sum(x[,var]*x[,weight])},var,weight)
  }
  return(tab)
}
