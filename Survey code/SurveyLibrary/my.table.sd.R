my.table.sd <- function(x,var,var1,var2,weight,order.col=NA,order.row=NA,
                        drop=FALSE){
  require(Hmisc)
### This a utility that generates a table of weighted means of the
### variable var along the variables var1 and var2. x is a data frame
### and var,var1,var2,weight are strings that denote some columns of
### x. the table has var1 along the rows and var2 along the
### columns. If var2 is NULL then a one dimensional table is
### generated. order.col is a numeric vector with as many element as
### columns of the table that specifies a re-ordering of the
### columns. similarly for order.row. Example:
### my.table.mean(a,"totexp","ageg","health.status","w",order.col=c(2,1,3,4,5))

    if (drop){
        x[,var1] <- factor(x[,var1])
        if (!is.null(var2))
            x[,var2] <- factor(x[,var2])
    }

    if (!is.null(var2)){
        a <-  split(x,list(x[,var1],x[,var2]),drop=TRUE)
        s <-  sapply(a,FUN=function(x,var,weight){sqrt(wtd.var(x[,var],x[,weight],normwt=TRUE,na.rm=TRUE))},
                     var,weight)
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
        tab <-  sapply(s,
                       FUN=function(x,var,weight){sqrt(wtd.var(x[,var],x[,weight],normwt=TRUE,na.rm=TRUE))},
                       var,weight)
        if (!identical(order.row,NA)) tab <- tab[order.row]
    }
    return(tab)
}

