fix.Qu29<-function(x){
  problem.entries<-!is.na(x$error_tokens)
  
  z<- as.data.frame(x[,2:5])
  
  problem.entries <- problem.entries & rowSums(z,na.rm=T)<100
  noproblem.entries <- !(problem.entries)
  
  problem.entries <- c(1:nrow(x))[problem.entries]
  noproblem.entries <- c(1:nrow(x))[noproblem.entries]
  
  
  z.tmp <- z[noproblem.entries,]
  z.tmp[is.na(z.tmp)]<-0
  z[noproblem.entries,]<-z.tmp
  
  
  abcd<-apply(z[problem.entries,],1,FUN=function(x){which(is.na(x))})
  
  abcd <- sapply(abcd, function(x){c("a","b","c","d")[x]})
  to.wrong.to.keep<-names(abcd)[sapply(abcd,FUN=length)==4]
  abcd<- sapply(abcd,function(x){paste(x,collapse=",")})
  abcd <- unlist(abcd)
  
  x$error_tokens<- ""
  x$error_tokens[problem.entries]<- abcd
  
  
  
  fix.able <- problem.entries[!(problem.entries%in%to.wrong.to.keep)]
  zz<- z[fix.able,]
  replacment <- 100-rowSums(z[fix.able ,],na.rm=T)
  for(i in rownames(zz)){
    tmp <- strsplit(abcd[i],",")
    tmp <- sapply(tmp,function(x){paste("taxesimportant_",x,sep="")})
    rep <- replacment[i]/length(tmp)
    for(j in tmp){
      zz[i,j]<-rep
    }
  }
  z[fix.able,]<- zz[,1:4]
  
  tmp<-(rowSums(z)==100 & !is.na(rowSums(z)) )
  x[!tmp,2:5] <- NA
  x[tmp,2:5]<- z[tmp,]
  
  return(x)
}