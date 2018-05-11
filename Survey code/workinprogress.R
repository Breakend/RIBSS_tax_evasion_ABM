x <- df$perceivedtaxrate
y <- df$income.val


df.tmp<-ALP.match.cor(x,y,rho=0.08,verbose=T)
df.tmp<- df.tmp[complete.cases(df.tmp),]

cor(df.tmp$x,df.tmp$y)



sampled.dist <- sample(1:10,30,replace=T)

predicted.reg <- 1:30


ALP.get.rearranged.sampled.dist<- function(sampled.dist,predicted.reg)
  a<-order(predicted.reg, decreasing = T)
  b<-order(sampled.dist, decreasing = T)
  
  tmp <- sampled.dist
  tmp[a] <- tmp[b]
  
  rearranged.sampled.dist<-tmp
  
  return(rearranged.sampled.dist)
}
