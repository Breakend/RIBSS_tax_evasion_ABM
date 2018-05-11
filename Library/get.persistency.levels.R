get.persistency.levels<- function(dyn,persitancy.levels = c(1,2,5,10,15,20) ){
  persistency.df <-NULL
  persistency.dfL <-NULL
  persistency.dfH <-NULL
  for(p in persitancy.levels){
    
    cat.df<-cbind.data.frame(tax.ids=dyn$tax.ids,cat=findInterval(dyn$hideable.reported, c(0,p,100-p,100),all.inside = T) )
    
    out.lower<-by(cat.df,cat.df$tax.ids,simplify = F, FUN=function(x){
      y<-as.numeric(x$cat)
      y1 <- all(!as.logical(y-1))
      return(y1)
    })
    
    out.higher<-by(cat.df,cat.df$tax.ids,simplify = F, FUN=function(x){
      y<-as.numeric(x$cat)
      y2 <- all(!as.logical(y-3))
      return(y2)
    })
    
    out.all<-by(cat.df,cat.df$tax.ids,simplify = F, FUN=function(x){
      y<-as.numeric(x$cat)
      y1 <- all(!as.logical(y-1))
      y2 <- all(!as.logical(y-3))
      return(y1|y2)
    })
    
    cat.df$persistency.lower  <- unlist(out.lower)
    cat.df$persistency.higher  <- unlist(out.higher)
    cat.df$persistency  <- unlist(out.all)
    
    persistency.lower <- round(100*table(cat.df$persistency.lower)/nrow(cat.df),1)
    persistency.higher <- round(100*table(cat.df$persistency.higher)/nrow(cat.df),1)
    persistency <- round(100*table(cat.df$persistency)/nrow(cat.df),1)
    
    persistency.df <- c(persistency.df,persistency["TRUE"])
    persistency.dfL <- c(persistency.dfL,persistency.lower["TRUE"])
    persistency.dfH <- c(persistency.dfH,persistency.higher["TRUE"])
  }
  
  persistency.df <- cbind.data.frame(persitancy.levels=persitancy.levels,persistency=persistency.df,persistency.dfL,persistency.dfH)
  
  names(persistency.df)[c(3,4)] <- c("low compliance persistency",
                                  "high compliance persistency")
  return(persistency.df)
  
}


