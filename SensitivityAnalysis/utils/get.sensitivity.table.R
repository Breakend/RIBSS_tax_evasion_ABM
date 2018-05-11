get.sensitivity.table<- function(CART.fit,RF.fit,Normalize=TRUE, which.fit = "both"){
  
  #require(rpart)
  #require(randomForest)  
  
  dat <- as.data.frame(RF.fit$importance, stringsAsFactors = F)
  #Dropping the IncNodePurity Variable since we already have Per.IncMSE
  dat$IncNodePurity <- NULL
  colnames(dat)[1] <- "Per.RF.Imp"
  
  if(Normalize){
    dat$Per.RF.Imp <- 100*(dat$Per.RF.Imp/max(dat$Per.RF.Imp, na.rm = T))
    dat[names((CART.fit$variable.importance)),"Per.CART.Imp"] <- 100*(CART.fit$variable.importance/max(CART.fit$variable.importance, na.rm = T))
  }
  
  dat$model.inputs <- rownames(dat)
  if(which.fit == "both") {
    dat[, "overall"] <- apply(abs(dat[, 1:2]), 1, mean, na.rm = T)
  } else if(which.fit == "RF") {
    dat[, "overall"] <- dat[, 1]
  } else if(which.fit == "CART") {
    dat[, "overall"] <- dat[, 2]
  }
  
  dat <- dat[!is.na(dat$overall), ]
  dat <- dat[order(dat[,"overall"], decreasing = T),]
  
  return(dat)
}