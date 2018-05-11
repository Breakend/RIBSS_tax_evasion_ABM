convert.UTM.to.longlatcoor<- function(x,zone=10){
  
  specification<- paste("+proj=utm +zone=",zone,sep="")
  utmcoor<-SpatialPoints(cbind(x$X,x$Y), 
                         proj4string=CRS(specification))
  longlatcoor<-as.data.frame(spTransform(utmcoor,CRS("+proj=longlat")))
  colnames(longlatcoor)<- c("lon","lat")
  
  return(longlatcoor)
}