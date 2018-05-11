get.great.circle.distance <- function(lon1,lat1,lon2,lat2,rads= FALSE){
  
  if(!rads){
    lon1 <- deg2rad(lon1)
    lon2 <- deg2rad(lon2) 
    lat1 <- deg2rad(lat1)
    lat2 <- deg2rad(lat2) 
  } 
  
  
  R<- gcd.slc(lon1,lat1,lon2,lat2)
  
  return(R)
} 