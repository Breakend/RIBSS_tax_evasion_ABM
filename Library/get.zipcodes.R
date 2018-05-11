get.zipcodes <- function(table.of.coords, zipcode.data, is.UTM=FALSE,zone=10){
  
  ## reverse geocoding 
  tmp <- table.of.coords
  
  if(is.UTM){
    tmp<- convert.UTM.to.longlatcoor(tmp,zone=zone)
  }
  
  zipcode.to.store<-NULL 
  for(i in 1:nrow(tmp)){
    tmp2<-which.zipcode(tmp$lon[i],tmp$lat[i],zipcode.data)
    zipcode.to.store<- c(zipcode.to.store,tmp2$zip)  
  }
  

  ## Alternative way of doing reverse geocoding  
#   require("ggmap")
#   tmp2<- tmp[,c("lon","lat")]
#   zipcode.to.store<-NULL 
#   for(i in 1:nrow(tmp)){
#     x<-as.numeric(tmp2[i,c("lon","lat")])
#     res <- revgeocode(x, output="more")
#     zipcode.to.store<- c(zipcode.to.store,res$postal_code)  
#   }
  
  tmp$zipcode<-zipcode.to.store 
  
  table.of.coords<- cbind(table.of.coords,tmp)
  
  return(table.of.coords)
}

