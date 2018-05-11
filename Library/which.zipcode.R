which.zipcode <- function(long,lat, zipcode.data){
  
  distances.km <- get.great.circle.distance(long, lat,
                 zipcode.data$longitude,
                 zipcode.data$latitude,
                 rads= FALSE)
  
  row.position <-which.min(distances.km) 
  return(zipcode.data[row.position,])
  #example test -118.46210   34.04696 => 90025 zipcode
}

# deg2rad <- function(deg) return(deg*pi/180)
# lonlat_sample.rad <- deg2rad(lonlat_sample) 
# zipcode.rad <- deg2rad(zipcode[,c("longitude","latitude")])
# 
# tmp <- gcd.slc(lonlat_sample.rad[1], lonlat_sample.rad[2], zipcode.rad$longitude, zipcode.rad$latitude)
# 
# 
# #example test -118.46210   34.04696 => 90025 zipcode
# tmp <- get.great.circle.distance(lonlat_sample[1], 
#                                  lonlat_sample[2],
#                                  zipcode$longitude,
#                                  zipcode$latitude,
#                                  rads= FALSE)
# 
# row.position <-which.min(tmp)
# zipcode[row.position, ]
# tmp[row.position]