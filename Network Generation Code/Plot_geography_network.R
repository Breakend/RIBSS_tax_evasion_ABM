#############################################################
###                                                       ###
###        Load Google maps of Porland              
###                                                       ###
#############################################################

maps.dir <- "../../Network Data/Google_map_files/"
PortlandMap.file<- paste(maps.dir,"PortlandMap.RData",sep="")
if(file.exists(PortlandMap.file)){
  PortlandMap<- readRDS(file=PortlandMap.file)
}else{
  ### Note this is blocked by RAND's firewall - need to step outside RAND.
  PortlandMap<- list()
  PortlandMap[["9"]] <- get_map(location = 'Portland', zoom = 9)
  PortlandMap[["10"]]<-  get_map(location = 'Portland', zoom = 10)
  PortlandMap[["11"]]<-  get_map(location = 'Portland', zoom = 11)
  PortlandMap[["12"]]<-  get_map(location = 'Portland', zoom = 12)
  saveRDS(PortlandMap,file=PortlandMap.file)
}


#############################################################
###                                                       ###
###        Load Shape files of Portland              
###                                                       ###
#############################################################

Portland.dsn<-  "../../Network Data/Zip_Code_Boundaries/."
Portland = readOGR(dsn=Portland.dsn, layer="Zip_Code_Boundaries") 

Portland@data$id = rownames(Portland@data)

Portland.points = fortify(Portland, region="id")
Portland.df = join(Portland.points, Portland@data, by="id")
rownames(Portland.df) <- as.character(1:nrow(Portland.df))

postcodes <-unique(Portland.df$id)
postcode.values <- data.frame(id = c(postcodes),
                              postcode.values = runif(length(postcodes),5.0, 25.0))

Portland.df<- merge(Portland.df, postcode.values, by.x='id')


zip.df<- aggregate(Income~zipcode,data=demog.household, mean)
P.df<-merge.zipcodeinfo.shapedf(Portland.df,zip.df)
P.df$Income[P.df$Income<1]<-NA

# ggplot(data = Portland.df, aes(x = long, y = lat, group=group)) + 
#   geom_path()+
#   geom_polygon(aes(fill = postcode.values))+
#   scale_fill_continuous(guide = FALSE)
# 
# 
# tmp.demog.household <- demog.household[1:100000,]
# 
# ggplot() + 
#   geom_path(data = Portland.df, aes(x = long, y = lat,group=group), color="white")+
#   geom_polygon(data = Portland.df,aes(x = long, y = lat, group=group, fill = postcode.values))+
#   scale_fill_continuous(guide = FALSE) +
#   geom_point(data = tmp.demog.household, aes(x=lon,y=lat,color = Vehicles),  size = 0.5, alpha = 0.1)+
#   xlim(-123,-122.5)+ylim(45.2,45.7)



ggmap(PortlandMap[["10"]]) + 
  geom_polygon(aes(fill = postcode.values, x = long, y = lat, group = group), 
               data = Portland.df,
               alpha = 0.6, 
               color = "black",
               size = 0.2)+
  geom_point(data = tmp.demog.household, aes(x=lon,y=lat,color = Vehicles),  size = 0.5, alpha = 0.1)+
    scale_fill_continuous(guide = FALSE) +
  scale_color_continuous(guide = FALSE) 



ggmap(PortlandMap[["10"]]) + 
  geom_polygon(aes(fill = Income, x = long, y = lat, group = group), 
               data = P.df,
               alpha = 0.75, 
               color = "black",
               size = 0.4)+
  geom_point(data = demog.household, 
             aes(x=lon,y=lat),  
             size = 0.05, alpha = 0.1,color="magenta4")+
  scale_fill_continuous(guide = FALSE)+
  scale_color_continuous(guide = FALSE)

#############################################################
###                                                       ###
###        Plot reduced set of Households            
###                                                       ###
#############################################################

head(contact.new.sub.rewired)

Person.ID.set <- unique(c(contact.new.sub.rewired$Person.Id.1,contact.new.sub.rewired$Person.Id.2 ))

demog.person.sub <- demog.person[demog.person$Person.Id%in%Person.ID.set,]

demog.person.sub <- merge(demog.person.sub, 
                          demog.household[,c("Household.Id","lon","lat")],by="Household.Id" )

ggmap(PortlandMap[["10"]]) + 
  geom_polygon(aes(fill = Income, x = long, y = lat, group = group), 
               data = P.df,
               alpha = 0.75, 
               color = "black",
               size = 0.4)+
  geom_point(data = demog.person.sub, 
             aes(x=lon,y=lat),  
             size = 0.4, alpha = 1,color="firebrick4")+
  scale_fill_continuous(guide = FALSE)+
  scale_color_continuous(guide = FALSE)



  




