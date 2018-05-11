merge.zipcodeinfo.shapedf <-function(shape.df,zip.df){
  
  colnames(zip.df)[colnames(zip.df)%in%"zipcode"] <- "ZIPCODE"
  zip.df$ZIPCODE<- as.numeric(zip.df$ZIPCODE)
  
  C.zip.df <- colnames(zip.df)[!(colnames(zip.df)%in%"ZIPCODE")] 
  shape.df <- shape.df[,!(colnames(shape.df) %in% C.zip.df)]
  
  shape.df$order<- as.numeric(rownames(shape.df))
  tmp<- merge(shape.df, zip.df, by.x='ZIPCODE',all.x=T,sort = FALSE)
  tmp<- tmp[order(tmp$order),]
  
  for(cc in C.zip.df){
    tmp[is.na(tmp[,cc]),cc] <- 0
  }

  shape.df<- tmp[,!(colnames(tmp)%in%"order")]
  
  return(shape.df)
}