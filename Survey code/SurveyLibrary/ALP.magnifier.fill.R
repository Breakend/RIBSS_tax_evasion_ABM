ALP.magnifier.fill<- function(per.aud.rate, per.aud.rate.mag ){
  
  z<- per.aud.rate
  x<- per.aud.rate.mag
  
  tmp <- !is.na(x) 
  y<- per.aud.rate.mag[tmp]
  new.per.aud.rate.mag <- NA*y
  
  new.per.aud.rate.mag[y%in%1]<- 0
  new.per.aud.rate.mag[y%in%2]<- 0.001/2
  new.per.aud.rate.mag[y%in%3]<- 0.01/2
  new.per.aud.rate.mag[y%in%4]<- 0.1/2
  new.per.aud.rate.mag[y%in%5]<- 1/2
  new.per.aud.rate.mag[y%in%6]<- 1
  
  z[tmp][is.na(z[tmp])] <- 1
  z[tmp] <- pmin(z[tmp],new.per.aud.rate.mag)
  
  return(z)
}
