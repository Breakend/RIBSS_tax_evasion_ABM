do.Elliptical.Grid.Mapping<-function(u,v,x.zoom=1,y.zoom=1){
  #https://stackoverflow.com/questions/13211595/how-can-i-convert-coordinates-on-a-circle-to-coordinates-on-a-square
  #https://arxiv.org/ftp/arxiv/papers/1509/1509.06344.pdf
  
  mm.u <- mean(u)
  mm.v<- mean(v)
  
  u <- u-mm.u
  v<- v-mm.v
  
  ss<- sqrt(max(abs(range(u^2+v^2))))
  
  ss.u<- max(abs(range(u)))
  ss.v<- max(abs(range(v)))
  
  u<- u/ss
  v<- v/ss
  
  #x = ½ √( 2 + u² - v² + 2u√2 ) - ½ √( 2 + u² - v² - 2u√2 )
  x<-0.5*sqrt(2+u^2-v^2+2*u*sqrt(2))-0.5*sqrt(2+u^2-v^2-2*u*sqrt(2))
  # y = ½ √( 2 - u² + v² + 2v√2 ) - ½ √( 2 - u² + v² - 2v√2 )
  y<-0.5*sqrt(2-u^2+v^2+2*v*sqrt(2))-0.5*sqrt(2-u^2+v^2-2*v*sqrt(2)) 
  
  x<-ss*x+mm.u
  y<-ss*y+mm.v
  
  mm.x<- mean(x)
  mm.y<- mean(y)
  
  x<- x.zoom*(x-mm.x)+mm.x
  y<- y.zoom*(y-mm.y)+mm.y
  
  return(cbind(x=x,y=y))
}