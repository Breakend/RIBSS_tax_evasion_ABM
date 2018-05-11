get.ALP.c1.density<-function(X,adjust=1.5){
  
  dens <- density(X, adjust=adjust,from=0,to=100,n=101,na.rm=T)
  
  n <- length(dens$y)                       #$
  dx <- mean(diff(dens$x))                  # Typical spacing in x $
  y.unit <- sum(dens$y) * dx                # Check: this should integrate to 1 $
  dx <- dx / y.unit                         # Make a minor adjustment
  
  below30<- dens$x<=30
  renom <- sum(dens$y[below30])*dx
  lower.mean<-sum((dens$x*dens$y/renom)[below30]) * dx
  above30<- dens$x>30
  renom <- sum(dens$y[above30])*dx
  upper.mean<-sum((dens$x*dens$y/renom)[above30]) * dx
  overall.mean <- sum(dens$x*dens$y/sum(dens$y)) * dx
  
  means<- c(mean=overall.mean, mean.under30= lower.mean ,mean.over30= upper.mean, N= length(X))
  
  r<- list(density=dens, means=means )
  return(r)
}