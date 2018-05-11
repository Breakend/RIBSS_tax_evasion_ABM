rpert <- function( n, x.min, x.max, x.mode=NULL, lambda = 4, mu=NULL ){
  # See http://www.riskamp.com/beta-pert
  if(!is.null(mu) & is.null(x.mode)) {
    x.mode<- (mu*( lambda + 2 )- (x.min + x.max))/lambda
    if(x.mode>x.max) x.mode <- min(mu,x.max)
    if(x.mode<x.min) x.mode <- x.min
  }
  if(is.null(mu) & is.null(x.mode)) stop( "invalid parameters" );
  if( x.min > x.max || x.mode > x.max || x.mode < x.min ) stop( "invalid parameters" );
  
  x.range <- x.max - x.min;
  if( x.range == 0 ) return( rep( x.min, n ));
  
  mu <- ( x.min + x.max + lambda * x.mode ) / ( lambda + 2 );
  
  # special case if mu == mode
  if( mu == x.mode ){
  v <- ( lambda / 2 ) + 1
  }else {
    v <- (( mu - x.min ) * ( 2 * x.mode - x.min - x.max )) /
      (( x.mode - mu ) * ( x.max - x.min ));
  }
  
  w <- ( v * ( x.max - mu )) / ( mu - x.min );
  return ( rbeta( n, v, w ) * x.range + x.min );
}