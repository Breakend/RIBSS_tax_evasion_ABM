ALP.match.cor.guess <- function(rho.guess,rho,x,y,return.diff=FALSE,return.vector=FALSE) {
  
  old <- .Random.seed
  on.exit( { .Random.seed <<- old } )
  set.seed(2)
  
  n     <- length(x)                    # length of vector
  theta <- acos(rho.guess)             # corresponding angle
  x1    <- x       # fixed given data
  x2    <- rnorm(n, 10, 0.5)      # new random data
  X     <- cbind(x1, x2)         # matrix
  Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
  
  Id   <- diag(n)                               # identity matrix
  Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
  P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
  x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
  Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
  Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
  
  z <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
  
  #print(cor(x,z))
  
  a<-order(z, decreasing = T)
  b<-order(y, decreasing = T)
  
  tmp <- y
  tmp[a] <- tmp[b]
  
  R <- cor(x,tmp)
  if(return.vector){return(tmp)}else{
    if(return.diff) {return(R-rho)}else{return(R)}}
}