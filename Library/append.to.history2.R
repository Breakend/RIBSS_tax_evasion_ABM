
append.to.history2 <- function(x.history, x.present, k, label = NULL)
{
  k<- k+1
  k <- max(k, 4)
  x.history <- cbind(x.history, x.present)
  if(ncol(x.history) > k)
  {
    n.cols <- ncol(x.history)
    range.of.cols <- (n.cols-k+1):n.cols
    x.history <- x.history[, range.of.cols]
  }
  
  n.cols <- ncol(x.history)
  #x.history <- as.data.frame(x.history)
  colnames(x.history) <- paste(label,seq((n.cols-1),0,-1),"years.ago",sep=".")
  
  return (x.history)
}