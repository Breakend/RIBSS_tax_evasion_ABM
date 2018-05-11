find.closest.entry<- function (x, value, only.first=TRUE) 
{
  R <- which(abs(x - value) == min(abs(x - value)))
  if (only.first & length(R) > 1) {
    R <- R[1]
  }
  return(R)
}
