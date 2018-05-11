check.for.equilibrium <- function(seq.x, last.n.years=200, mean.value.threshold = 5e-5, sign.value.threshold=5) {
  
  #Sometimes we may not have 40 years of data accrued yet. In that case modify last.n.years
  retval <- FALSE
  
  if(length(seq.x) > last.n.years) {
    rev.seq.x <- rev(seq.x)[1:last.n.years]
    # 1. Check whether the mean is greater than threshold
    if(abs(mean(rev.seq.x)) < mean.value.threshold && abs(mean(diff(rev.seq.x))) < 0.1*mean.value.threshold) {
      retval <- TRUE
      # y <- cumsum(sign(rev.seq.x))
      # if(all(abs(y)<sign.value.threshold)) {
      #   retval <- TRUE
      # } else {
      #   retval <- FALSE
      # }
    } 
  }  

  return(retval)
}