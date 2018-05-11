get.quantiles <- function(var, name.prefix="var", breakpoints = NULL, return.percents = TRUE)
{
  #Setting the breakpoints if not supplied
  if(is.null(breakpoints))
    breakpoints <- c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100)
  
  h <- hist(x=var, breaks = breakpoints, plot = F)
  qs <- h$counts
  
  if(return.percents == T) {
    total.n <- length(var)
    qs <- 100*(qs/total.n)
  }
  
  names(qs) <- paste0(h$breaks[-1], '%')
  names(qs) <- lapply(names(qs), function(i) sprintf("%s.%s", name.prefix, i))
  
  return(qs)
}