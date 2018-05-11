

get.background.color <- function() {
  sysinf <- Sys.info()
  bg <- NULL
  if(sysinf['sysname'] != "Windows") {
    bg <- "rgba(1,1,1,1)"
  }
 return(bg) 
}