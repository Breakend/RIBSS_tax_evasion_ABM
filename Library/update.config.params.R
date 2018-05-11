
update.config.params <- function(conf, ux.config.params, input) {
  
  conf.params <- conf[, 1] #All param names
  
  if(!is.null(ux.config.params)) {
    rt <- sapply(ux.config.params, function(p){
      pos <- which(p == conf.params)
      if(length(pos) > 0) {
        conf[pos, 'value'] <<- input[[p]]
      }
    })
  }
  
  invisible(conf)
}