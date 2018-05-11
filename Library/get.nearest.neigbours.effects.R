#Get the mean effects of network per individual

get.nearest.neighbours.effects <- function(track.data, nn, var.names, functions, cl = NULL)
{
  if(is.null(cl))
  {
    alters.mean.var <- NULL
    for(id in track.data$tax.ids)
    {
      neighbs <- nn[id]
      neighbs <- unlist(neighbs)
      
      alters.mean.vars <- NULL
      n.vars <- length(var.names)
      for(i in 1:n.vars)
      {
        var.name <- var.names[i]
        f <- functions[[i]]
        
        var.val <- track.data[, var.name]
        alters.mean.vars <- cbind(alters.mean.vars, f(var.val[neighbs]))
        
      }
      
      alters.mean.var <- rbind(alters.mean.var, alters.mean.vars)
    }
  } else {
      alters.mean.vars <- NULL
      alters.mean.var <- parSapply(cl = cl, X = track.data$tax.ids, FUN = function(id)
        {
          neighbs <- nn[id]
          neighbs <- unlist(neighbs)
          n.vars <- length(var.names)
          
          var.name <- var.names[1]
          var.val <- track.data[, var.name]
          v1 <- mean(var.val[neighbs])
          
          var.name <- var.names[2]
          var.val <- track.data[, var.name]
          v2 <- mean(var.val[neighbs])
          rbind(v1,v2)
          
          for(i in 1:n.vars)
          {
            var.name <- var.names[i]
            f <- functions[[i]]
            var.val <- track.data[, var.name]
            alters.mean.vars <- cbind(alters.mean.vars, f(var.val[neighbs]))
          }
          alters.mean.vars
          
        })
      
      alters.mean.var <- as.data.frame(t(alters.mean.var))
  }
  return (alters.mean.var)
}


