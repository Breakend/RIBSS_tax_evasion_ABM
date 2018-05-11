#Update the plot data for time t

update.plot.data <- function(tmp.dyn, 
                             alpha.node.attribute = "evaded",
                             alpha.node.exponent=2,
                             alpha.edge.exponent=1,
                             alpha.edge.factor=1,
                             group1=NULL,
                             group2=NULL) {
  
  df.vis <- g.data
  col.tmp <- df.vis$nodes$color
  alpha.adj <- (pmax(tmp.dyn[,alpha.node.attribute],0))^alpha.node.exponent
  
  df.vis$nodes$color <- mapply(FUN=function(x,y) {
    adjustcolor( x , alpha.f = y)
  },col.tmp,alpha.adj )
  
  if(!is.null(group1)){
    df.vis$nodes$shape[tmp.dyn[,group1]] <- "star"
    df.vis$nodes$color[tmp.dyn[,group1]] <- "#00FF33"
    df.vis$nodes$size[tmp.dyn[,group1]] <-  5
  }
  
  if(!is.null(group2)){
    df.vis$nodes$shape[tmp.dyn[,group2]] <- "dot"
    df.vis$nodes$color[tmp.dyn[,group2]] <- "white"
    df.vis$nodes$size[tmp.dyn[,group2]] <- 6
  }
  
  ids.c <- which(tmp.dyn$hideable.reported >= 95)
  df.vis$nodes[ids.c, 'group'] <- 'Compliant'
  
  ids.nc <- which(tmp.dyn$hideable.reported < 95)
  df.vis$nodes[ids.nc, 'group'] <- 'Non-Compliant'
  
  ids.a <- which(tmp.dyn$audited == T)
  df.vis$nodes[ids.a, 'group'] <- 'Audited'
  
  ids.p <- which(tmp.dyn$penalized == T)
  df.vis$nodes[ids.p, 'group'] <- 'Penalized'
  
  edge.alpha.adj<-cbind(alpha.adj[df.vis$edges$from],alpha.adj[df.vis$edges$to])
  edge.alpha.adj<- alpha.edge.factor*pmax((apply(edge.alpha.adj,1,min)),0)^alpha.edge.exponent

  df.vis$edges$color <- mapply(FUN=function(x,y) {
    adjustcolor( x , alpha.f = y)
  },"pink", edge.alpha.adj )
  
  #g.data <<- df.vis
  return(df.vis)
}

# update.plot.data.old <- function(tmp.dyn)
# {
#   #tmp.dyn <- track.dyn[track.dyn$t == t, ]
#   
#   nodes <- g.data$nodes
#   nodes[, "shapes"] <- "circle"
#   std.size <- 5
#   nodes[, 'size'] <- std.size + (100 - tmp.dyn$hideable.reported)/20
#   nodes[, 'title'] <- paste0("<p><b>Reported ", tmp.dyn$report, "% of Income</b></p>")
#   nodes[, 'font.size'] <- 24
#   
#   #Compliant
#   ids.c <- which(tmp.dyn$hideable.reported >= 95)
#   nodes[ids.c, "color"] <- "cornflowerblue" 
#   nodes[ids.c, 'group'] <- 'Compliant'
#   
#   #Non-Compliant
#   ids.nc <- which(tmp.dyn$hideable.reported < 95)
#   nodes[ids.nc, "color"] <- "red" 
#   nodes[ids.nc, 'group'] <- 'Non-Compliant'
#   
#   #Audited
#   ids.a <- which(tmp.dyn$audited == T)
#   #nodes[ids.a, "shape"] <- "square"
#   nodes[ids.a, 'color'] <- "yellow"
#   nodes[ids.a, 'group'] <- 'Audited'
#   nodes[ids.a, 'size'] <- nodes[ids.a, 'size'] + 5
#   nodes[ids.a, 'title'] <- paste0("<p><b>Reported ", tmp.dyn[ids.a, "hideable.reported"], "% of Income</b><br><b>Audited</b></p>")
#   
#   #Penalized
#   ids.p <- which(tmp.dyn$penalized == T)
#   #nodes[ids.p, "shape"] <- "triangle"
#   nodes[ids.p, "color"] <- "black"
#   nodes[ids.p, 'group'] <- 'Penalized'
#   nodes[ids.p, 'size'] <- nodes[ids.p, 'size'] + 10
#   nodes[ids.p, 'title'] <- paste0("<p><b>Reported ", tmp.dyn[ids.p, "hideable.reported"], "% of Income</b><br><b>Penalized</b></p>")
#   
#   g.data$nodes <<- nodes
# }
