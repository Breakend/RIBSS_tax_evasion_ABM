
#returns a scatter plot of Ego Vs. Alters of hideable income reported
get.alter.ego.plot <- function(track.dyn, g.info, t, var.of.int ='perc.hideable.income') {
  
  td <- track.dyn
  nn <- g.info$nn
  voi <- var.of.int
  
  td <- td[td$t <= t, ]
  df <- get.nearest.neighbours.effects(pop.data = td, nn = nn, var.names = voi, functions = c(mean))
  
  x <- df[, 1]
  y <- td[, voi]
  
  df <- data.frame(alters = x, ego = y, t = td$t, size = cor(y, x))
  ae.plot <- ggplot(df, aes(alters, ego)) + geom_jitter(alpha = 0.02) + geom_smooth()
  
  return(ae.plot)
}