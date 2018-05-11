
get.nn.plot.for.var <- function(track.data, var.of.interest, nn, last.n.yrs = 40, 
                                alpha = 0.01, cl = NULL, big = 14, small = 12) {
  
  track.data <- retval.post.eq$track.dyn
  max.t <- max(track.data$t)
  cut.off.t <- max.t - last.n.yrs
  voi <- var.of.interest #"hideable.reported"
  track.data <- track.data[track.data$t > cut.off.t, ]
  df <- get.nearest.neighbours.effects(track.data = track.data, nn = nn, 
                                       var.names = voi, functions = c(mean), 
                                       cl = cl)
  x <- df[, 1]
  y <- track.data[, voi]
  df <- data.frame(alters = x, ego = y)
  
  
  nn.plot <- ggplot(df, aes(alters, ego)) + geom_jitter(alpha = alpha) + 
    geom_smooth(method = "gam") + theme_bw() +
    theme(legend.title = element_blank(),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small), 
          axis.text.y=element_text(size=small ) ,
          strip.text=element_text( size=small ) ,
          axis.title.x = element_text( size=big ) ,
          axis.title.y = element_text( size=big ),
          legend.text = element_text( size = small))
  return(nn.plot)
}