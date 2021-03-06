parcoord.mod<- function (x, col = 1, lty = 1, var.label = FALSE, ...) 
{
  rx <- apply(x, 2L, range, na.rm = TRUE)
  x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/(max(x, 
                                                                na.rm = TRUE) - min(x, na.rm = TRUE)))
  matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty, 
          xlab = "", ylab = "", axes = FALSE,...)
  axis(1, at = 1L:ncol(x), labels = colnames(x),cex.axis=0.75)
  for (i in 1L:ncol(x)) {
    lines(c(i, i), c(0, 1), col = "grey70")
    if (var.label) 
      text(c(i, i), c(0, 1), labels = format(rx[, i], digits = 3), 
           xpd = NA, offset = 0.9, pos = c(1, 3), cex = 0.6)
  }
  invisible()
}
