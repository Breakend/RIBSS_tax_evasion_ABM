#Calculating Jensen-Shannon Divergence
#Source: https://stackoverflow.com/questions/11226627/jensen-shannon-divergence-in-r

jensen.shannon.divergence <- function(p, q, scaled = F) {
  
  if(scale) {
    p <- p/q
    q <- rep(1, length(q))
  }
  m <- 0.5 * (p + q)
  JS <- 0.5 * (sum(p * log(p / m), na.rm = T) + sum(q * log(q / m), na.rm = T))
  return (JS)
  #return(H(m %*% w) - apply(m, 2, H) %*% w)
}

#Entropy
H <- function(v) {
  v <- v[v > 0]
  return(sum(-v * log(v)))
}
