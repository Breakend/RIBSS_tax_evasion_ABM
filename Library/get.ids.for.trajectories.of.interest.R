get.ids.for.trajectories.of.interest <- function(subset.track.dyn, t) {
  
  ids <- unique(subset.track.dyn$tax.ids)
  c.to.nc <-intersect(subset.track.dyn$tax.id[subset.track.dyn$t == (t/2) & subset.track.dyn$hideable.reported > 95], 
                      subset.track.dyn$tax.id[subset.track.dyn$t == t & subset.track.dyn$hideable.reported < 5] )
  n <- length(c.to.nc)
  if(n > 0) {
    c.to.nc <- c.to.nc[sample(1:n, 1)]
    ids <- ids[!(ids %in% c.to.nc)]
  }
  
  nc.to.almost.c <-intersect(subset.track.dyn$tax.id[subset.track.dyn$t == (t/2) & subset.track.dyn$hideable.reported < 50], 
                             subset.track.dyn$tax.id[subset.track.dyn$t == t & subset.track.dyn$hideable.reported > 90] )
  n <- length(nc.to.almost.c)
  if(n > 0) {
    nc.to.almost.c <- nc.to.almost.c[sample(1:n, 1)]
    ids <- ids[!(ids %in% nc.to.almost.c)]
  }
  
  at.random <- sample(ids, 4 - (length(c.to.nc) + length(nc.to.almost.c))) # To ensure there are always four plots (2x2)
  
  return( c(c.to.nc, nc.to.almost.c, at.random) )
}