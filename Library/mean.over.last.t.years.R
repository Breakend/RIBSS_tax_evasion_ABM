
mean.over.last.t.years <- function(ids, track.id, var.name, last.t.yrs=50, by="bracket")
{
  var <- with(track.id, track.id[tax.ids %in% ids & t > last.t.yrs, var.name])
  return(mean(var))
}