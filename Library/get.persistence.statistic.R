

get.persistence.statistic <- function(track.data, last.t.years = 40, percent = TRUE) {
  
  persistently.compliant <- 0
  persistently.non.compliant <- 0
  pop.size <- length(unique(track.data$tax.ids))
  
  ids <- track.data[track.data$percent.hideable.reported.0.years.ago >= 95, 'tax.ids']
  if(length(ids) > 0) {
    factored.ids <- factor(ids)
    df <- as.data.frame(table(factored.ids))
    
    persistent.ids <- df[df$Freq == last.t.years, 1]
    persistently.compliant <- length(persistent.ids)
  }
  
  ids <- track.data[track.data$percent.hideable.reported.0.years.ago <= 5, 'tax.ids']
  if(length(ids) > 0) {
    factored.ids <- factor(ids)
    df <- as.data.frame(table(factored.ids))
    
    persistent.ids <- df[df$Freq == last.t.years, 1]
    persistently.non.compliant <- length(persistent.ids)
  }
  
  if(percent == TRUE) {
    persistently.compliant <- 100*persistently.compliant/pop.size
    persistently.non.compliant <- 100*persistently.non.compliant/pop.size
    total.persistence <- persistently.compliant + persistently.non.compliant
  }
  
  return(list(persistently.compliant = persistently.compliant, 
              persistently.non.compliant = persistently.non.compliant,
              total.persistence = total.persistence))
}