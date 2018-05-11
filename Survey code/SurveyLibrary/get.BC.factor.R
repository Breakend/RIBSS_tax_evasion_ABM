get.BC.factor <- function (z) 
{
  df <- z[, c("perceivedauditrate", "bombcrater", "bombcrateramount")]
  df[df$bombcrater %in% "same", "bombcrateramount"] <- df[df$bombcrater %in% 
                                                            "same", "perceivedauditrate"]
  df[df$bombcrater %in% "lower", "bombcrateramount"] <- pmin(df[df$bombcrater %in% 
                                                                  "lower", "perceivedauditrate"], df[df$bombcrater %in% 
                                                                                                       "lower", "bombcrateramount"])
  df$BC.factor <- pmax(1e-04, df$bombcrateramount)/pmax(1e-04, 
                                                        df$perceivedauditrate)
  return(df$BC.factor)
}