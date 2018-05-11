get.ARUP.factor <- function (z) 
{
    df <- z[, c("perceivedauditrate", "auditrandom", "perceivedarunderreport", 
                "perceivedaruprob")]
    df[df$perceivedarunderreport %in% "same", "perceivedaruprob"] <- df[df$perceivedarunderreport %in% 
                                                                          "same", "perceivedauditrate"]
    df[df$perceivedarunderreport %in% "lower", "perceivedaruprob"] <- pmin(df[df$perceivedarunderreport %in% 
                                                                                "lower", "perceivedauditrate"], df[df$perceivedarunderreport %in% 
                                                                                                                     "lower", "perceivedaruprob"])
    df$ARUP.factor <- pmax(1e-04, df$perceivedaruprob)/pmax(1e-04, 
                                                            df$perceivedauditrate)
    return(df$ARUP.factor)
}