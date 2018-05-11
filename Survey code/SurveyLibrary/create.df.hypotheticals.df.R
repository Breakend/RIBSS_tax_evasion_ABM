create.df.hypotheticals<- function(x){

df.xta<-x[,c("prim_key", 
              "perceivedtaxrate", 
              "perceivedpenaltyrate",
              "perceivedauditrate",
              "perceivedevasionrate",
              "perceivedunderreporttaxlower",
              "perceivedunderreporttaxmuchlower",
              "perceivedunderreporttaxhigher",
              "perceivedunderreporttaxmuchhigher",
              "perceivedunderreportpenaltylower",
              "perceivedunderreportpenaltymuchlower",
              "perceivedunderreportpenaltyhigher",
              "perceivedunderreportpenaltymuchhigher",
              "perceivedunderreportaudithigher",
              "perceivedunderreportauditmuchhigher")]
#               "tax.rate.threshold.min",
#               "tax.rate.threshold.max",
#               "c1.guessed")]

df.xta[,-1] <- as.data.frame(apply(df.xta[,-1],2,as.numeric))

baseline.df <-df.xta[,c("prim_key", 
                    "perceivedtaxrate", 
                    "perceivedpenaltyrate",
                    "perceivedauditrate",
                    "perceivedevasionrate")]

baseline.df$question <- "perceivedevasionrate"
baseline.df$control <- "baseline"

baseline.df<- baseline.df[, c("prim_key", 
                              "control",
                              "question",
                              "perceivedtaxrate", 
                              "perceivedpenaltyrate",
                              "perceivedauditrate",
                              "perceivedevasionrate")]


L.tax <- get.hypoteticals.perceivedevasionrate(df.xta,
          field.to.transform = "perceivedtaxrate", 
          mul=c(0.75,0.5), 
          fields = c("perceivedunderreporttaxlower",
                     "perceivedunderreporttaxmuchlower"),
          control ="taxrate",
          decreasing = TRUE, bound.at.100=TRUE)

H.tax <- get.hypoteticals.perceivedevasionrate(df.xta,
          field.to.transform = "perceivedtaxrate",
          mul=c(1.5,2), 
          fields = c("perceivedunderreporttaxhigher", 
                     "perceivedunderreporttaxmuchhigher"),
          control ="taxrate",
          decreasing = FALSE, bound.at.100=TRUE)


L.pen <- get.hypoteticals.perceivedevasionrate(df.xta,
          field.to.transform = "perceivedpenaltyrate", 
          mul=c(0.75,0.5),
          fields = c("perceivedunderreportpenaltylower",
                     "perceivedunderreportpenaltymuchlower"),
          control ="penaltyrate",
          decreasing = FALSE, bound.at.100=FALSE)

H.pen <- get.hypoteticals.perceivedevasionrate(df.xta,
          field.to.transform = "perceivedpenaltyrate",
          mul=c(1.5,2), 
          fields = c("perceivedunderreportpenaltyhigher",
                     "perceivedunderreportpenaltymuchhigher"),
          control ="penaltyrate",
          decreasing = TRUE, bound.at.100=FALSE)

audit.df <- get.hypoteticals.perceivedevasionrate(df.xta,
             field.to.transform = "perceivedauditrate",
             mul=c(2,3),
             fields = c("perceivedunderreportaudithigher",
                        "perceivedunderreportauditmuchhigher"),
             control ="auditrate",
             decreasing = TRUE, bound.at.100=TRUE)



tax.df<- unique(rbind(L.tax,H.tax))

tax.df <- merge(tax.df,baseline.df[,c( "prim_key", "perceivedpenaltyrate",
                              "perceivedauditrate")], by= "prim_key")
tax.df <- tax.df[,names(baseline.df)] 

pen.df<- unique(rbind(L.pen,H.pen))
pen.df<- merge(pen.df,baseline.df[,c( "prim_key", "perceivedtaxrate",
                             "perceivedauditrate")], by= "prim_key")
pen.df<-pen.df[,names(baseline.df)] 

audit.df <- merge(audit.df,baseline.df[,c( "prim_key", "perceivedtaxrate",
                                "perceivedpenaltyrate")], by= "prim_key")
audit.df <- audit.df[,names(baseline.df)]


out.df <- rbind(baseline.df,tax.df,pen.df,audit.df)

out.df<- out.df[order(out.df$prim_key,out.df$control),]

#out.df <- out.df[out.df$perceivedtaxrate>0,]

return(out.df)
}
