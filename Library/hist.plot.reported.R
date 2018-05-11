hist.plot.reported <- function(track.data,recent.years=6,small=14,big=18,title=NULL,subtitle=NULL){
  
  
  track.data$audit.group[track.data$years.since.last.audit<recent.years  ] <- "Recently Audited"
  track.data$pen.group<- paste("Penalized, but not in the last",recent.years, "years",sep=" ") 
  track.data$pen.group[track.data$years.since.last.penalty>last.t.years] <- 
    paste("Never Penalized in the last",last.t.years , "years",sep=" ")
  track.data$pen.group[track.data$years.since.last.penalty<recent.years ] <- "Recently Penalized"
  
  report.plot <- 
    ggplot(track.data) +
    geom_histogram(boundary = 0, aes(x=percent.hideable.reported.0.years.ago/100,
                                     y=(..count../sum(..count..)), 
                                     fill = pen.group ),alpha=0.8,  
                   color="black",bins=10) +
    scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::percent)+
    theme_bw() +
    xlab("Hideable Income Reported") +
    ylab("Tax Payers")+
    scale_fill_manual(values = c( "steelblue3","darkolivegreen3", "red"))+ #"#78429E","#4F81BD"
    theme(legend.title = element_blank(),
          legend.position=c(0.55, 0.80),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small,face="italic"), 
          axis.text.y=element_text(size=small,face="italic" ) ,
          strip.text=element_text( size=small,face="italic" ) ,
          axis.title.x = element_text( size=big,face="italic" ) ,
          axis.title.y = element_text( size=big ,face="italic"),
          legend.text = element_text( size = small,face="italic"))+
    labs(title = title, subtitle = subtitle)
  
  print(report.plot)
  return(report.plot)
}



# 
# track.data<-pn1.fs
# last.t.years<- 30
# track.data$audit.group[track.data$years.since.last.audit<recent.years  ] <- "Recently Audited"
# track.data$pen.group<- paste("Penalized, but not in the last",recent.years, "years",sep=" ") 
# track.data$pen.group[track.data$years.since.last.penalty>last.t.years] <- 
#   paste("Never Penalized in the last",last.t.years , "years",sep=" ")
# track.data$pen.group[track.data$years.since.last.penalty<recent.years ] <- "Recently Penalized"
# 
# plot<-ggplot(track.data) +
#   geom_histogram(boundary = 0, aes(x=percent.reported.0.years.ago/100,
#                                    y=(..count../sum(..count..)), 
#                                    fill = pen.group ),alpha=0.8,  
#                  color="black",bins=20) +
#   scale_x_continuous(labels = scales::percent)+
#   scale_y_continuous(labels = scales::percent)+
#   theme_bw() +
#   xlab("Income Reported") +
#   ylab("Tax Payers")+
#   scale_fill_manual(values = c( "steelblue3","darkolivegreen3", "red"))+ #"#78429E","#4F81BD"
#   theme(legend.title = element_blank(),
#         legend.position=c(0.55, 0.80),
#         legend.background = element_rect(fill="transparent"),
#         panel.border = element_blank(),
#         axis.text.x=element_text(size=small,face="italic"), 
#         axis.text.y=element_text(size=small,face="italic" ) ,
#         strip.text=element_text( size=small,face="italic" ) ,
#         axis.title.x = element_text( size=big,face="italic" ) ,
#         axis.title.y = element_text( size=big ,face="italic"),
#         legend.text = element_text( size = small,face="italic")) 
# 
# file.name <- paste(figures.dir,"Model_","FullIncomeReportedHistogram",case,"PN1",".pdf",sep="")
# ggsave(plot,
#        file=file.name, 
#        width=7, height=3.5)
# 
# pop.data<- read.csv("Inputs/PN1_population_data.csv")
# sum(pop.data$prop.hideable.income*pop.data$income)/sum(pop.data$income)
