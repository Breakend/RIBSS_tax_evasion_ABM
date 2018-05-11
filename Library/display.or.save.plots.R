#Display or Save Plots
display.or.save.plots <- function(plots, figures.dir, print.plots = TRUE, save.plots = FALSE,
                                  extra.label=NULL)
{
  if(print.plots)
  {
    for(i in names(plots)){
      print(plots[[i]])
    }
  }
  
  if(!is.null(extra.label)){
    extra.label <- paste("_",extra.label,sep="")
  }
  if(save.plots)
  {
    
    for(i in names(plots)){
      file.name <- gsub("\\.","_",i)
      file.name <- paste(figures.dir,"Model_",file.name,extra.label,".pdf",sep="")
      ggsave(plots[[i]],
             file=file.name, 
             width=7, height=3.5)
    }
  }
}

#   ggsave(plots$c1.plot,
#          file=paste(figures.dir,"Model_","c1_plot",".pdf",sep=""), 
#          width=10, height=5)
#   
#   ggsave(plots$w.plot,file=paste(figures.dir ,"Model_","w_plot",".pdf",sep=""),  width=10, height=5)
#   ggsave(plots$report.plot,file=paste(figures.dir ,"Model_","report_plot",".pdf",sep=""),  width=10, height=5)
#   ggsave(plots$Delta.Network.vs.Per.plot,file=paste(figures.dir ,"Model_","SI_compliance_plot",".pdf",sep=""),  width=10, height=5)
#   ggsave(plots$per.audit.rate.plot,file=paste(figures.dir,"Model_",
#                                               "per_audit_rate_plot",".pdf",sep=""),  width=10, height=5)
#   ggsave(plots$per.penalty.rate.plot,file=paste(figures.dir,"Model_",
#                                                 "per_penalty_rate_plot",".pdf",sep=""),  width=14, height=5)
#   ggsave(plots$years.since.last.compliant.plot,file=paste(figures.dir,"Model_",
#                                                           "years_since_last_compliant_plot",".pdf",sep=""),  width=10, height=5)
#   ggsave(plots$sample.report.trajectory.plot,
#          file=paste(figures.dir,"Model_",
#                     "sample_report_trajectory_plot",".pdf",sep=""),  width=15, height=10)
#   ggsave(plots$se.focus.ids.trajectory.plot,
#          file=paste(figures.dir,"Model_",
#                     "se_trajectory_plot",".pdf",sep=""),  width=14, height=11)
#   ggsave(plots$nse.focus.ids.trajectory.plot,
#          file=paste(figures.dir,"Model_",
#                     "nse_trajectory_plot",".pdf",sep=""),  width=14, height=12)
#   ggsave(plots$iterative.map.plot,
#          file=paste(figures.dir,"Model_","iterative_map_plot",".pdf",sep=""),  width=10, height=5)
#   ggsave(plots$aggregated.dyn.plot,
#          file=paste(figures.dir,"Model_","aggregated_dyn_plot",".pdf",sep=""),  width=10, height=5)