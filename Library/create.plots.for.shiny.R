#############################################################
###                                                       ###
###       Create Plots for Shiny           
###                                                       ###
#############################################################
create.plots.for.shiny <- function(sim.data, track.dyn, aggregated.dyn, config)
{
  select.trajectories <- 3
  N <- get.config.param(config, 'population.size')
  
  tmp1<-intersect(track.dyn$tax.id[track.dyn$t%in%50 & track.dyn$report>95], 
                  track.dyn$tax.id[track.dyn$t%in%100 & track.dyn$report<5] )
  if(length(tmp1)>select.trajectories) tmp1<- sample(tmp1,select.trajectories)
  
  focus.ids<- tmp1
  
  tmp2<-intersect(track.dyn$tax.id[track.dyn$t%in%50 & track.dyn$report<50], 
                  track.dyn$tax.id[track.dyn$t%in%100 & track.dyn$report>90] )
  if(length(tmp2)>select.trajectories) tmp2<- sample(tmp2,select.trajectories)
  
  tmp3<- sample(1:N,select.trajectories)
  
  focus.ids <- c(tmp1,tmp2,tmp3)
  
  track.dyn.focus.ids <- track.dyn[track.dyn$tax.ids%in%focus.ids, ]
  track.dyn.focus.ids$tax.ids <- as.factor(track.dyn.focus.ids$tax.ids)
  
  tmp <-cbind(track.dyn.focus.ids[track.dyn.focus.ids$audit, c("t","tax.ids")],type="audit")
  tmp <- rbind(tmp, cbind(track.dyn.focus.ids[track.dyn.focus.ids$penalized, c("t","tax.ids")],type="penalized"))
  
  track.dyn.focus.ids.penalized <- tmp
  
  
  sim.data$report<-sim.data[,"percent.reported.0.years.ago"]
  sim.data$compliant.in.past.3.years <- with(sim.data, as.factor(years.since.last.compliant<4))
  
  sim.data$recently.penalized <- with(sim.data, as.factor(years.since.last.penalty<4))
  sim.data$penalized.3.yearsago <- with(sim.data, as.factor(years.since.last.penalty==3))
  sim.data$penalized.1.yearsago <- with(sim.data, as.factor(years.since.last.penalty==1))
  
  sim.data$compliant.group<- "compliant in the last 40 years"
  generation.half.life <- get.config.param(config, 'generation.half.life')
  sim.data$compliant.group[sim.data$years.since.last.compliant>generation.half.life] <- "never-compliant in last 40 years"
  
  morale.half.life <- get.config.param(config, 'morale.half.life')
  sim.data$compliant.group[sim.data$years.since.last.compliant<2*morale.half.life ] <- "compliant in last 4 years"
  sim.data$audit.group<- "audited but not in last 4 years"
  sim.data$audit.group[sim.data$years.since.last.audit>generation.half.life] <- "never audited in last 40 years"
  sim.data$audit.group[sim.data$years.since.last.audit<2*morale.half.life ] <- "recently audited"
  sim.data$pen.group<- "penalized but not in last 4 years"
  sim.data$pen.group[sim.data$years.since.last.penalty>generation.half.life] <- "never penalized in last 40 years"
  sim.data$pen.group[sim.data$years.since.last.penalty<2*morale.half.life ] <- "recently penalized"
  
  tmp <-sim.data$years.since.last.penalty
  tmp[tmp>3] <- Inf
  tmp <- 1/(tmp+1)
  tmp<-as.factor(tmp)
  levels(tmp) <- c("not penalized with past 3 years","penalized 3 years ago","penalized 2 years ago",
                   "penalized a years ago","penalized this year")
  sim.data$penalty.lag <- tmp
  
  c1.plot <- ggplot(sim.data) +
    geom_histogram(aes(x=c1,y=..density..),alpha=0.55, fill="pink",color="pink",binwidth = 2)+
    geom_histogram(aes(x=c1.tilde,y=..density..),alpha=0.55, fill="lightblue",color="blue",binwidth = 2)+
    geom_density(aes(x=c1, y = ..density..),alpha=0.7, fill="pink",color="red",size=0.1) +
    geom_density(aes(x=c1.tilde, y = ..density..),alpha=0.7, fill="lightblue",color="blue",size=0.1)+
    theme_bw() +
    xlab("tax rate") +
    ylab("density")+
    ggtitle(paste("distribution of c1 (red) and c1.tilde (blue)",sep=""))  
  
  w.plot <- ggplot(sim.data) +
    geom_histogram(aes(x=w,fill = compliant.group),alpha=1, color="black",binwidth = 2) +
    theme_bw() +
    xlab("propensity to fully report as a %") +
    ylab("count")+
    ggtitle(paste("Histogram of propensity to fully report",sep=""))  
  
  report.plot <- ggplot(sim.data) +
    geom_histogram(aes(x=report,fill = compliant.group ),alpha=1,  color="black",binwidth = 2) +
    theme_bw() +
    xlab(" % of income reported") +
    ylab("count")+
    ggtitle(paste("Histogram of % of income reported",sep=""))
  
  per.audit.rate.plot <- ggplot(sim.data) +
    geom_histogram(aes(x=per.audit.rate,fill=audit.group),alpha=1, color="black",binwidth = 1) +
    theme_bw() +
    xlab("per.audit.rate as a %") +
    ylab("count")+
    ggtitle(paste("Histogram of the per.audit.rate",sep="")) 
  
  per.penalty.rate.plot <- ggplot(sim.data) +
    geom_histogram(aes(x=per.penalty.rate, fill =pen.group),alpha=1, color="black",binwidth = 1) +
    theme_bw() +
    xlab("per.penalty.rate as a %") +
    ylab("count")+
    ggtitle(paste("Histogram of the per.penalty.rate",sep="")) 
  
  years.since.last.compliant.plot <- ggplot(sim.data) +
    geom_histogram(aes(x=years.since.last.compliant,fill =audit.group), color="black",binwidth = 2) +
    theme_bw() +
    xlab("years.since.last.compliant") +
    ylab("count")+
    ggtitle(paste("Histogram years.since.last.compliant",sep="")) 
  
  report.compliance.prob.plot <- ggplot(sim.data) +
    geom_point(aes(x=w, y=report, colour = compliant.in.past.3.years,
                   size=penalty.lag,alpha=c1.tilde))+
    xlab("prob to fully report (w) this year") +
    ylab("proportion of income reported this year")+
    ggtitle(paste("proportion of income reported",sep=""))
  
  sample.report.trajectory.plot<- ggplot(track.dyn.focus.ids) +
    geom_line(aes(x=t, y=report, group= tax.ids, colour = tax.ids),size=1.5)+
    geom_vline(data =track.dyn.focus.ids.penalized, aes(xintercept = t, colour= type, linetype=type)) +
    facet_wrap(~ tax.ids)+
    xlab("year") +
    ylab("report")+
    ggtitle(paste("proportion of income reported for various tax payers of interest",sep=""))
  
  
  tax.gap.dyn <- aggregated.dyn[,c("t","tax.gap")]
  tax.gap.dyn.lag <- cbind(tax.gap.dyn, tax.gap.lag=c(0,tax.gap.dyn$tax.gap[-nrow(tax.gap.dyn)]) )
  tax.gap.dyn.lag <- cbind(tax.gap.dyn.lag, tax.gap.lag2=c(0,tax.gap.dyn.lag$tax.gap.lag[-nrow(tax.gap.dyn.lag)]) )
  last.20.years <- nrow(tax.gap.dyn.lag)
  last.20.years <-c((last.20.years-75):last.20.years)
  
  iterative.map.plot <- ggplot(tax.gap.dyn.lag[last.20.years,]) +
    geom_point(aes(x=tax.gap.lag, y=tax.gap, color=t),size=3)+
    geom_line(aes(x=tax.gap.lag, y=tax.gap.lag),alpha=0.4,size=2)+
    geom_segment(aes(x = tax.gap.lag2, y = tax.gap.lag, xend = tax.gap.lag, yend = tax.gap,color=t),
                 arrow = arrow(length = unit(0.02, "npc"),type = "closed",angle=20))+
    xlab("tax gap in year t") +
    ylab("tax gap in year t+1")+
    ggtitle(paste("Tax Gap Trajectory",sep=""))
  
  melt.aggregated.dyn <- melt(aggregated.dyn,id.vars = "t",variable.name = "agg_variable", 
                              value.name = "agg_value")
  
  aggregated.dyn.plot <- ggplot(melt.aggregated.dyn)+
    geom_line(aes(x=t, y=agg_value, group= agg_variable, colour = agg_variable),size=1.5)+
    #facet_wrap(~ agg_variable)+
    xlab("year")+
    ylab("proportion")+
    ggtitle(paste("Dynamics of Aggregates",sep=""))
  
  all.plots <- list("c1.plot"=c1.plot, 
                    "w.plot"=w.plot, 
                    "report.plot"=report.plot, 
                    "per.audit.rate.plot" = per.audit.rate.plot, 
                    "per.penalty.rate.plot"=per.penalty.rate.plot, 
                    "years.since.last.compliant.plot"=years.since.last.compliant.plot,
                    "report.compliance.prob.plot"=report.compliance.prob.plot, 
                    "sample.report.trajectory.plot"=sample.report.trajectory.plot, 
                    "iterative.map.plot"=iterative.map.plot, 
                    "aggregated.dyn.plot"=aggregated.dyn.plot)
  
  return(all.plots)
  
}