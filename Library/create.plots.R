#############################################################
###                                                       ###
###       Create Plots             
###                                                       ###
#############################################################
create.plots <- function(sim.data, track.dyn, aggregated.dyn, 
                         config, g.info, last.t.years = 30, small = 14, 
                         big = 18, font.reduction.factor=0.75, label.events = F, plots.for.shiny = F)
{
  # detach("package:reshape", unload=TRUE)
  # require(reshape2)
  # track.dyn<- track.id
  # small = 14
  # big = 18
  # font.reduction.factor=0.75
  # label.events = F
  # select.trajectories <- 2
  #Correcting errors from rounding off
 # sim.data[sim.data$report > 100.00, 'report'] <- 100.00
  
  N <- length(unique(sim.data$tax.ids)) #get.config.param(config, 'population.size')
  final.year <- get.config.param(config, 'total.years')
  
  t <- dim(aggregated.dyn)[1]
  #generation.half.life <- get.config.param(config, 'generation.half.life')
  
  g <- g.info$g
  nn <- g.info$nn
  
  #############################################################
  ###  Rearrange the data and labels        
  #############################################################
  
  se.ids <- sim.data[sim.data$self.employed, 'tax.ids'] 
  se.track.dyn <- track.dyn[track.dyn$tax.ids %in% se.ids, ]
  se.focus.ids <- get.ids.for.trajectories.of.interest(se.track.dyn, t)
  se.track.dyn <- se.track.dyn[se.track.dyn$tax.ids %in% se.focus.ids, ]
  se.track.dyn$type <- ''
  se.track.dyn[se.track.dyn$audited, 'type'] <- 'Audited'
  se.track.dyn[se.track.dyn$penalized, 'type'] <- 'Penalized'
  se.track.dyn$tax.ids <- as.factor(se.track.dyn$tax.ids)
  se.focus.ids.trajectory.plot <- NULL
  
  nse.ids <- sim.data[!sim.data$self.employed, 'tax.ids']
  nse.track.dyn <- track.dyn[track.dyn$tax.ids %in% nse.ids, ]
  nse.focus.ids <- get.ids.for.trajectories.of.interest(nse.track.dyn, t)
  nse.track.dyn <- nse.track.dyn[nse.track.dyn$tax.ids %in% nse.focus.ids, ]
  nse.track.dyn$type <- ''
  nse.track.dyn[nse.track.dyn$audited, 'type'] <- 'Audited'
  nse.track.dyn[nse.track.dyn$penalized, 'type'] <- 'Penalized'
  nse.track.dyn$tax.ids <- as.factor(nse.track.dyn$tax.ids)
  nse.focus.ids.trajectory.plot <- NULL
  
  focus.ids <- c(se.focus.ids, nse.focus.ids)
  
  track.dyn.focus.ids <- track.dyn[track.dyn$tax.ids%in%focus.ids, ]
  track.dyn.focus.ids$tax.ids <- as.factor(track.dyn.focus.ids$tax.ids)
  
  tmp <- NULL
  if(any(track.dyn.focus.ids$penalized))
  {
    tmp <-cbind(track.dyn.focus.ids[track.dyn.focus.ids$audited, c("t","tax.ids")],type="audited")
    tmp <- rbind(tmp, cbind(track.dyn.focus.ids[track.dyn.focus.ids$penalized, c("t","tax.ids")],type="penalized"))
  }
  track.dyn.focus.ids.penalized <- tmp
  
  #browser()
  track.data <- track.dyn[track.dyn$t >= (t - last.t.years), ]
  track.data$per.audit.rate <-  track.data$per.audit.rate*100
  track.data$per.audit.rate <- ifelse(track.data$per.audit.rate > 100, 100, track.data$per.audit.rate)
  track.data$per.penalty.rate <- track.data$per.penalty.rate*100
  track.data$Delta.Morale = round(100*track.data$Delta.Morale, 0)
  track.data$Delta.Network = round(100*track.data$Delta.Network, 0)
  track.data$w = round(100*track.data$w, 0)
  
  track.data$report<-track.data[,"percent.reported.0.years.ago"]
  track.data$compliant.in.past.3.years <- with(track.data, as.factor(years.since.last.compliant<4))
  track.data$years.since.last.compliant <- ifelse(track.data$years.since.last.compliant > 100, 100, track.data$years.since.last.compliant)
  track.data$recently.penalized <- with(track.data, as.factor(years.since.last.penalty<4))
  track.data$penalized.3.yearsago <- with(track.data, as.factor(years.since.last.penalty==3))
  track.data$penalized.1.yearsago <- with(track.data, as.factor(years.since.last.penalty==1))
  
  
  recent.years <-round(last.t.years/5,0)
  
  track.data$compliant.group<- paste("Compliant in the last",last.t.years, "years",sep=" ")
  track.data$compliant.group[track.data$years.since.last.compliant>last.t.years] <- paste("Never Compliant in the last" ,last.t.years, "years",sep=" ")
  
  morale.half.life <- get.config.param(config, 'morale.half.life')
  track.data$compliant.group[track.data$years.since.last.compliant<recent.years ] <- paste("Compliant in the last",recent.years, "years",sep=" ")
  track.data$audit.group<- paste("Audited, but not in the last", recent.years, "years",sep=" ")
  track.data$audit.group[track.data$years.since.last.audit>last.t.years] <- paste("Never Audited in the last", last.t.years , "years",sep=" ")
  
  track.data$audit.group[track.data$years.since.last.audit<recent.years  ] <- "Recently Audited"
  track.data$pen.group<- paste("Penalized, but not in the last",recent.years, "years",sep=" ") 
  track.data$pen.group[track.data$years.since.last.penalty>last.t.years] <- 
    paste("Never Penalized in the last",last.t.years , "years",sep=" ")
  track.data$pen.group[track.data$years.since.last.penalty<recent.years ] <- "Recently Penalized"
  
  tmp <-track.data$years.since.last.penalty
  tmp[tmp>3] <- Inf
  tmp <- 1/(tmp+1)
  tmp<-as.factor(tmp)
  levels(tmp) <- c("Not Penalized within the past 3 years",
                   "Penalized 3 years ago","Penalized 2 years ago",
                   "Penalized a year ago","Penalized this year")
  track.data$penalty.lag <- tmp
  
  
  #############################################################
  ### c1 Plot        
  #############################################################
  
   #binwidth_c1 <- (max(sim.data$c1)-min(sim.data$c1))/30
   #binwidth_c1.tilde <- (max(sim.data$c1.tilde)-min(sim.data$c1.tilde))/30
  
  tmp <- reshape2::melt(sim.data, id.vars = c("c1","c1.tilde"), measure.vars=c("c1","c1.tilde"))
  labels =  c(expression(italic(c[1]^{(i)})),expression(italic(tilde(c)[1]^{(i)})))
  
  
  c1.plot <- 
    ggplot(tmp) +
    geom_histogram(boundary = 0, 
                   aes(x=value/100,y=..count../sum(..count..),fill=variable),
                   alpha=0.7,bins = 25, position = "identity")+
    scale_x_continuous(limits=c(0, 0.72), labels = scales::percent)+
    scale_y_continuous(labels = scales::percent)+
    #geom_density(aes(x=c1, y = 0.3*..count../sum(..count..)),alpha=0.7, fill="pink",color="red",size=0.1) +
    #geom_density(aes(x=c1.tilde, y = 2*..count..),alpha=0.7, fill="lightblue",color="blue",size=0.1)+
    theme_bw() +
    scale_fill_manual(name="group",values=c("red","steelblue3"),labels=labels)+
    scale_color_manual(name="group",values=c("red","steelblue3"),labels=labels)+
    xlab("Threshold Values") +
    ylab("Tax Payers") + 
    theme(panel.border = element_blank(),
               legend.title = element_blank(),
               legend.position = c(0.80,0.80),
               legend.background = element_rect(fill="transparent"),
               axis.text.x=element_text(size=small,face="italic"), 
               axis.text.y=element_text(size=small,face="italic" ) ,
               strip.text=element_text( size=small,face="italic" ) ,
               axis.title.x = element_text( size=big,face="italic" ) ,
               axis.title.y = element_text( size=big,face="italic" ),
               legend.text = element_text( size = big,face="italic"))
  #ggtitle(paste("distribution of c1 (red) and c1.tilde (blue)",sep=""))  
  
  #############################################################
  ###  W Plot      
  #############################################################
  
  w.plot <- 
    ggplot(track.data) +
    geom_histogram(boundary = 0, aes(x=w/100,y=(..count../sum(..count..)),fill = pen.group),
                   alpha=0.8, color="black",bins = 10) +
    scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::percent)+
    theme_bw() +
    xlab("Propensity to Fully Report their \n Hideable Income") +
    ylab("Tax Payers")+
    #ggtitle(paste("Histogram of propensity to fully report",sep="")) + 
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
          legend.text = element_text( size = small,face="italic"))
  
  #############################################################
  ### Report Plot      
  #############################################################
  
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
          legend.text = element_text( size = small,face="italic"))
  
  #############################################################
  ### Alter Report vs Ego report  plot 1     
  #############################################################
  
  track.data$report.cat <- as.factor(findInterval(
    track.data$percent.hideable.reported.0.years.ago, c(0,2,98)))
  levels(track.data$report.cat) <- c("Reported 0-2% of Hideable Income", 
                                     "Reported 2-98% of Hideable Income",
                                     "Reported 98-100% of Hideable Income")
  
  Delta.Network.vs.Per.plot <- 
    ggplot(track.data) +
    geom_histogram(aes(x=Delta.Network/100,(..count../sum(..count..)),
                       fill =track.data$report.cat),bins = 10,
                   alpha=0.8,color="black") +
    theme_bw() +
    guides(fill = guide_legend(title="Level of Delta.Per"))+
    #ggtitle(paste("Histogram Delta.Network for different Delta.Per",sep="")) +  
    theme_bw() +
    xlab("Alter's Average Compliance Propensity") +
    ylab("Tax Payers")+
    scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(breaks = round(seq(0, 0.50, by = 0.10),2),labels = scales::percent)+
    #ggtitle(paste("Histogram of Percent of income reported",sep=""))+ 
    scale_fill_manual(values = c( "red","darkolivegreen3", "steelblue3"))+ #"#78429E","#4F81BD"
    theme(legend.title = element_blank(),
          legend.position=c(0.5, 0.80),
          legend.background = element_rect(fill="transparent"),
          axis.text.x=element_text(size=small,face="italic" ), 
          axis.text.y=element_text(size=small,face="italic" ) ,
          panel.border = element_blank(),
          title = element_text( size=small,face="italic" ) ,
          strip.text=element_text( size=small ,face="italic") ,
          axis.title.x = element_text( size=big,face="italic" ) ,
          axis.title.y = element_text( size=big,face="italic" ),
          legend.text = element_text( size = small,face="italic"))
  
  track.data$audit.group <- as.factor(track.data$audit.group)
  track.data$audit.group <- ordered(track.data$audit.group, 
                                    levels = levels(track.data$audit.group)[3:1])
  
  
  #############################################################
  ### Alter Report vs Ego report  plot 2    
  #############################################################
  
  focused.data <- track.data[,c("Delta.Network","Delta.Morale")]
  cor.value1 <-round(100*cor(focused.data[,1],focused.data[,2]),0)
  cor.label <- paste("correlation: ",cor.value1 ,"%" ,sep="")
  
  var1.cat <- as.numeric(quantile(focused.data[,1],prob=c(0:5)/5))
  var1.cat <- round(var1.cat,0)
  var1.cat[var1.cat>10]<- round(var1.cat[var1.cat>10],-1)
  label.set.1<-paste("[",var1.cat[-length(var1.cat)],",",var1.cat[-1],")",sep="")
  label.set.1[length(label.set.1)] <- gsub( ")", "]",label.set.1[length(label.set.1)]) 
  focused.data$cat1 <- as.factor(findInterval(
    focused.data[,1], var1.cat,all.inside = T))
  levels(focused.data$cat1) <- label.set.1
  
  
  var2.cat <- as.numeric(quantile(
    focused.data[,2],
    prob=c(0:5)/5))
  var2.cat <- round(var2.cat,0)
  var2.cat[var2.cat>10]<- round(var2.cat[var2.cat>10],-1)
  label.set.2<-paste("[",var2.cat[-length(var2.cat)],",",var2.cat[-1],")",sep="")
  label.set.2[length(label.set.2)] <- gsub( ")", "]",label.set.2[length(label.set.1)]) 
  focused.data$cat2 <- as.factor(findInterval(
    focused.data[,2], var2.cat,all.inside = T))
  levels(focused.data$cat2) <- label.set.2
  
  
  focused.data.cat <- focused.data[,c("cat1","cat2")]
  #cor(as.numeric(focused.data.cat[,1]),as.numeric(focused.data.cat[,2]))
  
  focused.data.cat <- table(focused.data.cat)
  #focused.data.cat <- reshape2::melt(focused.data.cat)
  focused.data.cat <- reshape2::melt(log(focused.data.cat+1,10))
  focused.data.cat$overlap<- focused.data.cat$value/max(focused.data.cat$value)
  
  
  Alters.Ego.Raster.morale.plot <-
    ggplot() + 
    geom_tile(data = focused.data.cat, 
              aes(x=cat1, y=cat2, fill=overlap),color = "black")+
    scale_fill_gradient(high = "red",low = "yellow")+
    scale_alpha(guide = 'none')+
    xlab("Alters tax morale (%)") +
    ylab("Ego tax morale (%)") + 
    theme_bw() +
    annotate("text", x = 2.2, y = 4, label = cor.label,size=0.4*small)+
    theme(panel.border = element_blank(),
          axis.text.x=element_text(size=0.75*small,face="italic" ), 
          axis.text.y=element_text(size=0.75*small,face="italic"  ) ,
          strip.text=element_text( size=small,face="italic"  ) ,
          axis.title.x = element_text( size=big,face="italic"  ) ,
          axis.title.y = element_text( size=big,face="italic"  ),
          legend.title = element_text(size=small,face="italic" ),
          legend.text = element_text( size = 0.75*small,face="italic" ))
  
  
  #############################################################
  ### Alter Report vs Ego report  plot 3  
  #############################################################
  
  
  vars <- c("percent.hideable.reported.0.years.ago")
  functions <- c(mean)
  tmp <- track.data[track.data$t>(max(track.data$t)-last.t.years) & 
                      track.data$t<max(track.data$t), ]
  #pop.data[, 'audit'] <- pop.data[, 'audited']
  alters.mean.effects <- get.nearest.neighbours.effects(tmp, nn, vars, functions, cl=NULL)
  if(any(is.nan( alters.mean.effects))){stop("NaN produced in calling get.nearest.neighbours.effects")}
  focused.data <-cbind.data.frame(alters.mean.effects=alters.mean.effects,
                                  ego.mean.effects=tmp[,c("percent.hideable.reported.0.years.ago")])
  
  
  cor.value1 <-round(100*cor(focused.data[,1],focused.data[,2]),0)
  cor.label <- paste("correlation: ",cor.value1 ,"%" ,sep="")

  var1.cat <- as.numeric(quantile(focused.data[,1],prob=c(0:5)/5))
  if(any(duplicated(var1.cat))){
    var1.cat<- c(0,15,20,50,75,100)
  }else{
    var1.cat[4:6]<-c(50,75,100)
    var1.cat <- 5*round(var1.cat/5,0)
    var1.cat[var1.cat>10]<- 5*round(var1.cat[var1.cat>10]/5,0)
  }
  

  label.set.1<-paste("[",var1.cat[-length(var1.cat)],",",var1.cat[-1],")",sep="")
  label.set.1[length(label.set.1)] <- gsub( ")", "]",label.set.1[length(label.set.1)]) 
  #It was crashing because var1.cat was not sorted
  #So sorting it here
  var1.cat <- sort(var1.cat)
  focused.data$cat1 <- as.factor(findInterval(
    focused.data[,1], var1.cat,all.inside = T))
  levels(focused.data$cat1) <- label.set.1
  
  
  var2.cat <- as.numeric(quantile(
    focused.data[,2],
    prob=c(0:5)/5))
  if(any(duplicated(var2.cat))){
    var2.cat<- c(0,5,10,50,75,100)
  }else{
    var2.cat[2:3]<-c(5,10)
    var2.cat <- round(var2.cat,0)
    var2.cat[var2.cat>10]<- round(var2.cat[var2.cat>10],-1)
  }
  

  label.set.2<-paste("[",var2.cat[-length(var2.cat)],",",var2.cat[-1],")",sep="")
  label.set.2[length(label.set.2)] <- gsub( ")", "]",label.set.2[length(label.set.2)]) 
  focused.data$cat2 <- as.factor(findInterval(
    focused.data[,2], var2.cat,all.inside = T))
  levels(focused.data$cat2) <- label.set.2
  
  
  focused.data.cat <- focused.data[,c("cat1","cat2")]
  #cor(as.numeric(focused.data.cat[,1]),as.numeric(focused.data.cat[,2]))
  
  focused.data.cat <- table(focused.data.cat)
  #focused.data.cat <- reshape2::melt(focused.data.cat)
  focused.data.cat <- reshape2::melt(log(focused.data.cat+1,10))
  focused.data.cat$overlap<- focused.data.cat$value/max(focused.data.cat$value)
  
  
  Alters.Ego.Raster.reported.plot <-
    ggplot() + 
    geom_tile(data = focused.data.cat, 
              aes(x=cat1, y=cat2, fill=overlap),color = "black")+
    scale_fill_gradient(high = "red",low = "yellow")+
    scale_alpha(guide = 'none')+
    xlab("Alter's hideable income reported (%)") +
    ylab("Ego's hideable income\n reported (%)") + 
    theme_bw() +
    annotate("text", x = 2.2, y = 4, label = cor.label,size=0.4*small)+
    theme(panel.border = element_blank(),
          axis.text.x=element_text(size=0.75*small,face="italic" ), 
          axis.text.y=element_text(size=0.75*small,face="italic"  ) ,
          strip.text=element_text( size=small,face="italic"  ) ,
          axis.title.x = element_text( size=big,face="italic"  ) ,
          axis.title.y = element_text( size=big,face="italic"  ),
          legend.title = element_text(size=small,face="italic" ),
          legend.text = element_text( size = 0.75*small,face="italic" ))
  
  
  #############################################################
  ### Alter Report vs Ego report  plot 4  
  #############################################################
  
  assort <- get.compliance.assortativity.measure(g,track.dyn,
                            field ="hideable.reported",threshold=0.02, mixing.matrix = T)
  
  assort.tmp <- reshape2::melt(assort$mixing.matrix,id.vars=c('cat1', 'cat2'),
                     value.name = "density")
  
  colnames(assort.tmp) <- c('cat1', 'cat2','density')
  
  assort.tmp$percentage <- 100*assort.tmp$density 
  
  assort.m <-assort$assortativity.measures
  assort.m$measure <- as.character(assort.m$measure)
  assort.m$value <- round(assort.m$value,2)
  assort.label <- NULL
  for(ii in 1:nrow(assort.m)){
    assort.label<- c(assort.label,paste(assort.m[ii,],collapse="="))
  }
  assort.label <- paste(assort.label,collapse="\n")
  
  
  Assort.plot <-
    ggplot() + 
    geom_tile(data =  assort.tmp, 
              aes(x=cat1, y=cat2, fill=percentage),color = "black")+
    scale_fill_gradient(high = "red",low = "yellow")+
    scale_alpha(guide = 'none')+
    xlab("Hideable income reported (%)") +
    ylab("Hideable income\n reported (%)") + 
    theme_bw()  +
   annotate("text", x = 2.2, y = 4, label = assort.label,size=0.4*small)+
    theme(panel.border = element_blank(),
          axis.text.x=element_text(size=0.75*small,face="italic" ), 
          axis.text.y=element_text(size=0.75*small,face="italic"  ) ,
          strip.text=element_text( size=small,face="italic"  ) ,
          axis.title.x = element_text( size=big,face="italic"  ) ,
          axis.title.y = element_text( size=big,face="italic"  ),
          legend.title = element_text(size=small,face="italic" ),
          legend.text = element_text( size = 0.75*small,face="italic" ))
  
  
  #############################################################
  ### Persitancy Plot    
  #############################################################
  
  tmp <- track.data[track.data$t>(max(track.data$t)-last.t.years) & 
                      track.data$t<max(track.data$t), ]
  
  persitancy<- get.persistency.levels(tmp,persitancy.levels = c(1,2,5,10,15,20) )
  persitancy <- persitancy[,c(1,3,4)]
  persitancy <- reshape2::melt(persitancy,id.vars="persitancy.levels",
          variable.name = "compliance persistency", 
          value.name = "value" )
  #levels(persitancy$`compliance persistency`) <- c("Low","High")
  
  levels(persitancy$`compliance persistency`)<- paste(last.t.years,"years",levels(persitancy$`compliance persistency`),sep=" ")
  persitancy$persitancy.levels<- as.factor(persitancy$persitancy.levels)
  
  
  persitancy.plot <- 
    ggplot(persitancy) +
    geom_col(aes(x=persitancy.levels,
                                     y=value/100, 
                                     fill = `compliance persistency`),alpha=0.8,  
                   color="black") +
   # scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::percent)+
    theme_bw() +
    xlab("Within Range (%)") +
    ylab("Tax Payers")+
    scale_fill_manual(values = c( "red","steelblue3"))+ #"#78429E","#4F81BD"
    theme(legend.title = element_blank(),
          legend.position=c(0.30, 0.9),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small,face="italic"), 
          axis.text.y=element_text(size=small,face="italic" ) ,
          strip.text=element_text( size=small,face="italic" ) ,
          axis.title.x = element_text( size=big,face="italic" ) ,
          axis.title.y = element_text( size=big ,face="italic"),
          legend.text = element_text( size = small,face="italic"))
  
  
  
  #############################################################
  ### Perceived Audit Rate Plot    
  #############################################################
  
  track.data$audit.group <- as.factor(track.data$audit.group)
  track.data$audit.group<- ordered(  track.data$audit.group, levels = c("Recently Audited", 
                                                                        "Audited, but not in the last 6 years",
                                                                        "Never Audited in the last 30 years"))
  
  per.audit.rate.plot <- 
    ggplot(track.data) +
    geom_histogram(boundary = 0, aes(x=per.audit.rate/100,
                                     y=(..count../sum(..count..)),
                                     fill=factor(audit.group),
                                     group=factor(audit.group)),
                   position = position_dodge(width = -0.09),
                   alpha=0.9, color="black", bins = 10) +
    theme_bw() +
    xlab("Perceived Audit Rate") +
    ylab("Tax Payers")+
    scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(breaks = round(seq(0, 0.50, by = 0.10),2),labels = scales::percent)+
    #ggtitle(paste("Histogram of the per.audit.rate",sep="")) +
    scale_fill_manual(values = c( "red",  "darkolivegreen3","steelblue3"))+ #"#78429E","#4F81BD"
    theme(legend.title = element_blank(),
          legend.position=c(0.60, 0.80),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small), 
          axis.text.y=element_text(size=small ) ,
          strip.text=element_text( size=small ) ,
          axis.title.x = element_text( size=big,face="italic" ) ,
          axis.title.y = element_text( size=big,face="italic" ),
          legend.text = element_text( size = small,face="italic"))
  
  
  track.data$pen.group <- as.factor(track.data$pen.group)
  track.data$pen.group <- ordered(track.data$pen.group, 
                                    levels = levels(track.data$pen.group)[3:1])
  
  #############################################################
  ### Perceived Penalty Plot       
  #############################################################
  
  per.penalty.rate.plot <- 
    ggplot(track.data) +
    geom_histogram(boundary = 0, aes(x=per.penalty.rate, 
                      y=(..count../sum(..count..)), 
                      fill =pen.group), 
                   position = position_dodge(width = -0.25),
                   alpha=1, color="black",bins = 10) +
    theme_bw() +
    xlab("Perceived Penalty Rate (%)") +
    ylab("Tax Payers")+
    #ggtitle(paste("Histogram of the per.penalty.rate",sep="")) +
    scale_fill_manual(values = c( "red","darkolivegreen3", "steelblue3"))+ #"#78429E","#4F81BD"
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_y_continuous(limits=c(0, 0.4),breaks = round(seq(0, 0.50, by = 0.10),2),labels = scales::percent)+
    theme(legend.title = element_blank(),
          legend.position=c(0.4, 0.80),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small,face="italic" ), 
          axis.text.y=element_text(size=small,face="italic"  ) ,
          strip.text=element_text( size=small,face="italic"  ) ,
          axis.title.x = element_text( size=big,face="italic"  ) ,
          axis.title.y = element_text( size=big,face="italic"  ),
          legend.text = element_text( size = small,face="italic" ))
  
  #############################################################
  ### Years since last compliant plot       
  #############################################################
  
  track.data$years.since.last.compliant[track.data$years.since.last.compliant>
                last.t.years] <- last.t.years
  
  years.since.last.compliant.plot <- 
    ggplot(track.data) +
    geom_histogram(boundary = 0, aes(x=years.since.last.compliant, y=(..count../sum(..count..)), fill =pen.group), color="black",bins = 10) +
    theme_bw() +
    xlab("Years Since Fully Compliant") +
    ylab("Tax Payers")+
    #ggtitle(paste("Histogram years.since.last.compliant",sep="")) +
    scale_fill_manual(values = c( "red","darkolivegreen3","steelblue3"))+ #"#78429E","#4F81BD"
    scale_y_continuous(labels = scales::percent)+
    theme(legend.title = element_blank(),
          legend.position= c(0.5, 0.6),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small,face="italic" ), 
          axis.text.y=element_text(size=small,face="italic"  ) ,
          strip.text=element_text( size=small,face="italic"  ) ,
          axis.title.x = element_text( size=big,face="italic"  ) ,
          axis.title.y = element_text( size=big,face="italic"  ),
          legend.text = element_text( size = small,face="italic" ))
  
  # report.compliance.prob.plot <- ggplot(track.data) +
  #   geom_point(aes(x=w, y=report, colour = compliant.in.past.3.years,
  #                  size=penalty.lag,alpha=c1.tilde))+
  #   xlab("prob to fully report (w) this year") +
  #   ylab("proportion of income reported this year")+
  #   ggtitle(paste("proportion of income reported",sep=""))
  
  
  #############################################################
  ### Trajectory Plot 1      
  #############################################################
  
  facet.labels <- sapply(track.dyn.focus.ids$tax.ids, function(id) {
    f.lab <- paste("Tax ID:", id,sep="")
  })
  levels(track.dyn.focus.ids$tax.ids) <- facet.labels
  
  sample.report.trajectory.plot<- 
    ggplot(track.dyn.focus.ids) +
    geom_line(aes(x=t, y=hideable.reported, group= tax.ids, colour = tax.ids),size=1.5) +
    facet_wrap(~ tax.ids, nrow = 2)+
    xlab("Year") +
    ylab("Hideable Income Reported (%)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.text.x=element_text(size=small*font.reduction.factor,angle=35,face="italic" ), 
          axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
          strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
          axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
          axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
          legend.text = element_text( size = small*font.reduction.factor,face="italic" ),
          legend.position="none")
  #ggtitle(paste("proportion of income reported for various tax payers of interest",sep=""))
  
  if(!is.null(track.dyn.focus.ids.penalized) & label.events)
  {
    sample.report.trajectory.plot <- sample.report.trajectory.plot + 
      geom_vline(data =track.dyn.focus.ids.penalized, aes(xintercept = t, colour= type, linetype=type)) 
  }
  
  #############################################################
  ### Trajectory Plot  of Self employed     
  #############################################################
  
  # se.facet.labels <- sapply(se.track.dyn$tax.ids, function(id) {
  #   f.lab <- paste("Tax ID:", id, "| Hideable Income =", 
  #                  round(se.track.dyn[id, 'perc.hideable.income'], 2), "%")
  # })
  # levels(se.track.dyn$tax.ids) <- se.facet.labels
  
  se.facet.labels <- sapply(se.track.dyn$tax.ids, function(id) {
    f.lab <- paste("Self-employed | ","Tax ID:", id,sep="")
  })
  levels(se.track.dyn$tax.ids) <- se.facet.labels
  
  se.focus.ids.trajectory.plot <- 
    ggplot(se.track.dyn) +
    geom_line(aes(x=t, y=hideable.reported, group= tax.ids, colour = tax.ids),size=1.5) +
    facet_wrap(~ tax.ids, nrow = 2)+
    xlab("Year") +
    ylab("Hideable Income Reported (%)") + 
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position="none",
          panel.border = element_blank(),
          axis.text.x=element_text(size=small*font.reduction.factor,angle=35,face="italic" ), 
          axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
          strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
          axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
          axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
          legend.text = element_text( size = small*font.reduction.factor,face="italic" )) + 
    geom_vline(data = se.track.dyn[se.track.dyn$type != '', ], 
               aes(xintercept = t, color = type, linetype = type),size=0.75)+
    scale_linetype_manual(values=c("dotdash","solid"))+
    scale_color_manual(values=c( "pink","gray60",
                                "steelblue3","darkolivegreen3",
                                "purple", "navy"))
  
  if(any(se.track.dyn$type !='') & label.events) {
    se.focus.ids.trajectory.plot <- se.focus.ids.trajectory.plot + 
      geom_text(data=se.track.dyn[se.track.dyn$type !='', ], check_overlap = T,
                mapping=aes(x=t, y=0, label=type), size=3, angle=90, vjust=-0.4, hjust=0)
  }

  #############################################################
  ### Trajectory Plot of Non-self employed     
  #############################################################  
  
  
  # nse.facet.labels <- sapply(nse.track.dyn$tax.ids, function(id) {
  #   f.lab <- paste("Tax ID:", id, "| Hideable Income =", round(nse.track.dyn[id, 'perc.hideable.income'], 2), "%")
  # })
  nse.facet.labels <- sapply(nse.track.dyn$tax.ids, function(id) {
    f.lab <- paste("Salaried-employed | Tax ID:", id, sep=" ")
  })
  levels(nse.track.dyn$tax.ids) <- nse.facet.labels
  
  nse.focus.ids.trajectory.plot <- 
    ggplot(nse.track.dyn) +
    geom_line(aes(x=t, y=hideable.reported, group= tax.ids, colour = tax.ids),size=1.5) +
    facet_wrap(~ tax.ids, nrow = 2)+
    xlab("Year") +
    ylab("Hideable Income Reported (%)") + 
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position="none",
          panel.border = element_blank(),
          axis.text.x=element_text(size=small*font.reduction.factor,angle=35,face="italic" ), 
          axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
          strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
          axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
          axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
          legend.text = element_text( size = small*font.reduction.factor,face="italic" )) +
    geom_vline(data = nse.track.dyn[nse.track.dyn$type != '', ], 
               aes(xintercept = t, color = type, linetype = type),size=0.75)+
    scale_linetype_manual(values=c("dotdash","solid"))+
    scale_color_manual(values=c( "pink","gray60",
                                 "steelblue3","darkolivegreen3",
                                 "purple", "navy"))
  
  if(any(nse.track.dyn$type !='') & label.events) {
    nse.focus.ids.trajectory.plot <- nse.focus.ids.trajectory.plot + 
      geom_text(data=nse.track.dyn[nse.track.dyn$type !='', ], check_overlap = T, 
                mapping=aes(x=t, y=0, label=type), size=3, angle=90, vjust=-0.4, hjust=0)
  }
  

  #############################################################
  ### Trajectory Plot of Non-self employed     
  #############################################################  
  
  tax.gap.dyn <- aggregated.dyn[,c("t","tax.gap")]
  tmp<- tax.gap.dyn$tax.gap[-nrow(tax.gap.dyn)]
  tax.gap.dyn.lag <- cbind(tax.gap.dyn, tax.gap.lag=c(tmp[1],tmp) )
  tmp <- tax.gap.dyn.lag$tax.gap.lag[-nrow(tax.gap.dyn.lag)]
  tax.gap.dyn.lag <- cbind(tax.gap.dyn.lag, tax.gap.lag2=c(tmp[1],tmp) )
  last.100.years <- nrow(tax.gap.dyn.lag)
  last.100.years <-c((last.100.years-100):last.100.years)
  
  iterative.map.plot <- 
    ggplot(tax.gap.dyn.lag[last.100.years,]) +
    geom_point(aes(x=tax.gap.lag, y=tax.gap, color=t,alpha=t),size=1)+
    geom_segment(aes(x = tax.gap.lag2, y = tax.gap.lag, 
                     xend = tax.gap.lag, yend = tax.gap,color=t,alpha=t),
                 arrow = arrow(length = unit(0.03, "npc"),
                               type = "closed",angle=20))+
    geom_line(aes(x=tax.gap.lag, y=tax.gap.lag),alpha=0.9,
              color="black",size=1,linetype="dotdash")+
    scale_color_gradient(high = "#132B43", low = "#56B1F7")+
    scale_alpha(guide = 'none')+
    scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::percent)+
    xlab("Previous year's Tax Gap") +
    ylab("Tax Gap") + theme_bw() + labs(color = "Year") + 
    theme(panel.border = element_blank(),
        axis.text.x=element_text(size=small,face="italic" ), 
        axis.text.y=element_text(size=small,face="italic"  ) ,
        strip.text=element_text( size=small,face="italic"  ) ,
        axis.title.x = element_text( size=big,face="italic"  ) ,
        axis.title.y = element_text( size=big,face="italic"  ),
        legend.title = element_text(size=small,face="italic" ),
        legend.text = element_text( size = 0.75*small,face="italic" ))
  #ggtitle(paste("Tax Gap Trajectory",sep=""))
  
  
  #############################################################
  ### aggegated plots  
  ############################################################# 
  
  agg.dyn <- aggregated.dyn[, c('t', 'tax.gap', 'mean.per.audit.rate', 'mean.per.penalty.rate')]
  agg.dyn[, c('tax.gap', 'mean.per.audit.rate', 'mean.per.penalty.rate')] <- agg.dyn[, c('tax.gap', 'mean.per.audit.rate', 'mean.per.penalty.rate')] 
  names(agg.dyn) <- c("t", "Tax Gap", "Mean Perceived Audit Rate", "Mean Perceived Penalty Rate")
  melt.aggregated.dyn <- reshape2::melt(agg.dyn,id.vars = "t",variable.name = "agg_variable", 
                              value.name = "agg_value")
  
  melt.aggregated.dyn$agg_variable <- as.factor(melt.aggregated.dyn$agg_variable)
  melt.aggregated.dyn$agg_variable <- ordered(melt.aggregated.dyn$agg_variable, 
                                    levels = levels(melt.aggregated.dyn$agg_variable)[3:1])
  
  aggregated.dyn.plot <- 
    ggplot(melt.aggregated.dyn)+
    geom_line(aes(x=t, y=agg_value, group= agg_variable, colour = agg_variable),size=1.5)+
    #facet_wrap(~ agg_variable)+
    xlab("Year")+
    ylab("Percentage")+
    scale_y_continuous(labels = scales::percent)+
    scale_color_manual(values = c( "steelblue3","darkolivegreen3","red"))+ 
    theme_bw() + theme(legend.title = element_blank(),
                       legend.position=c(0.5, 0.70),
                       legend.background = element_rect(fill="transparent"), 
                       panel.border = element_blank(),
                       axis.text.x=element_text(size=small,face="italic"), 
                       axis.text.y=element_text(size=small,face="italic" ) ,
                       strip.text=element_text( size=small,face="italic" ) ,
                       axis.title.x = element_text( size=big,face="italic" ) ,
                       axis.title.y = element_text( size=big,face="italic" ),
                       legend.text = element_text( size = small,face="italic"))
  #ggtitle(paste("Dynamics of Aggregates",sep=""))
  
  #Network plot
  #browser()
  t <- final.year
  #net.plots <- net.plot.at.time.t(track.dyn, g, t)
  #net.plot <- net.plots[[1]]
  net.plot <- NULL
  
  #############################################################
  ###                                                       ###
  ###       Track output            
  ###                                                       ###
  #############################################################
  
  #print(t(sim.data[1:10, 1:15]))
  all.plots <- list("c1.plot"=c1.plot, 
                    "w.plot"=w.plot, 
                    "report.plot"=report.plot,
                    "per.audit.rate.plot" = per.audit.rate.plot, 
                    "per.penalty.rate.plot"=per.penalty.rate.plot, 
                    "years.since.last.compliant.plot"=years.since.last.compliant.plot,
                    "Delta.Network.vs.Per.plot" = Delta.Network.vs.Per.plot,
                    "Alters.Ego.Raster.morale.plot"=Alters.Ego.Raster.morale.plot,
                    "Alters.Ego.Raster.reported.plot"=Alters.Ego.Raster.reported.plot,
                    "Assort.plot"= Assort.plot,
                    "persitancy.plot" = persitancy.plot,
                    "sample.report.trajectory.plot"=sample.report.trajectory.plot,
                    "se.focus.ids.trajectory.plot" = se.focus.ids.trajectory.plot,
                    "nse.focus.ids.trajectory.plot" = nse.focus.ids.trajectory.plot,
                    "iterative.map.plot"=iterative.map.plot, 
                    "aggregated.dyn.plot"=aggregated.dyn.plot)
  
  return(all.plots)
  
}