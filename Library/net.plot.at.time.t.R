#Creates network plot at time t using the igraph object 'g'

net.plot.at.time.t <- function(track.dyn, g, t)
{
  #visual.layout<-layout.kamada.kawai(g)
  visual.layout<-layout.lgl(g)
  visual.layout<-as.data.frame(visual.layout)
  colnames(visual.layout)<-c("x","y")
  visual.layout$tax.ids <- unique(track.dyn$tax.ids)
  segments.table.edges<-get.segments.table.edges(g,visual.layout)

  net.plots <- list()
  for(i in t)
  {
    time.frame.att <- track.dyn[track.dyn$t == i, ] 
    time.frame.att <- merge(visual.layout,time.frame.att,by="tax.ids")
    time.frame.att$size <- as.numeric(time.frame.att$audited) + as.numeric(time.frame.att$penalized)
    time.frame.att$event <- as.factor(time.frame.att$size)
    levels(time.frame.att$event) <- c("no action", "audited","penalized")
    time.frame.att$alpha <- pmin(1,time.frame.att$w+
                                   as.numeric(time.frame.att$audited)+
                                   as.numeric(time.frame.att$penalized))
    time.segments.table.edges<- get.edge.property(time.frame.att,segments.table.edges)
    
    ind <- as.character(i)
    net.plots[[ind]] <- ggplot(time.frame.att,aes(x,y))+
      geom_point(aes(fill= compliant,
                     alpha = alpha,
                     size = size, 
                     shape =event))+
      scale_alpha(range = c(0.4, 1))+
      scale_size(range = c(4, 6))+
      scale_shape_manual(values = c(21,22,23))+
      guides(fill = guide_legend("Compliant"))+
      guides(shape = guide_legend("Event"))+
      guides(size = FALSE)+
      guides(alpha = guide_legend("Proportion Reported"))+
      theme(axis.text.x=element_blank(),#axis.title.x=element_blank(),
            axis.text.y=element_blank(),#axis.title.y=element_blank(),
            title = element_text( size=16 ) ,
            strip.text=element_text( size=12 ) ,
            axis.title.x = element_text( size=14 ) ,
            axis.title.y = element_text( size=14 ),
            legend.text = element_text( size = 16),
            axis.ticks=element_blank())+ 
      guides(colour = guide_legend(override.aes = list(size=10)))+
      geom_segment(aes(x=A.x, y=A.y, xend = B.x, yend = B.y,color=edge.property),
                   data=time.segments.table.edges, size = 0.7, alpha=0.08)+
      scale_fill_manual(values = c("red","lightblue"))+
      scale_color_manual(values = c("blue","red","green"))
  }
  
  return(net.plots)
}


save.net.plot.at.time.t <- function(track.dyn, g, t, img.format = "png")
{
  visual.layout<-layout.kamada.kawai(g)
  visual.layout<-as.data.frame(visual.layout)
  colnames(visual.layout)<-c("x","y")
  visual.layout$tax.ids <- unique(track.dyn$tax.ids)
  segments.table.edges<-get.segments.table.edges(g,visual.layout)
  
  net.plots.paths <- list()
  for(i in t)
  {
    time.frame.att <- track.dyn[track.dyn$t == i, ] 
    time.frame.att <- merge(visual.layout,time.frame.att,by="tax.ids")
    time.frame.att$size <- as.numeric(time.frame.att$audited) + as.numeric(time.frame.att$penalized)
    time.frame.att$event <- as.factor(time.frame.att$size)
    levels(time.frame.att$event) <- c("no action", "audited","penalized")
    time.frame.att$alpha <- pmin(1,time.frame.att$w+
                                   as.numeric(time.frame.att$audited)+
                                   as.numeric(time.frame.att$penalized))
    time.segments.table.edges<- get.edge.property(time.frame.att,segments.table.edges)
    
    ind <- as.character(i)
    fname <- tempfile(fileext = paste0(".", img.format))
    #fpath <- fname
    net.plots.paths[[ind]] <- fname
    
    net.plot <- ggplot(time.frame.att,aes(x,y))+
      geom_point(aes(fill= compliant,
                     alpha = alpha,
                     size = size, 
                     shape =event))+
      scale_alpha(range = c(0.4, 1))+
      scale_size(range = c(4, 6))+
      scale_shape_manual(values = c(21,22,23))+
      guides(fill = guide_legend("Compliant"))+
      guides(shape = guide_legend("Event"))+
      guides(size = FALSE)+
      guides(alpha = guide_legend("Proportion Reported"))+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            title = element_text( size=16 ) ,
            strip.text=element_text( size=12 ) ,
            axis.title.x = element_text( size=14 ) ,
            axis.title.y = element_text( size=14 ),
            legend.text = element_text( size = 16),
            axis.ticks=element_blank())+ 
      guides(colour = guide_legend(override.aes = list(size=10)))+
      geom_segment(aes(x=A.x, y=A.y, xend = B.x, yend = B.y,color=edge.property),
                   data=time.segments.table.edges, size = 0.7, alpha=0.08)+
      scale_fill_manual(values = c("red","lightblue"))+
      scale_color_manual(values = c("blue","red","green"))
    
    ggsave(fname, plot = net.plot, dpi=100, width = 4, height = 4)
  }
  
  return(net.plots.paths)
}