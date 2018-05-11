plot.compliance.network <-function(g,frame){
  
  require(ggplot2)
  require(igraph)
  
  time.frame.att<- frame
  
  visual.layout<-layout.kamada.kawai(g)
  visual.layout<-as.data.frame(visual.layout)
  colnames(visual.layout)<-c("x","y")
  visual.layout$tax.ids <- c(1:nrow(visual.layout))
  segments.table.edges<-get.segments.table.edges(g,visual.layout)
  
  time.frame.att <- merge(visual.layout,time.frame.att,by="tax.ids")
  
  time.frame.att$size <- as.numeric(time.frame.att$audit)+
    as.numeric(time.frame.att$penalized)
  
  time.frame.att$event <- as.factor(time.frame.att$size)
  levels(time.frame.att$event) <- c("no action", "audit","penalized")
  
  
  time.frame.att$alpha <- pmin(1,time.frame.att$w+
                                 as.numeric(time.frame.att$audit)+
                                 as.numeric(time.frame.att$penalized))
  
  time.segments.table.edges<- get.edge.property(time.frame.att,segments.table.edges)
  
  time.segments.table.edges$w<- as.numeric(time.segments.table.edges$edge.property)
  
  tmp <-time.segments.table.edges$edge.property
  
  time.segments.table.edges$w[tmp=="assor.compliant"] <- 1
  time.segments.table.edges$w[tmp=="disassortative"] <- 6
  time.segments.table.edges$w[tmp=="assor.evaders"] <- 2
  
  g <- set.edge.attribute(g, "weight", value=time.segments.table.edges$w)
  
  visual.layout<-layout.kamada.kawai(g)
  #visual.layout<-layout.reingold.tilford(g, circular=T)
  #visual.layout<- layout.fruchterman.reingold(g)
  #visual.layout<- layout.lgl(g)
  #visual.layout<- layout_with_graphopt(g)
  visual.layout<-as.data.frame(visual.layout)
  colnames(visual.layout)<-c("x","y")
  visual.layout$tax.ids <- c(1:nrow(visual.layout))
  segments.table.edges<-get.segments.table.edges(g,visual.layout)
  
  
  time.frame.att<- frame
  time.frame.att <- merge(visual.layout,time.frame.att,by="tax.ids")
  
  time.frame.att$size <- as.numeric(time.frame.att$audit)+
    as.numeric(time.frame.att$penalized)
  
  time.frame.att$event <- as.factor(time.frame.att$size)
  levels(time.frame.att$event) <- c("no action", "audit","penalized")
  
  time.frame.att$alpha <- pmin(1,time.frame.att$w+
                                 as.numeric(time.frame.att$audit)+
                                 as.numeric(time.frame.att$penalized))
  
  time.segments.table.edges<- get.edge.property(time.frame.att,segments.table.edges)
  
  p <- ggplot(time.frame.att,aes(x,y))+
    geom_segment(aes(x=A.x, y=A.y, xend = B.x, yend = B.y,color=edge.property),
                 data=time.segments.table.edges, size = 0.8, alpha=0.10)+
    scale_fill_manual(values = c("red","lightblue"))+
    scale_color_manual(values = c("blue","red","green"))+
    guides(color = guide_legend("Edge Property",override.aes=list(alpha=0.7)))+
    geom_point(aes(fill= compliant,
                   alpha = alpha,
                   size = size, 
                   shape =event))+
    scale_alpha(range = c(0.3, 1))+
    scale_size(range = c(4, 7))+
    scale_shape_manual(values = c(21,22,23))+
    guides(fill = guide_legend("Compliant",override.aes=list(size=5,shape=21)))+
    guides(shape = guide_legend("Event",override.aes=list(size=5)))+
    guides(size = FALSE)+
    guides(alpha = guide_legend("Proportion Reported"))+
    theme(axis.text.x=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),axis.title.y=element_blank(),
          title = element_text( size=16 ) ,
          strip.text=element_text( size=12 ) ,
          axis.title.x = element_text( size=14 ) ,
          axis.title.y = element_text( size=14 ),
          legend.text = element_text( size = 16),
          axis.ticks=element_blank())
  
  return(p)
  
}