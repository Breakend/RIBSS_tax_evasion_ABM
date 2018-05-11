EC.tax.visNetwork.plot <-function(dyn.pop.data,df.vis,g,
                               group1=NULL,
                               group2=NULL,
                               id.to.focus.on=224,
                               group.cuts=c(0,5,50,95,100),
                               edge.width.based.on.compliance=FALSE,
                               broswerble=FALSE){ ## exponents were 5 and 2 before. 
  
  require(igraph)
  require(visNetwork)
  require(RColorBrewer)
  require(intergraph)
  require(dplyr)
  require(grDevices)
  require(htmltools)
  
  
  n.groups <- length(group.cuts)-1
  
  alters<- as.numeric(unlist(g.info$nn.int[id.to.focus.on]))
  second.alters<-as.numeric(unlist(lapply(alters,FUN=function(x) g.info$nn.int[x])))
  focus.nodes <-c(id.to.focus.on,alters, second.alters)
  
  dyn.pop.data <- dyn.pop.data[dyn.pop.data$tax.ids%in%focus.nodes,]
  compliant.level<- cut(dyn.pop.data$percent.hideable.reported.0.years.ago,
                        group.cuts,include.lowest = T)
  
  EC.g.vis <- df.vis
  
  #############################################################
  ### Modify Node attributes
  #############################################################
  
  EC.g.vis$nodes <- EC.g.vis$nodes[EC.g.vis$nodes$id%in%focus.nodes,]
  EC.g.vis$nodes$compliant.level <- compliant.level
  EC.g.vis$nodes$color <- as.factor(EC.g.vis$nodes$compliant.level)
  v.col <- heat.colors(length(levels(EC.g.vis$nodes$color)),alpha=1)
  #col <- colorRampPalette(brewer.pal(10, "RdYlBu")) ( length(levels(EC.g.vis$nodes$color)) )
  #col<- colorRampPalette(c("red", "yellow"))( length(levels(EC.g.vis$nodes$color)) )
  levels(EC.g.vis$nodes$color) <- v.col
  EC.g.vis$nodes$color <- as.character(EC.g.vis$nodes$color)
  
  EC.g.vis$nodes$size <- 12
  EC.g.vis$nodes$size[EC.g.vis$nodes$id%in%id.to.focus.on] <- 30
  EC.g.vis$nodes$size[EC.g.vis$nodes$id%in%alters] <- 18
  
  
  #############################################################
  ### Modify Edge attributes 
  #############################################################
  
  EC.g.vis$edges <- EC.g.vis$edges[EC.g.vis$edges$from%in%focus.nodes & 
                                     EC.g.vis$edges$to%in%focus.nodes,]
  
  
  e.level.from <- as.numeric(EC.g.vis$nodes$compliant.level[match(EC.g.vis$edges$from,EC.g.vis$nodes$id)])
  e.level.to <- as.numeric(EC.g.vis$nodes$compliant.level[match(EC.g.vis$edges$to,EC.g.vis$nodes$id) ])
  e.level <- e.level.from+e.level.to-1
  e.col <- heat.colors(2*n.groups-1,alpha=0.8)
  e.col <- colorRampPalette(c("red", "yellow"))( 2*n.groups-1 )
  e.width <- c(( 2*n.groups-1 ):1)/2
  
  EC.g.vis$edges$color<-e.col[e.level]
  
  EC.g.vis$edges$width <-3
  EC.g.vis$edges$width[EC.g.vis$edges$from%in%id.to.focus.on | 
                         EC.g.vis$edges$to%in%id.to.focus.on] <- 5
 
   if(edge.width.based.on.compliance){
    EC.g.vis$edges$width<- e.width[e.level]
  }

  
  #############################################################
  ### Modify Appearence based on Audit or Panalty 
  #############################################################
  
  enf.size <- 1.5*max(EC.g.vis$nodes$size)
  if(!is.null(group1)){
    EC.g.vis$nodes$shape[dyn.pop.data[,group1]] <- "star"
    EC.g.vis$nodes$color[dyn.pop.data[,group1]] <- "#00FF33"
    EC.g.vis$nodes$size[dyn.pop.data[,group1]] <-  enf.size 
  }
  
  if(!is.null(group2)){
    EC.g.vis$nodes$shape[dyn.pop.data[,group2]] <- "dot"
    EC.g.vis$nodes$color[dyn.pop.data[,group2]] <- "#fffff0"
    EC.g.vis$nodes$size[dyn.pop.data[,group2]] <- enf.size 
  }
  
  #############################################################
  ### Render the Network Plot
  #############################################################
  
  
  nn.alpha<-visNetwork(EC.g.vis$nodes, EC.g.vis$edges, 
                       height = "250pt",width="250pt") %>%
    visNodes(
      x=EC.g.vis$nodes$x, y= EC.g.vis$nodes$y, 
      label= NULL, physics=T, fixed=F ) %>%
    visEdges( hoverWidth =1)%>%
    visInteraction(navigationButtons = F, hover = T)%>% 
    visOptions(#selectedBy= "income.cat", 
      manipulation =FALSE, 
      #nodesIdSelection = TRUE, 
      highlightNearest = list(enabled = F,hover= T, degree = 1),
      collapse = list(enabled=T, fit=T),
      autoResize = T, clickToUse = FALSE)
  
  if(broswerble){
    nn.alpha<-browsable(
      tagList(
        tags$head(
          tags$style('div.vis-network{background-color: black;}')  
        ),
        nn.alpha
      )
    )}
  
  return(nn.alpha)
}