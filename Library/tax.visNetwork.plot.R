tax.visNetwork.plot <-function(dyn.pop.data,df.vis,
                               alpha.node.attribute, 
                               alpha.node.exponent=2,
                               alpha.edge.exponent=1,
                               alpha.edge.factor=1,
                               group1=NULL,
                               group2=NULL,
                               broswerble=FALSE){ ## exponents were 5 and 2 before. 
  
  require(igraph)
  require(visNetwork)
  require(RColorBrewer)
  require(intergraph)
  require(dplyr)
  require(grDevices)
  require(htmltools)
  
  
  col.tmp <- df.vis$nodes$color
  alpha.adj <- (pmax(dyn.pop.data[,alpha.node.attribute],0))^alpha.node.exponent

  df.vis$nodes$color <- mapply(FUN=function(x,y) {
    adjustcolor( x , alpha.f = y)
  },col.tmp,alpha.adj )
  
  if(!is.null(group1)){
    df.vis$nodes$shape[dyn.pop.data[,group1]] <- "star"
    df.vis$nodes$color[dyn.pop.data[,group1]] <- "#00FF33"
    df.vis$nodes$size[dyn.pop.data[,group1]] <-  5
  }
  
  if(!is.null(group2)){
    df.vis$nodes$shape[dyn.pop.data[,group2]] <- "dot"
    df.vis$nodes$color[dyn.pop.data[,group2]] <- "#fffff0"
    df.vis$nodes$size[dyn.pop.data[,group2]] <- 6
  }
  
  edge.alpha.adj<-cbind(alpha.adj[df.vis$edges$from],alpha.adj[df.vis$edges$to])
  edge.alpha.adj<- alpha.edge.factor*pmax((apply(edge.alpha.adj,1,min)),0)^alpha.edge.exponent
  # edge.alpha.adj<- 0.2*edge.alpha.adj/median(edge.alpha.adj)
  # df.vis$edges$color <- "red"
  df.vis$edges$color <- mapply(FUN=function(x,y) {
    adjustcolor( x , alpha.f = y)
  },"pink", edge.alpha.adj )
  
  
  nn.alpha<-visNetwork(df.vis$nodes, df.vis$edges, 
                       height = "380pt",width="380pt") %>%
    visNodes(color = list(highlight = "yellow"),
             x=df.vis$nodes$x, y= df.vis$nodes$y, 
             label= NULL, physics=F, fixed=T ) %>%
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