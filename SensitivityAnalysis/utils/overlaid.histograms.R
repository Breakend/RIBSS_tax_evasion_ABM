

overlaid.histograms <- function(all.cases, top.n.cases, var.name, 
                                add.target = F, target.value = NULL, ht = 0.5, offset = 2.5, 
                                small = 20, big = 26) {
  
  d <- data.frame(x = c(all.cases[, var.name], all.cases[1:top.n.cases, var.name]),
                  type = rep(c("All Cases", paste("Top", top.n.cases, "Cases")), c(dim(all.cases)[1], top.n.cases)))
  
  full.names <- get.full.names()
  full.var.name <- full.names[var.name]
  o.plot <- ggplot(d) + 
            #geom_histogram(aes(x=x, y = ..density.., colour=type, fill = type), alpha=0.75,color="pink",binwidth = 2) +
            geom_density(aes(x=x, y = ..density.., colour=type, fill = type), alpha=0.75,color="pink") + 
            theme_bw() +
            xlab(full.var.name) +
            ylab("Density") + theme(panel.border = element_blank(),
                                    legend.title = element_blank(),
                                    legend.position = "bottom",
                                    axis.text.x=element_text(size=small), 
                                    axis.text.y=element_text(size=small ) ,
                                    strip.text=element_text( size=small ) ,
                                    axis.title.x = element_text( size=big ) ,
                                    axis.title.y = element_text( size=big ),
                                    legend.text = element_text( size = small))
  
  if(add.target) {
    if(is.null(target.value)) {
      targets <- read.csv("Outputs/Calibration Targets/targets.csv", stringsAsFactors = F)
      idx <- which(targets$var == var.name)
      if(length(idx) > 0) 
        target.value <- targets$value[idx]
      else 
        print("No target value found or supplied")
    }
    
    if(!is.null(target.value))
      o.plot <- o.plot + geom_vline(xintercept = target.value) + 
        annotate("text", x=target.value + offset, y=ht, label= "Target", size = 0.2*small)
    
  }
  
  return(o.plot)
}
