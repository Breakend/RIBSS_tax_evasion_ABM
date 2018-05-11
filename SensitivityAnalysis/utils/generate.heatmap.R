generate.heatmap <- function(dat.realization.avg, input.fields, main.outputs, 
                             which.fit = "both", white.washers = 4, top.how.many = 6,
                             small = 12, 
                             big = 16, font.reduction.factor=0.75,
                             plots.dir = "SensitivityAnalysis/figures/sensitivity/",
                             do.other.plots=F
                             ) {
  
  
  dat.realization.avg<- sdat.realization.avg
  which.fit = "both"
  white.washers = 4
  top.how.many = 6
  small = 12
  big = 16
  font.reduction.factor=0.75
  plots.dir = "SensitivityAnalysis/figures/sensitivity/"
  do.other.plots=F
 
  require(ppcor)
  ### get pair-wise patial rank correlations
  pprc<-pcor(dat.realization.avg[,c(input.fields,main.outputs)])
  pprc.values <- pprc$estimate
  rownames(pprc.values) <-colnames(pprc.values) <-c(input.fields,main.outputs) 
  pprc.values<-pprc.values[input.fields,main.outputs]
  
  p.value<-pprc$p.value
  rownames(p.value) <-colnames(p.value) <-c(input.fields,main.outputs) 
  p.value<-p.value[input.fields,main.outputs]
  p.value.logical <- p.value<=0.01
  
  cor.mat<-cor(dat.realization.avg[,c(input.fields,main.outputs)])
  cor.mat<-cor.mat[input.fields,main.outputs]
  
  pprc.melt<-merge(melt(pprc.values),melt(p.value.logical),by=c("Var1","Var2"))
  pprc.melt<- merge(pprc.melt,melt(cor.mat),by=c("Var1","Var2"))
  colnames(pprc.melt)<-c("model.inputs","variable","pprc","significant","cor")
  
  
  overall.score.plot <- NULL
  #Backing this up, because sometimes there are not enough no. of elements, so we will be updating the term top.how.many
  top.how.many.original <- top.how.many
  
  model.input.heatmap.data <- NULL
  for(field in main.outputs)
  {
    #retval <- sensitivity.to.model.parameters.plots(field, dat.realization.avg, full.name = all.io.names[field], params = input.fields)
    #browser()
    retval <- sensitivity.to.model.inputs.plots(field, dat.realization.avg, 
                                                plots.dir = plots.dir, 
                                                which.fit = which.fit, 
                                                full.name = all.io.names[field], 
                                                params = input.fields, top.how.many)
    overall.score.plot <- retval$ind.sens.plot
    overall.sens.dat <- retval$ind.sens.dat
    overall.sens.dat$Per.CART.Imp <- NULL
    overall.sens.dat$Per.RF.Imp <- NULL
    rownames(overall.sens.dat) <- NULL
    colnames(overall.sens.dat) <- c('model.inputs', field)
    if(is.null(model.input.heatmap.data))
    {
      model.input.heatmap.data <- overall.sens.dat
    } else {
      model.input.heatmap.data <- merge(model.input.heatmap.data, overall.sens.dat, by="model.inputs")
    }
  }
  
  
  ###############################
  ## First two and the rest combined
  ###############################
  model.input.heatmap.data[, 'total.sensitivity'] <- apply(model.input.heatmap.data[, -1], 1, mean)
  model.input.heatmap.data <- model.input.heatmap.data[order(model.input.heatmap.data$total.sensitivity, decreasing = T), ]
  model.input.heatmap.data$total.sensitivity <- NULL
  
  n.inputs <- dim(model.input.heatmap.data)
  if(white.washers > 0) {
    white.washers.and.rest <- model.input.heatmap.data[1:white.washers, ]
    others.combined <- apply(model.input.heatmap.data[-(1:white.washers), -1], 2, sum)
  } else {
    white.washers.and.rest <- model.input.heatmap.data
    others.combined <- apply(model.input.heatmap.data[, -1], 2, sum)
  }
  
  white.washers.and.rest$model.inputs <- as.character(white.washers.and.rest$model.inputs)
  others.combined <- c(model.inputs = 'other.inputs.combined', others.combined)
  white.washers.and.rest <- rbind(white.washers.and.rest, others.combined)
  white.washers.and.rest$model.inputs <- factor(white.washers.and.rest$model.inputs, levels = white.washers.and.rest$model.inputs)
  white.washers.and.rest[, -1] <- apply(white.washers.and.rest[, -1], 2, as.numeric)
  white.washers.and.rest[, -1] <- white.washers.and.rest[, -1]/max(white.washers.and.rest[, -1])
  melt.white.washers.and.rest <- melt(white.washers.and.rest, id = "model.inputs")
  
  mi.names <- as.character(unique(melt.white.washers.and.rest$model.inputs))
  mi.names <- mi.names[!(mi.names %in% derived.and.point.params.names)]
  ou.names <- as.character(unique(melt.white.washers.and.rest$variable))
  color.palette <- c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c')
  
  
  melt.white.washers.and.rest <- merge(melt.white.washers.and.rest,pprc.melt,
                                       by = c("model.inputs","variable"),all.x = T)
  melt.white.washers.and.rest$ast <- ""
  melt.white.washers.and.rest$ast[melt.white.washers.and.rest$significant] <-"*"
  
  heatmap.white.washers.and.rest <- 
    ggplot(melt.white.washers.and.rest) + 
    geom_tile(aes(x=model.inputs, y = variable, fill=value),color="black")  +
    guides(fill = guide_legend(title="Overall Importance"))+
    scale_fill_gradientn(colours=color.palette) +
    #scale_fill_gradientn(colours = myPalette(6)) +
    ylab("Important Outputs")+ xlab("Model Inputs") +
    geom_text(vjust=-0.,size=6,aes(x=model.inputs, y = variable, 
                  label = round(value, 2)
                  #label= paste(round(value, 2),"\n",round(pprc,2))
                  ))+
    geom_text(vjust=1.2,size=5,color= "navy",aes(x=model.inputs, y = variable, 
                             label = paste(round(pprc, 2),ast, sep = "")
    )) +
    #ggtitle("Heatmap of Model Inputs vs. Model Outputs") + 
    scale_x_discrete(labels = str_wrap(all.io.names[mi.names], width = 15)) + 
    scale_y_discrete(labels = str_wrap(all.io.names[ou.names], width = 15))+
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position="none",
          panel.border = element_blank(),
          axis.text.x=element_text(size=small*font.reduction.factor,angle=0,face="italic" ), 
          axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
          strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
          axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
          axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
          legend.text = element_text( size = small*font.reduction.factor,face="italic" ))
  
  
  
  ###############################
  ## Only the rest
  ###############################
  if(white.washers > 0) {
    top.model.input.heatmap.data <- model.input.heatmap.data[-(1:white.washers), ]
  } else {
    top.model.input.heatmap.data <- model.input.heatmap.data
  }
  top.model.input.heatmap.data[, 'row.means'] <- rowMeans(top.model.input.heatmap.data[, -1])
  top.model.input.heatmap.data <- top.model.input.heatmap.data[order(top.model.input.heatmap.data$row.means, decreasing = T), ]
  top.how.many <- min(top.how.many.original, dim(top.model.input.heatmap.data)[1])
  top.model.input.heatmap.data <- top.model.input.heatmap.data[1:top.how.many, ]
  top.model.input.heatmap.data$model.inputs <- with(top.model.input.heatmap.data, 
                                                    ordered(model.inputs, levels=model.inputs))
  top.model.input.heatmap.data$row.means <- NULL
  top.model.input.heatmap.data[, -1] <- top.model.input.heatmap.data[, -1]/max(top.model.input.heatmap.data[, -1])
  hd.melt <- melt(top.model.input.heatmap.data, id="model.inputs")
  
  mi.names <- as.character(unique(hd.melt$model.inputs))
  mi.names <- mi.names[!(mi.names %in% derived.and.point.params.names)]
  ou.names <- as.character(unique(hd.melt$variable))
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  color.palette <- c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c')
  
  hd.melt <- merge(hd.melt,pprc.melt,by = c("model.inputs","variable"),all.x = T)
  hd.melt$ast <- ""
  hd.melt$ast[hd.melt$significant] <-"*"
  
  heatmap.all.the.rest <- 
    ggplot(hd.melt) + 
    geom_tile(aes(x=model.inputs, y = variable, fill=value),color="black")  +
    guides(fill = guide_legend(title="Overall Importance"))+
    scale_fill_gradientn(colours=color.palette) +
    #scale_fill_gradientn(colours = myPalette(6)) +
    ylab("Important Outputs")+ xlab("Model Inputs") +     
    geom_text(vjust=-0.,size=6,aes(x=model.inputs, y = variable, 
                                   label = round(value, 2)
                                   #label= paste(round(value, 2),"\n",round(pprc,2))
    ))+
    geom_text(vjust=1.2,size=5,color= "navy",aes(x=model.inputs, y = variable, 
                                                 label = paste(round(pprc, 2),ast, sep = "")
    )) +
    #ggtitle("Heatmap of Model Inputs vs. Model Outputs") + 
    scale_x_discrete(labels = str_wrap(all.io.names[mi.names], width = 10)) + 
    scale_y_discrete(labels = str_wrap(all.io.names[ou.names], width = 15))+
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position="none",
          panel.border = element_blank(),
          axis.text.x=element_text(size=small*font.reduction.factor,angle=0,face="italic" ), 
          axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
          strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
          axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
          axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
          legend.text = element_text( size = small*font.reduction.factor,face="italic" ))
  
  ###############################################
  ## One heatmap with all inputs
  ###############################################
  model.input.heatmap.data[, 'row.means'] <- rowMeans(model.input.heatmap.data[, -1])
  model.input.heatmap.data <- model.input.heatmap.data[order(model.input.heatmap.data$row.means, decreasing = T), ]
  top.how.many <- min(top.how.many.original, dim(model.input.heatmap.data)[1])
  model.input.heatmap.data <- model.input.heatmap.data[1:top.how.many, ]
  model.input.heatmap.data$model.inputs <- with(model.input.heatmap.data, 
                                                    ordered(model.inputs, levels=model.inputs))
  model.input.heatmap.data$row.means <- NULL
  model.input.heatmap.data[, -1] <- model.input.heatmap.data[, -1]/max(model.input.heatmap.data[, -1])
  all.hd.melt <- melt(model.input.heatmap.data, id="model.inputs")
  
  mi.names <- as.character(unique(all.hd.melt$model.inputs))
  mi.names <- mi.names[!(mi.names %in% derived.and.point.params.names)]
  ou.names <- as.character(unique(all.hd.melt$variable))
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  color.palette <- c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c')
  
  all.hd.melt <- merge(all.hd.melt,
                       pprc.melt,by = c("model.inputs","variable"),all.x = T)
  all.hd.melt$ast <- ""
  all.hd.melt$ast[all.hd.melt$significant] <-"*"
  
  heatmap.top.inputs <- 
    ggplot(all.hd.melt) + 
    geom_tile(aes(x=model.inputs, y = variable, fill=value),color="black")  +
    guides(fill = guide_legend(title="Overall Importance"))+
    scale_fill_gradientn(colours=color.palette) +
    #scale_fill_gradientn(colours = myPalette(6)) +
    ylab("Important Outputs")+ xlab("Model Inputs") +     
    geom_text(vjust=-0.,size=6,aes(x=model.inputs, y = variable, 
                                   label = round(value, 2)
                                   #label= paste(round(value, 2),"\n",round(pprc,2))
    ))+
    geom_text(vjust=1.2,size=5,color= "navy",aes(x=model.inputs, y = variable, 
                          label = paste(round(pprc, 2),ast, sep = "")
    )) +
    #ggtitle("Heatmap of Model Inputs vs. Model Outputs") + 
    scale_x_discrete(labels = str_wrap(all.io.names[mi.names], width = 10)) + 
    scale_y_discrete(labels = str_wrap(all.io.names[ou.names], width = 15))+
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position="none",
          panel.border = element_blank(),
          axis.text.x=element_text(size=small*font.reduction.factor,angle=0,face="italic" ), 
          axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
          strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
          axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
          axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
          legend.text = element_text( size = small*font.reduction.factor,face="italic" ))
  
  if(do.other.plots){
  do.parcoord.and.3D.plot(all.hd.melt,dat.realization.avg,
                          focus.output="tax.gap.percent",
                          top.threshold= 5,plots.dir =plots.dir )
  }
  
  return(list(ww = heatmap.white.washers.and.rest, rest = heatmap.all.the.rest, 
              top.inputs = heatmap.top.inputs, heatmap.data = model.input.heatmap.data))
}