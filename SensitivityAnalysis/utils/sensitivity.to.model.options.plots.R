#Returns the overall and specific sensitivity plots for the field to model options.

sensitivity.to.model.options.plots <- function(field, model.option.fields, full.name = NULL, dataset)
{
  
  all.names <- get.full.names()
  if(is.null(full.name)) full.name <- all.names[field]
  o.title <- paste("Overall Sensitivity of", full.name)
  sp.title <- paste("Sensitivity of", full.name, "(various metrics)")
  
  formula<- paste(field,"~",paste(model.option.fields,collapse="+"),sep="")
  formula <-eval(parse(text=formula))
  retval <- cart.forest.tabulate(formula, dataset)
  #cart.plot <- prp(retval[['cf']])
  #rp.plot <- rpart.plot(retval[['cf']])
  sensi.dat <- retval[['tab']]
  

  sensi.dat[, "overall"] <- apply(sensi.dat[, 1:3], 1, mean, na.rm = T)
  sensi.dat <- sensi.dat[order(sensi.dat$overall, decreasing = T), ]
  #Reordering the factors for an ordered plot. Reordering occurs accoding to 'overall'
  #sensi.dat$option <- ordered(sensi.dat$option)
  
  #Saving the scores for the purposes of a heatmap
  heat.map.data <- sensi.dat[, c("option", "overall")]
  #Overwrite the column name 'overall' to field name
  names(heat.map.data) <- c("model.options", field)
  row.names(heat.map.data) <- NULL
  
  sensi.dat$option <- ordered(sensi.dat$option, levels=sensi.dat$option)
  x.axis.labs <- as.character(levels(sensi.dat$option))
  
  overall <- ggplot()+
    geom_bar(data=sensi.dat,aes(x=option,y=overall), fill= "cornflowerblue",
             stat="identity", position="identity", color="black") +
    theme_bw() +
    ylab("Overall Importance Score")+ xlab("Model Options") + 
    ggtitle(o.title) +  
    scale_x_discrete(labels = str_wrap(all.names[x.axis.labs], width = 10))
  
  sensi.dat$overall <- NULL
  sensi.dat.melt <-melt(sensi.dat, id="option" ,value.name = "importance")
  sensi.dat.melt <- sensi.dat.melt[!is.na(sensi.dat.melt[,"importance"]),]
  
  specific <- ggplot()+
    geom_bar(data= sensi.dat.melt,aes(x=option,y=importance, fill= variable),
             stat="identity", color="black", position="dodge") + theme_bw() +
    guides(fill = guide_legend(title="Importance Metric"))+
    ylab("Overall Importance")+ xlab("Model Options") +
    ggtitle(sp.title) + 
    scale_x_discrete(labels = str_wrap(all.names[x.axis.labs], width = 10))
  
  return(list(overall=overall, specific=specific, heat.map.data=heat.map.data))
  
}