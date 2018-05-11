
#This function returns the plots of sensitivity of field of interest to model parameters

sensitivity.to.model.inputs.plots <- function(field, dataset, which.fit, 
                                              plots.dir = "SensitivityAnalysis/figures/sensitivity/", 
                                              full.name = NULL, params=NULL, top.how.many = 6)
{
  all.names <- get.full.names()
  if(is.null(full.name)) full.name <- all.names[field]
  
  o.title <- paste("Overall Sensitivity of", full.name)
  sp.title <- paste("Sensitivity of", full.name, "(various metrics)")
  fname <- gsub("[.]", "_", field)
  
  formula <- paste(field, "~", paste(params, collapse=" + "))
  formula <-eval(parse(text=formula))
  CART.fit <- rpart(formula, dataset, method="anova")
  RF.fit <- randomForest(formula,data = dataset,importance = TRUE)  
  
  sensi.dat <- get.sensitivity.table(CART.fit,RF.fit, Normalize = T, which.fit = which.fit)
  sensi.dat.melt <- melt(sensi.dat, id="model.inputs" ,value.name = "importance")
  sensi.dat.melt <- sensi.dat.melt[!is.na(sensi.dat.melt[,"importance"]),]
  
  #Reordering the factors for an ordered plot. Reordering occurs accoding to 'overall'
  sensi.dat$model.inputs <- ordered(sensi.dat$model.inputs, levels=sensi.dat$model.inputs)
  
  x.axis.labs <- as.character(levels(sensi.dat$model.inputs))
  top.how.many <- min(top.how.many, dim(sensi.dat)[1])
  ind.sens.plot <- ggplot()+
    geom_bar(data= sensi.dat[1:top.how.many, ],aes(x=model.inputs,y=overall), fill= "steelblue3",
             stat="identity", color="black", position="dodge") +
    theme_bw() +
    ylab("Overall Importance Score")+ xlab("Model Inputs") + 
    #ggtitle(o.title) +  
    scale_x_discrete(labels = str_wrap(all.names[x.axis.labs], width = 10)) + 
    theme(panel.border = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          axis.text.x=element_text(size=12), 
          axis.text.y=element_text(size=12 ) ,
          strip.text=element_text( size=12 ) ,
          axis.title.x = element_text( size=16 ) ,
          axis.title.y = element_text( size=16 ),
          legend.text = element_text( size = 12))
  
  save.plots(ind.sens.plot, paste0('ind_sens_plot_', 
                                   gsub(pattern = "[.]", replacement = "_", x = field), 
                                   "_", which.fit), 
             plots.dir = plots.dir, width = 10)
  
  return(list(ind.sens.plot = ind.sens.plot, ind.sens.dat = sensi.dat))
}