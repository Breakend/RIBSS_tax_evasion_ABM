
#This function returns the plots of sensitivity of field of interest to model parameters

sensitivity.to.model.parameters.plots <- function(field, dataset, full.name = NULL, params=NULL)
{
  all.names <- get.full.names()
  if(is.null(full.name)) full.name <- all.names[field]
  
  o.title <- paste("Overall Sensitivity of", full.name)
  sp.title <- paste("Sensitivity of", full.name, "(various metrics)")
  fname <- gsub("[.]", "_", field)
  browser()
  formula <- paste(field, "~", paste(model.option.fields, collapse=" + "))
  formula <-eval(parse(text=formula))
  CART.fit <- rpart(formula, dataset, method="anova")
  RF.fit <- randomForest(formula,data = dataset,importance = TRUE)  
  
  #rpart.plot(CART.fit)
  
  #fit.branches <- get.CART.branches(CART.fit)
  plots <- list()
  #plots[['cart.tree']] <- cart.plot
  
  overall.sensi.data <- NULL
  leaf.scores <- NULL
  total.obs <- nrow(dataset)
  wt <- NULL
 
  
  ln.conditions <- conditions.to.reach.leaf.nodes(CART.fit)
  for(con in ln.conditions)
  {
    #Gather indices of those observations that satisfy the condition
    if(con == "")
    {
      leaf.data <- dataset
    } else {
      indices <- which(with(dataset, eval(parse(text = con))))
      leaf.data <- dataset[indices, ]
      
      #Tokenize the conditions using "&" as the delimiter
      tokenized.con <- strsplit(con, split = " & ") %>% unlist()
      n.con <- length(tokenized.con)     
      last.con <- tokenized.con[n.con]
      
      tokenized.con <- tokenized.con[-n.con]
      new.n.con <- n.con - 1
      if(new.n.con > 1)
      {
        leaf.but.1.con <- paste(tokenized.con, collapse = " & ")
      } else {
        if(new.n.con == 1) 
          leaf.but.1.con <- tokenized.con[1]
        else
          leaf.but.1.con <- last.con
      }
      
      #print(paste(field, leaf.but.1.con))
      indices.lb1 <- which(with(dataset, eval(parse(text = leaf.but.1.con))))
      leaf.but.1.data <- dataset[indices.lb1, ]
      
      mod.op <- NULL
      
      #Splitting it further based on < or >=
      tokenized.con <- strsplit(last.con, "<") %>% unlist()
      if(length(tokenized.con) > 1)
        mod.op <- tokenized.con[1]
      else {
        tokenized.con <- strsplit(last.con, ">") %>% unlist()
        if(length(tokenized.con) > 1)
          mod.op <- tokenized.con[1]
      }
      
      leaf.but.1.data[, mod.op] <- as.factor(leaf.but.1.data[ , mod.op])
      
      ln.title <- gsub("< 0.5","=OFF", leaf.but.1.con)
      ln.title <- gsub(">=0.5","=ON", ln.title)
      ln.title <- gsub("&","AND", ln.title)
      
      
      #Plot the histogram
      leaf.node.hist <- ggplot(leaf.but.1.data) +
        geom_histogram(aes(x=leaf.but.1.data[, field], 
                           y=..count../sum(..count..),
                           fill=eval(parse(text=mod.op))),
                       bins = 20,  color="black") +
        theme_bw() +
        ylab("Proportion of runs")+ xlab(full.name) +
        scale_fill_discrete(mod.op, 
                            labels=c("Off", "On"))+
        ggtitle(paste(ln.title, sep=""))
      mod_op <- gsub("[.]", "_", mod.op)
      save.plots(leaf.node.hist, paste0(fname, "_leafnode_", mod_op), width = 10)
    }
    
    leaf.obs <- nrow(leaf.data)
    wt <- c(wt, leaf.obs/total.obs)
    mod.con <- gsub("< 0.5"," OFF", con)
    mod.con <- gsub(">=0.5"," ON", mod.con)
    mod.con <- gsub("&"," AND", mod.con)
    
    #print(paste("Percent weight: ", leaf.obs, "/", total.obs, leaf.obs/total.obs))
    
    #Calculate the importance of each of the parameters
    if(is.null(params)) params <- names(dataset)[12:40]
    
    formula<- paste(field,"~",paste(params,collapse="+"),sep="")
    formula <-eval(parse(text=formula))
    
    CART.fit <- rpart(formula, leaf.data, method="anova") ## , method="class" didn't work well: histograms seemed wrong.
    RF.fit <- randomForest(formula,data = leaf.data,importance = TRUE)  
    
    sensi.dat <- get.sensitivity.table(CART.fit,RF.fit, Normalize = F)
    sensi.dat.melt <-melt(sensi.dat, id="option" ,value.name = "importance")
    sensi.dat.melt <- sensi.dat.melt[!is.na(sensi.dat.melt[,"importance"]),]
    
    normd.sensi.dat <- get.sensitivity.table(CART.fit,RF.fit)
    sensi.dat[, "overall"] <- apply(abs(normd.sensi.dat[, 1:3]), 1, mean, na.rm = T)
    sensi.dat <- sensi.dat[order(sensi.dat$overall, decreasing = T), ]
    #Reordering the factors for an ordered plot. Reordering occurs accoding to 'overall'
    sensi.dat$option <- ordered(sensi.dat$option, levels=sensi.dat$option)
    x.axis.labs <- as.character(levels(sensi.dat$option))
    
    score.plot <- ggplot()+
      geom_bar(data= sensi.dat,aes(x=option,y=overall), fill= "steelblue3",
               stat="identity", color="black", position="dodge") +
      theme_bw() +
      ylab("Overall Importance Score")+ xlab("Model Inputs") + 
      ggtitle(o.title) +  
      scale_x_discrete(labels = str_wrap(all.names[x.axis.labs], width = 10))
    save.plots(score.plot, paste(fname,"to", "inputs", sep="_"), width = 10)
    
    
    #Saving the scores for the purposes of a heatmap
    leaf.sensi.data <- sensi.dat[, c("option", "overall")]
    #Overwrite the column name 'overall' to field name
    names(leaf.sensi.data) <- c("model.inputs", mod.con)
    row.names(leaf.sensi.data) <- NULL
    
    if(is.null(overall.sensi.data)) { 
      overall.sensi.data <- leaf.sensi.data
    } else {
      overall.sensi.data <- merge(overall.sensi.data, leaf.sensi.data, by="model.inputs")
    }
    
  }

  if(ncol(overall.sensi.data) > 2) 
    overall.sensi.data[, 'wtd.sum'] <- rowSums(overall.sensi.data[, -1]*wt)
  else
    overall.sensi.data[, 'wtd.sum'] <- overall.sensi.data[, -1]*wt
  
  overall.sensi.data[, 'overall'] <- with(overall.sensi.data, wtd.sum/max(wtd.sum)) 
  overall.sensi.data <- overall.sensi.data[order(overall.sensi.data$overall, decreasing=T), ]
  #But save the data without factorizing it for future use. Factorization screws up the order.
  heat.map.data <- overall.sensi.data[, c("model.inputs", "overall")]
  names(heat.map.data) <- c("model.inputs", field)
  overall.sensi.data$model.inputs <- with(overall.sensi.data, ordered(model.inputs, levels = model.inputs))
  
  overall.plot <- ggplot() + 
    geom_bar(data=overall.sensi.data, aes(x = model.inputs, y=overall), 
             stat="identity" , fill="steelblue3") + theme_bw()
  
  
  return(list(overall.plot=overall.plot, score.plot = score.plot, heat.map.data = heat.map.data))
}