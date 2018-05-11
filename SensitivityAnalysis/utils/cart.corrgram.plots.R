#Generate CART Corrgram plots

cart.corrgram.plots <- function(CART.fit, output.field, dataset, inputs=1:10)
{
  fit.branches<-get.CART.branches(CART.fit)
  node.plots <- list()
  subnode.plots <- list()
  corr.plots <- list()
  
  
  for(i in 1:length(fit.branches)){
    
    nodes<-fit.branches[[i]]
    option.split <- nodes[length(nodes)]
    split.cond<- nodes[-length(nodes)]
    dat.split <- dataset
    if(length(split.cond)>0){
      for(j in 1:length(split.cond)){
        con<-split.cond[j]
        con<- paste("dat.split$",con,sep="")
        dat.split <- dat.split[eval(parse(text=con)),]
      }
      split.cond.mod <- split.cond
      split.cond.mod <- gsub("< 0.5"," OFF",split.cond.mod)
      split.cond.mod <- gsub(">=0.5"," ON",split.cond.mod)
      p.title<-paste("branch",paste(split.cond.mod,collapse=", "),sep=" ")
    }else{
      p.title<-paste("root node")
    }
    
    p.title<- paste(output.field, "CART",p.title, sep=" ")
    dat.split[,option.split] <- as.factor(dat.split[,option.split])
    node.plots[[i]] <- ggplot(dat.split) +
      geom_histogram(aes(x=tax.gap.percent, 
                         y=..count../sum(..count..),
                         fill=eval(parse(text=option.split))),
                     bins = 20,  color="black") +
      ylab("Proportion of runs")+
      scale_fill_discrete(option.split, 
                          labels=c("Off", "On"))+
      ggtitle(paste(p.title,sep=""))  
    
    
    
    dat.split<- split(dat.split,dat.split[,option.split])
    
    
    for(j in names(dat.split)){
      
      dat.split.leaf <- dat.split[[j]]
      
      model.input.fields <- names(dat.split.leaf)[12:40]
      
      formula<- paste(output.field,"~",paste(model.input.fields,collapse="+"),sep="")
      formula <-eval(parse(text=formula))
      retval <- cart.forest.tabulate(formula, dataset)
      sensi.dat <- retval[['tab']]
      
      sensi.dat <- sensi.dat[inputs,]
      
      ff <- c(output.field , as.character(sensi.dat$option)[1:4])
      i.j <- paste0(i, j)
      corr.plots[[i.j]] <- corrgram(dat.split.leaf[,ff], order=FALSE, lower.panel=panel.shade,
               upper.panel=panel.pie, text.panel=panel.txt,
               main=paste("Correlogram of", p.title,sep=" "))
      
      
      sensi.dat.melt <-melt(sensi.dat, id="option" ,value.name = "importance")
      sensi.dat.melt <- sensi.dat.melt[!is.na(sensi.dat.melt[,"importance"]),]
      
      tmp <- c("OFF","ON")[as.numeric(j)+1]
      pp.title <- paste("Sensitivity ", p.title,", ", option.split," ",tmp,sep="")
      subnode.plots[[i.j]] <- ggplot()+
        geom_bar(data= sensi.dat.melt,aes(x=option,y=importance, fill= variable),
                 stat="identity", color="black", position="dodge") +
        theme_bw() +
        guides(fill = guide_legend(title="Importance Metric"))+
        xlab("Input Variable")+
        ylab("Overall Importance")+
        ggtitle(pp.title)
    }
  }
  
  return(list(node.plots, corr.plots, subnode.plots))
}