do.parcoord.and.3D.plot<- function(x,dat.realization.avg,
                                   focus.output="tax.gap.percent",
            top.threshold= 5,plots.dir =NULL){

require("plot3D")

x<-x[all.hd.melt$variable%in%focus.output,]

x$points<-0
x$points[order(x$value)] <- 1:nrow(x)
top.names<-x[order(x$points, decreasing = T),"model.inputs"]
top.names<- as.character(top.names[1:top.threshold])

x<- x[x$model.inputs%in%top.names,]

x$ordering.val <-x$value*sign(x$cor) 
x$points<-0
x$points[order(x$ordering.val)] <- 1:nrow(x)
#x$points[order(x$pprc)] <- 1:nrow(x)
#x$points[order(abs(x$pprc))] <- x$points[order(abs(x$pprc))]+1:nrow(x)
top.names<-x[order(x$points, decreasing = T),"model.inputs"]


all.names <- c(focus.output,as.character(top.names))

dat <- dat.realization.avg[,all.names]

dat.reduced <-  dat# dat[sample(1:nrow(dat),200),]
dat.reduced <- dat.reduced[order(dat.reduced$tax.gap.percent),]


group<-cut(dat.reduced$tax.gap.percen,quantile(dat.reduced$tax.gap.percent))

myPalette <- rev(brewer.pal(8, "Spectral"))
color.palette <- c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c')
k <- adjustcolor(color.palette[group], alpha=0.3)


pdf( paste(plots.dir,"parcood.plot.pdf",sep=""), width = 6, height = 4 )
parcoord.mod(dat.reduced,col=k,var.label = T)
dev.off()

pdf( paste(plots.dir,"plot3D.pdf",sep=""), width = 6, height = 6 )
scatter3D(dat.reduced$tax.rate.delta, 
          dat.reduced$audit.rate, 
          dat.reduced$beta.personal, 
          colvar =dat.reduced$tax.gap.percent , 
          col = myPalette,
          revolutions=2,
          alpha=1,
          theta = 40, 
          phi = 40,
          bty = "g",
          pch = 20, 
          cex = 0.7, 
          cex.axis=0.7,
          cex.lab=1,
          ticktype = "detailed",
          col.grid = "darkblue",
          xlab = "tax.rate.delta",
          ylab ="audit.rate", 
          zlab = "beta.personal")
dev.off()

return(NULL)
}


