
rm(list=ls())
### Load Libraries: please ensure you have installed these R packages.
library(ggplot2)
library(Hmisc)
library(igraph)
library(reshape2)
library(sdtoolkit)
library(rpart) #for CART routines: rpart = recursive partitioning and regression trees
library(rpart.plot)
library(randomForest)
library(plyr)
library(dplyr)
library(scales)
library(ggExtra)
library(ggfortify)
library(stringr)
library(RColorBrewer)
library(xtable)
library(corrgram)

#Setting options
#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)
source("SensitivityAnalysis/utils/utils.R")

calib.dir <- "SensitivityAnalysis/figures/calibration/"
fig.dir <- "WriteUp/Figures/"
tab.dir <- "WriteUp/Tables/"

small = 14
big = 18
font.reduction.factor=0.75

dataset <- read.csv("Outputs/Calibration/full_calibration_dataset.csv")
dataset$tax.gap <- NULL
dataset[, 'comp.rate.rmse'] <- calc.comp.rate.rmse(dataset)

### convert network.model to logical
tmp <- !(dataset$network.model%in%FALSE)
dataset$network.model <- tmp

ll<-match("seed",colnames(dataset))
input.fields <- colnames(dataset)[1:ll]
output.fields<- colnames(dataset)[!(colnames(dataset)%in%input.fields)]
#input.fields<- input.fields[!(input.fields%in%"seed")]

#dat.split <- split(dataset, list(dataset$model.option,dataset$case))
dat.split <- split(dataset, dataset$case)

dat.split.tmp <- lapply(dat.split,FUN=average.over.realizations,
                        input.fields=input.fields,
                        output.fields=output.fields)
dat.realization.avg<- as.data.frame(do.call("rbind",dat.split.tmp))
dat.realization.avg$log.comp.rate.rmse <- log10(dat.realization.avg$comp.rate.rmse)

dat.realization.avg$Rannoyed <- ifelse(is.nan(dat.realization.avg$Rannoyed), 0, dat.realization.avg$Rannoyed)
dat.realization.avg$hideable.Rannoyed <- ifelse(is.nan(dat.realization.avg$hideable.Rannoyed), 0, dat.realization.avg$hideable.Rannoyed)


ind <- which(input.fields == 'case')
params <- input.fields[-(1:ind)]
ind <- which(params == 'tax.rate')
params <- params[-ind]

# plots.tax.gap$heat.map
model.input.heatmap.data <- NULL
all.io.names <- get.full.names()
model.option.fields <- input.fields[3:9]

exp.design.params <- read.csv("Inputs/Exp.Design.model.parameters.csv", stringsAsFactors = F)
derived.and.point.params.inds <- which(exp.design.params$pdf %in% c('Derived', 'Point'))
derived.and.point.params.names <- exp.design.params$X[derived.and.point.params.inds]
input.fields <- exp.design.params$X[-derived.and.point.params.inds]
ind <- which(input.fields == 'tax.rate')
input.fields <- input.fields[-ind]
cart.tree.params <- input.fields
#Important global variable. do not delete
lhs.field.name <- NULL

ooi <- c("tax.gap.percent", "mean.per.audit.rate", "mean.per.penalty.rate", "total.persistence", "netw.effect.hideable.reported.gradient","comp.rate.rmse")
ooi1 <- c("tax.gap.percent", 'R0', 'hideable.R0', 'R1', 'hideable.R1', 'R3', 'hideable.R3', 'Rannoyed', 'hideable.Rannoyed')
main.outputs <- c("tax.gap.percent","comp.rate.rmse","total.persistence",
                  "hideable.R0", "hideable.R3" , "hideable.Rannoyed","audit.ratio" )



##########################################
## Calibration
##########################################
calibration.targets <- read.csv("Outputs/Calibration Targets/targets.csv", stringsAsFactors = F)
calibration.targets$source <- NULL
num.tar <- nrow(calibration.targets)
calibration.targets <- calibration.targets[-((num.tar-1):num.tar), ]

target.vars <- main.outputs[!(main.outputs%in%"audit.ratio")]
observed <- dat.realization.avg[, target.vars]
pop.data <- read.csv("Inputs/PN1_population_data.csv", stringsAsFactors = F)
observed[, 'audit.ratio'] <- dat.realization.avg$mean.per.audit.rate/mean(pop.data$per.audit.rate*100)
dat.realization.avg[, 'audit.ratio'] <- observed$audit.ratio

dat.realization.avg[, 'obj.fn.value'] <- calib.obj(calibration.targets, observed)
min.id <- which.min(dat.realization.avg$obj.fn.value)
case.id <- dat.realization.avg$case[min.id]

# #Check the U Distribution
# d.vars <- names(dat.realization.avg)[grep("perc.hideable.income", names(dat.realization.avg))]
# d.vars <- d.vars[-c(13, 14)]
# just.distr <- dat.realization.avg[, d.vars]
# u.dist <- just.distr[case.id, ]
# u.dist <- as.data.frame(t(u.dist))
# barplot(u.dist[, 1])

ordered.dat <- dat.realization.avg[order(dat.realization.avg$obj.fn.value), ]
ordered.dat[, 'rank'] <- 1:nrow(ordered.dat)
top.cases.level <- 50
cal.res <- 
  ggplot(ordered.dat) + 
  geom_line( aes(rank, obj.fn.value), size =2 , color= "steelblue3" ) + 
  xlab("Rank") + ylab("Objective Function Value") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  #ggtitle("Calibration Results") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position=c(0.4, 0.80),
        legend.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        axis.text.x=element_text(size=small,face="italic" ), 
        axis.text.y=element_text(size=small,face="italic"  ) ,
        strip.text=element_text( size=small,face="italic"  ) ,
        axis.title.x = element_text( size=big,face="italic"  ) ,
        axis.title.y = element_text( size=big,face="italic"  ),
        legend.text = element_text( size = small,face="italic" )) + 
  geom_vline(xintercept = top.cases.level, linetype = "dotdash")
#print(cal.res)
save.plots(cal.res, "calibration_results", plots.dir = calib.dir, width = 10, height = 5)
save.plots(cal.res, "calibration_results", plots.dir = fig.dir, width = 10, height = 5)

best.case <- ordered.dat[1, ]
top.cases <- ordered.dat[1:top.cases.level, ]

top.50 <- ordered.dat[1:top.cases.level, ]
top.50.tax.gap <- hist(top.50$tax.gap.percent)
#best.case$tax.gap.percent
#case.id

top.50 <- ordered.dat[1:50, ]
cal.verif <- ggplot(data = top.50) +
  geom_histogram(boundary = 0,
                 aes(x= tax.gap.percent), alpha=0.8, fill="steelblue3", color="black",breaks = 13:20) +
  theme_bw() +
  xlab("Mean Tax Gap in Top 50 Model-Runs") +
  ylab("Count")+
  scale_fill_manual(values = c( "steelblue3","darkolivegreen3", "red"))+ #"#78429E","#4F81BD"
  theme(legend.title = element_blank(),
        legend.position=c(0.5, 0.80),
        panel.border = element_blank(),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=12 ) ,
        axis.title.y = element_text( size=12 ),
        legend.text = element_text( size = 12))
save.plots(cal.verif, "calibration_verification_hist", plots.dir = calib.dir, width = 6, height = 4)

source('Survey code/SurveyLibrary/summaryfunctionFull.R')
retvals <- apply(top.50[, main.outputs], 2, summaryfunctionFull)
top.50.summ.tbl <- bind_rows(retvals)
top.50.summ.tbl<- cbind.data.frame(main.outputs=main.outputs,top.50.summ.tbl)
top.50.summ.tbl<- top.50.summ.tbl[match(top.50.summ.tbl$main.outputs,main.outputs),]
save(top.50.summ.tbl, file=paste0(calib.dir, "top_50_summ_tbl.Rdata"))

full.retvals <- apply(ordered.dat[, main.outputs], 2, summaryfunctionFull)
full.summ.tbl <- bind_rows(full.retvals)
full.summ.tbl <- cbind.data.frame(main.outputs=main.outputs,full.summ.tbl)
full.summ.tbl<- full.summ.tbl[match(full.summ.tbl$main.outputs,main.outputs),]
save(full.summ.tbl, file=paste0(calib.dir, "full_summ_tbl.Rdata"))

combined.tbl <- (rbind.data.frame(top.50.summ.tbl, full.summ.tbl))
save(combined.tbl, file=paste0(calib.dir, "combined_tbl.Rdata"))

out<-full.summ.tbl
tmp.tab.out<- xtable(out,auto=T,digits=1)
print(tmp.tab.out,file=paste(tab.dir,"full.summ.tbl.tex",sep=""), include.rownames=F)

out<-top.50.summ.tbl
tmp.tab.out<- xtable(out,auto=T,digits=1)
print(tmp.tab.out,file=paste(tab.dir,"top.50.summ.tbl.tex",sep=""), include.rownames=F)



###################################
## Calibration Histograms
###################################
tg.oh <- overlaid.histograms(ordered.dat, 50, 'tax.gap.percent', add.target = T, offset = 0.6)
save.plots(tg.oh, "tax_gap_overlay_hist", plots.dir = calib.dir, width = 10, height = 5)

div.oh <- overlaid.histograms(ordered.dat, 50, 'comp.rate.rmse', add.target = T, offset = 0.6, ht = 0.3)
save.plots(div.oh, "divergence_distr_overlay_hist", plots.dir = calib.dir, width = 13, height = 5)

hRa.oh <- overlaid.histograms(ordered.dat, 50, 'hideable.Rannoyed', add.target = T, offset = -1.0, ht = 0.2)
save.plots(hRa.oh, "hideable_Rannoyed_overlay_hist", plots.dir = calib.dir, width = 10, height = 5)

hR0.oh <- overlaid.histograms(ordered.dat, 50, 'hideable.R0', add.target = T, offset = 2.0, ht = 0.175)
save.plots(hR0.oh, "hideable_R0_overlay_hist", plots.dir = calib.dir, width = 10, height = 5)

hR3.oh <- overlaid.histograms(ordered.dat, 50, 'hideable.R3', add.target = T, offset = 1.5, ht = 0.15)
save.plots(hR3.oh, "hideable_R3_overlay_hist", plots.dir = calib.dir, width = 10, height = 5)

tp.oh <- overlaid.histograms(ordered.dat, 50, 'total.persistence', add.target = T, offset = 2.5, ht = 0.125)
save.plots(tp.oh, "persistence_overlay_hist", plots.dir = calib.dir, width = 10, height = 5)





###################################
## Input Filtering Calibration Analysis 
###################################

input.fields<- c("audit.rate","detection.eff","penalty.rate",
                 "v.PP","c1.dist.weight","c2","m.qP",
                 "beta.personal","return.weight",
                 "rate.refund.movement","morale.half.life",
                 "media.mid.effect" )


dfmelt.all <- melt(ordered.dat[,input.fields],variable.name = "variable", value.name = "value")
dfmelt.best <- melt(top.50[,input.fields],variable.name = "variable", value.name = "value")

dfmelt.all$set<- "all"
dfmelt.best$set <- "best"

dfmelt <- rbind.data.frame(dfmelt.best,dfmelt.all)

dfmelt$variable = factor(dfmelt$variable, levels = input.fields)

means.all <- aggregate(value ~  variable, dfmelt.all, mean)
means.all$set <- "all"
means.best <- aggregate(value ~  variable, dfmelt.best, mean)
means.best$set <- "best"

means <- rbind.data.frame(means.best,means.all)

small = 10 
big = 18

calib.input.filter.box.plot<- 
  ggplot(dfmelt)+
  geom_violin( aes(x=set, y=value,fill=set), alpha=0.3,
               position = position_dodge()) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_boxplot( aes(x=set, y=value,fill=set),width=.4, alpha=0.8,
                position = position_dodge()) + 
  facet_wrap(~variable,nrow = 3, ncol = 4, scales = "free")+   
  xlab(NULL) +
  ylab(NULL)+
  scale_alpha_manual(values=c(1,0.5))+ 
  stat_summary(aes(x=set, y=value), fun.y=mean, colour="darkolivegreen3", geom="point", 
               size=2)    +
  #scale_fill_hue(c=50, l=85)+
  scale_fill_manual(values = c("red", "steelblue3","darkolivegreen3"))+ #"#78429E","#4F81BD"
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position="none",
        legend.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        axis.text.x=element_blank( ), 
        axis.text.y=element_text(size=small,face="italic" ) ,
        strip.text=element_text( size=small,face="italic" ) ,
        axis.title.x = element_blank( ) ,
        axis.title.y = element_blank( ),
        legend.text = element_text( size = small,face="italic"),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

save.plots(calib.input.filter.box.plot, "calib.input.filter.box.plot", plots.dir = fig.dir, width = 10, height = 5)


###################################
##  Inputs Corrgram & Scaled Principle Component
###################################


cor.before <- cor(ordered.dat[, input.fields])
cor.after<-cor(top.50[, input.fields])

mm<-cor.after*(abs(cor.after-cor.before)>0.30)
tmp<- rowSums(mm)>0
sub.input.fields<-input.fields[tmp]
if(all(c("m.qP","s.qP")%in%sub.input.fields)){
  sub.input.fields<- sub.input.fields[!(sub.input.fields%in%"s.qP")]
}

cor.case.input.runs.full.plot<-
  corrgram(ordered.dat[, sub.input.fields], order=FALSE,
           main=NULL,
           upper.panel=panel.cor, lower.panel=panel.pie,
           diag.panel=panel.minmax, text.panel=panel.txt)

out<-cor.before[sub.input.fields,sub.input.fields]
tmp.tab.out<- xtable(out,auto=T,digits=2)
print(tmp.tab.out,file=paste(tab.dir,"cor.case.input.runs.full.tbl.tex",sep=""), include.rownames=F)
out<-cor.after[sub.input.fields,sub.input.fields]
tmp.tab.out<- xtable(out,auto=T,digits=2)
print(tmp.tab.out,file=paste(tab.dir,"cor.case.input.runs.top.tbl.tex",sep=""), include.rownames=F)


cor.top.50 <- cor(top.50[, sub.input.fields], use='pair')
pc<- na.omit(prcomp(top.50[, sub.input.fields],scale=T))

top.50$top.case.selected <- TRUE
pc.case.runs.inputs.top.50.plot<-
  autoplot(pc, data = top.50 , 
           loadings = TRUE, loadings.colour = 'navy', frame = TRUE,
           colour = 'top.case.selected', 
           frame.colour= 'top.case.selected',
           loadings.label = TRUE, loadings.label.size = 4)+   
  scale_color_manual(values=c( "steelblue3","darkolivegreen3",
                               "purple", "navy"))+
  scale_fill_manual(values=c( "steelblue3","darkolivegreen3",
                              "purple", "navy"))+
  #xlab("Principle Component 1") +
  #ylab("Principle Component 2") +
  scale_x_continuous(expand =  0.02*c(1, 1,1, 1))+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x=element_text(size=small*font.reduction.factor,angle=0,face="italic" ), 
        axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
        strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
        axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
        axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
        legend.text = element_text( size = small*font.reduction.factor,face="italic" ),
        legend.position="none")
save.plots(pc.case.runs.inputs.top.50.plot, "pc.case.runs.inputs.top.50.plot", 
           plots.dir = fig.dir, width = 8, height = 4)





###################################
## Outputs Calibration Analysis 
###################################

output.fields<- c("tax.gap.percent","log.comp.rate.rmse","total.persistence",
                  "hideable.R0", "hideable.R3" , "hideable.Rannoyed" )

ct <- calibration.targets
colnames(ct) <- c("variable","value","weight")
rownames(ct) <- ct$variable
ct["log.comp.rate.rmse",] <- ct["comp.rate.rmse",]
ct["log.comp.rate.rmse","variable"] <- "log.comp.rate.rmse" 
ct["log.comp.rate.rmse","value"] <- -1 

ct <- ct[output.fields,]
ct$variable <- as.factor(ct$variable)
ct$weight <- as.factor(ct$weight)


dfmelt.all <- melt(ordered.dat[,output.fields],variable.name = "variable", value.name = "value")
dfmelt.all <- rbind.data.frame(dfmelt.all,ct[,colnames(dfmelt.all)])
dfmelt.best <- melt(top.50[,output.fields],variable.name = "variable", value.name = "value")

dfmelt.all$set<- "all"
dfmelt.best$set <- "best"

dfmelt <- rbind.data.frame(dfmelt.best,dfmelt.all)

dfmelt$variable = factor(dfmelt$variable, levels = output.fields)

means.all <- aggregate(value ~  variable, dfmelt.all, mean)
means.all$set <- "all"
means.best <- aggregate(value ~  variable, dfmelt.best, mean)
means.best$set <- "best"

means <- rbind.data.frame(means.best,means.all)

small = 10 
big = 18

calib.output.filter.box.plot<- 
  ggplot(dfmelt)+
  geom_hline(data =ct, aes(yintercept = value, size=weight,linetype=weight),color="navy") +
  geom_violin( aes(x=set, y=value,fill=set), alpha=0.3,
               position = position_dodge()) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_boxplot( aes(x=set, y=value,fill=set),width=.4, alpha=0.8,
                position = position_dodge()) + 
  facet_wrap(~variable,nrow = 2,  scales = "free")  +   
  xlab(NULL) +
  ylab(NULL) +
  scale_alpha_manual(values=c(1,0.5,0.25))+ 
  stat_summary(aes(x=set, y=value), fun.y=mean, colour="darkolivegreen3", geom="point", 
               size=2)    +
  #scale_fill_hue(c=50, l=85)+
  scale_fill_manual(values = c("red", "steelblue3","darkolivegreen3"))+ #"#78429E","#4F81BD"
  #scale_color_manual(values = c("blue", "navy","black"))+
  scale_size_manual(values = c(1,1.5,2))+
  scale_linetype_manual(values = c("solid","dotted","solid"))+
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position="none",
        legend.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        axis.text.x=element_blank( ), 
        axis.text.y=element_text(size=small,face="italic" ) ,
        strip.text=element_text( size=small,face="italic" ) ,
        axis.title.x = element_blank( ) ,
        axis.title.y = element_blank( ),
        legend.text = element_text( size = small,face="italic"),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

save.plots(calib.output.filter.box.plot, "calib.output.filter.box.plot", plots.dir = fig.dir, width = 8, height = 4)


###################################
## Output Corrgram & Scaled Principle Component
###################################


cor.case.runs.full.plot<-
  corrgram(ordered.dat[, main.outputs], order=FALSE,
                               main=NULL,
                               upper.panel=panel.cor, lower.panel=panel.pie,
                               diag.panel=panel.minmax, text.panel=panel.txt)

#pdf(paste(fig.dir,"cor.case.runs.full.plot.pdf",sep=""), useDingbats=FALSE, width = 8, height = 4)
#dev.off()



ordered.dat$top.case.selected <- FALSE 
ordered.dat$top.case.selected[ordered.dat$case%in%top.50$case]<- TRUE

pc<- na.omit(prcomp(ordered.dat[, main.outputs],scale=T))
pc$weight<-round(100*pc$sdev^2/sum(pc$sdev^2),0)

ct <- calibration.targets
ct$pc.weight <- pc$weight
ct$scale <- pc$scale
ct$sdev<-pc$sdev
out<-ct
tmp.tab.out<- xtable(out,auto=T,digits=2)
print(tmp.tab.out,file=paste(tab.dir,"Output.Principle.Component.Analysis.tex",sep=""), include.rownames=F)



pc.case.runs.full.plot<- 
  autoplot(pc, data = ordered.dat ,alpha='top.case.selected',colour = 'top.case.selected', 
         loadings = TRUE, loadings.colour = 'navy',   frame = TRUE,
         loadings.label = TRUE, loadings.label.colour="navy" , 
         loadings.label.size = 4, 
         loadings.label.vjust=1,
         loadings.label.hjust=0.2)+    
  #xlab("Principle Component 1") +
  #ylab("Principle Component 2") +
  scale_color_manual(values=c( "red","steelblue3","darkolivegreen3",
                               "purple", "navy"))+
 # scale_fill_manual(values=c( "red","steelblue3","darkolivegreen3","purple", "navy"))+
  scale_x_continuous(expand =  0.02*c(1, 1,1, 1))+
  scale_y_continuous(expand =  0.02*c(1, 1,1, 1))+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x=element_text(size=small*font.reduction.factor,angle=0,face="italic" ), 
        axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
        strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
        axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
        axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
        legend.text = element_text( size = small*font.reduction.factor,face="italic" ),
        legend.position="none")
save.plots(pc.case.runs.full.plot, "pc.case.runs.full.plot", 
           plots.dir = fig.dir, width = 8, height = 4)


cor.case.runs.top.plot<-
  corrgram(top.50[, main.outputs], order=FALSE,
           main=NULL,
           upper.panel=panel.cor, lower.panel=panel.pie,
           diag.panel=panel.minmax, text.panel=panel.txt)

cor.top.50 <- cor(top.50[, main.outputs], use='pair')

pc<- na.omit(prcomp(top.50[, main.outputs],scale=T))
top.50$top.case.selected <- TRUE
pc.case.runs.top.50.plot<-
  autoplot(pc, data = top.50 , 
         loadings = TRUE, loadings.colour = 'navy', frame = TRUE,colour = 'top.case.selected', 
         frame.colour= 'top.case.selected',
         loadings.label = TRUE, loadings.label.size = 4)+    
  #xlab("Principle Component 1") +
  #ylab("Principle Component 2") +
  scale_color_manual(values=c( "steelblue3","darkolivegreen3",
                               "purple", "navy"))+
  scale_fill_manual(values=c( "steelblue3","darkolivegreen3",
                               "purple", "navy"))+
  scale_x_continuous(expand =  0.02*c(1, 1,1, 1))+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x=element_text(size=small*font.reduction.factor,angle=0,face="italic" ), 
        axis.text.y=element_text(size=small*font.reduction.factor,face="italic"  ) ,
        strip.text=element_text( size=small*font.reduction.factor,face="italic"  ) ,
        axis.title.x = element_text( size=big*font.reduction.factor,face="italic"  ) ,
        axis.title.y = element_text( size=big*font.reduction.factor,face="italic"  ),
        legend.text = element_text( size = small*font.reduction.factor,face="italic" ),
        legend.position="none")
save.plots(pc.case.runs.top.50.plot, "pc.case.runs.top.50.plot", 
           plots.dir = fig.dir, width = 8, height = 4)




###################################
## Run the tax model for the best case to generate plots
###################################

source("TaxModel.R")
library(beepr)
id <- which(names(best.case) == 'seed')
case <- best.case$case
best.case.seed <- best.case$seed


########################################
# 1k Population
########################################
lhs.configs <- read.csv("Inputs/lhs.cases_calibration.csv", stringsAsFactors = F)
best.case.config <- lhs.configs[lhs.configs$case == case, ]
best.case.config <- prepare.config(best.case.config)

#First, run this best case till equilibrium, and then, use the final state as input to the run
best.case.config[best.case.config$config.vars == 'run.till.equilibrium', 'value'] <- TRUE

#best.case.config[25, 2] <- 0.023487

retval <- tax.model(config = best.case.config, return.final.state.not.outputs = TRUE, seed = best.case$seed, 
                    population.data.file = "Inputs/PN1_population_data.csv", 
                    network.data.file = "Inputs/PN1_network_data.csv", parallelize.tax.model = T)
beep("coin")
final.state <- retval$final.state
pd <- retval$population.data
nd <- retval$network.data
g.info <- retval$g.info

save(retval, file = "SensitivityAnalysis/figures/BestCase_1k/best_case_1k_pre_eq.RData")

#Now, run the actual model and save plots
best.case.config[best.case.config$config.vars == 'run.till.equilibrium', 'value'] <- FALSE
retval.post.eq <- tax.model(config = best.case.config, seed=best.case$seed, initial.state = final.state, 
                            population.data = pd, network.data = nd, g.info = g.info,
                            save.plots = T, figures.dir = "SensitivityAnalysis/figures/BestCase_1k/", parallelize.tax.model = T)
beep("coin")
save(retval.post.eq, file = "SensitivityAnalysis/figures/BestCase_1k/best_case_1k_post_eq.RData")

#Remove network effect
no.netw.config <- best.case.config
no.netw.config[no.netw.config$config.vars == 'beta.network', 'value'] <- 0
retval.no.netw.effect <- tax.model(config = no.netw.config, seed=best.case$seed, initial.state = final.state,
                                   save.plots = T, figures.dir = "SensitivityAnalysis/figures/BestCase_1k/NoNetwork_1k/")
no.media.config <- best.case.config
no.media.config[no.media.config$config.vars == 'beta.media', 'value'] <- 0
no.media <- tax.model(config = no.media.config, seed=best.case$seed, initial.state = final.state, 
                      population.data = pd, network.data = nd, g.info = g.info,
                      save.plots = T, figures.dir = "SensitivityAnalysis/figures/BestCase_1k/NoMedia_1k/", parallelize.tax.model = T)

beep("fanfare")


################################
# 10k
################################

best.case.config[best.case.config$config.vars == 'run.till.equilibrium', 'value'] <- TRUE
#best.case.config[25, 2] <- 0.023487

load("Inputs/g_info_10.RData")
retval.10k <- tax.model(config = best.case.config, return.final.state.not.outputs = TRUE, seed = best.case$seed, 
                    g.info = g.info,
                    population.data.file = "Inputs/PN10_population_data.csv", 
                    network.data.file = "Inputs/PN10_network_data.csv", parallelize.tax.model = T)

save(retval.10k, file = "SensitivityAnalysis/figures/BestCase_10k/best_case_10k_pre_eq.RData")

beep("coin")
final.state <- retval.10k$final.state
pd <- retval.10k$population.data
nd <- retval.10k$network.data
g.info <- retval.10k$g.info

#Now, run the actual model and save plots
best.case.config[best.case.config$config.vars == 'run.till.equilibrium', 'value'] <- FALSE
retval.post.eq.10k <- tax.model(config = best.case.config, seed=best.case$seed, initial.state = final.state, 
                    population.data = pd, network.data = nd, g.info = g.info,
                    save.plots = T, figures.dir = "SensitivityAnalysis/figures/BestCase_10k/", parallelize.tax.model = T)
save(retval.post.eq.10k, file = "SensitivityAnalysis/figures/BestCase_10k/best_case_10k_post_eq.RData")

no.media.config <- best.case.config
no.media.config[no.media.config$config.vars == 'beta.media', 'value'] <- 0
no.media.10k <- tax.model(config = no.media.config, seed=best.case$seed, initial.state = final.state, 
                      population.data = pd, network.data = nd, g.info = g.info,
                      save.plots = T, figures.dir = "SensitivityAnalysis/figures/BestCase_10k/NoMedia_10k/", parallelize.tax.model = T)


beep("fanfare")
beep("fanfare")
beep("fanfare")



