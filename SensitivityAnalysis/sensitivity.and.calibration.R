
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
#library(corrgram)
library(stringr)
library(RColorBrewer)
library(xtable)

#Setting options
#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)
source("SensitivityAnalysis/utils/utils.R")

dataset <- read.csv("Outputs/Calibration/calibration_dataset.csv")
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
main.outputs <- c('tax.gap.percent', 'hideable.Rannoyed', 'hideable.R0', 'hideable.R3', 'mean.per.audit.rate', 'comp.rate.rmse')


##########################################
## Calibration
##########################################
calibration.targets <- read.csv("Outputs/Calibration Targets/targets.csv", stringsAsFactors = F)
calibration.targets$source <- NULL
num.tar <- nrow(calibration.targets)
calibration.targets <- calibration.targets[-((num.tar-1):num.tar), ]

target.vars <- c('tax.gap.percent', 'comp.rate.rmse', 'hideable.Rannoyed', 'hideable.R0', 'hideable.R3', 'total.persistence')
observed <- dat.realization.avg[, target.vars]
pop.data <- read.csv("Inputs/PN1_population_data.csv", stringsAsFactors = F)
target.vars <- c(target.vars, 'mean.per.audit')
observed[, 'mean.per.audit'] <- dat.realization.avg$mean.per.audit.rate/mean(pop.data$per.audit.rate*100)
dat.realization.avg[, 'mean.per.audit'] <- observed$mean.per.audit

dat.realization.avg[, 'obj.fn.value'] <- calib.obj(calibration.targets, observed)
min.id <- which.min(dat.realization.avg$obj.fn.value)
case.id <- dat.realization.avg$case[min.id]

ordered.dat <- dat.realization.avg[order(dat.realization.avg$obj.fn.value), ]
ordered.dat[, 'rank'] <- 1:nrow(ordered.dat)
top.cases <- 50
cal.res <- ggplot(ordered.dat, aes(rank, obj.fn.value)) + geom_line() + xlab("Rank") + ylab("Objective Function Value") +
  #ggtitle("Calibration Results") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=12 ) ,
        axis.title.y = element_text( size=12 ),
        legend.text = element_text( size = 12)) #+ 
  #geom_vline(xintercept = top.cases, linetype = "dotdash")
#print(cal.res)
save.plots(cal.res, "calibration_results", width = 6, height = 4)

top.cases <- ordered.dat[1:top.cases, ]

#Identified the best case
#print(paste("Best case ID: ", case.id))
best.case <- ordered.dat[1, ]


top.50 <- ordered.dat[1:50, ]
top.50.tax.gap <- hist(top.50$tax.gap.percent)
best.case$tax.gap.percent

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
save.plots(cal.verif, "calibration_verification_hist", width = 6, height = 4)

source('Survey code/SurveyLibrary/summaryfunctionFull.R')
retvals <- apply(top.50[, target.vars], 2, summaryfunctionFull)
top.50.summ.tbl <- bind_rows(retvals)
top.50.summ.tbl[, 'target.var'] <- target.vars
save(top.50.summ.tbl, file="SensitivityAnalysis/figures/top_50_summ_tbl.Rdata")

full.retvals <- apply(ordered.dat[, target.vars], 2, summaryfunctionFull)
full.summ.tbl <- bind_rows(full.retvals)
full.summ.tbl[, 'target.var'] <- target.vars
save(full.summ.tbl, file="SensitivityAnalysis/figures/full_summ_tbl.Rdata")

combined.tbl <- (rbind.data.frame(top.50.summ.tbl, full.summ.tbl))
save(combined.tbl, file="SensitivityAnalysis/figures/combined_tbl.Rdata")


###################################
## Calibration Histograms
###################################
tg.oh <- overlaid.histograms(ordered.dat, 50, 'tax.gap.percent', add.target = T, offset = 0.6)
save.plots(tg.oh, "tax_gap_overlay_hist", width = 10, height = 5)

div.oh <- overlaid.histograms(ordered.dat, 50, 'comp.rate.rmse', add.target = T, offset = 0.6, ht = 0.3)
save.plots(div.oh, "divergence_distr_overlay_hist", width = 13, height = 5)

hRa.oh <- overlaid.histograms(ordered.dat, 50, 'hideable.Rannoyed', add.target = T, offset = -1.0, ht = 0.2)
save.plots(hRa.oh, "hideable_Rannoyed_overlay_hist", width = 10, height = 5)

hR0.oh <- overlaid.histograms(ordered.dat, 50, 'hideable.R0', add.target = T, offset = 2.0, ht = 0.175)
save.plots(hR0.oh, "hideable_R0_overlay_hist", width = 10, height = 5)

hR3.oh <- overlaid.histograms(ordered.dat, 50, 'hideable.R3', add.target = T, offset = 1.5, ht = 0.15)
save.plots(hR3.oh, "hideable_R3_overlay_hist", width = 10, height = 5)

tp.oh <- overlaid.histograms(ordered.dat, 50, 'total.persistence', add.target = T, offset = 2.5, ht = 0.125)
save.plots(tp.oh, "persistence_overlay_hist", width = 10, height = 5)



#Run the tax model for the best case to generate plots
source("TaxModel.R")

id <- which(names(best.case) == 'seed')
case <- best.case$case
best.case.seed <- best.case$seed

lhs.configs <- read.csv("Inputs/lhs.cases_calibration.csv", stringsAsFactors = F)
best.case.config <- lhs.configs[lhs.configs$case == case, ]
best.case.config <- prepare.config(best.case.config)

#First, run this best case till equilibrium, and then, use the final state as input to the run
best.case.config[best.case.config$config.vars == 'run.till.equilibrium', 'value'] <- TRUE
retval <- tax.model(config = best.case.config, return.final.state.not.outputs = TRUE, seed = best.case$seed)
final.state <- retval$final.state

#Now, run the actual model and save plots
best.case.config[best.case.config$config.vars == 'run.till.equilibrium', 'value'] <- FALSE
retval <- tax.model(config = best.case.config, seed=best.case$seed, initial.state = final.state,
                    save.plots = T, figures.dir = "SensitivityAnalysis/figures/")

#Remove network effect
best.case.config[best.case.config$config.vars == 'run.till.equilibrium', 'value'] <- FALSE
best.case.config[best.case.config$config.vars == 'beta.network', 'value'] <- 0
retval.no.netw.effect <- tax.model(config = best.case.config, seed=best.case$seed, initial.state = final.state,
                    save.plots = T, figures.dir = "SensitivityAnalysis/figures/ToCompare/")

#worst.case <- ordered.dat[nrow(ordered.dat), ]
#print(paste("Best case's tax gap percent:", best.case$tax.gap.percent))
#print(paste("Worst case's tax gap percent:", worst.case$tax.gap.percent))



