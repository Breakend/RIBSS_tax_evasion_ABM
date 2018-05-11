# #########################################################
# ## Sensitivity Analysis
# ##
# #########################################################
rm(list = ls())

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

########################################################
##                 All Parameters                     ##
########################################################
sensitivity.data <- NULL
source("SensitivityAnalysis/utils/utils.R")

sens.dir <- "SensitivityAnalysis/figures/sensitivity/Full/"

sensitivity.data <- read.csv("Outputs/Sensitivity/full_sensitivity_dataset.csv", stringsAsFactors = F)
sensitivity.data[, 'comp.rate.rmse'] <- calc.comp.rate.rmse(sensitivity.data)

tmp <- !(sensitivity.data$network.model%in%FALSE)
sensitivity.data$network.model <- tmp

exp.design.params <- read.csv("Inputs/Sensitivity.Exp.Design.model.parameters.csv", stringsAsFactors = F)
derived.and.point.params.inds <- which(exp.design.params$pdf %in% c('Derived', 'Point'))
derived.and.point.params.names <- exp.design.params$X[derived.and.point.params.inds]
input.fields <- exp.design.params$X[-derived.and.point.params.inds]
output.fields<- colnames(sensitivity.data)[!(colnames(sensitivity.data)%in%input.fields)]

sdat.split <- split(sensitivity.data, sensitivity.data$case)

sdat.split.tmp <- lapply(sdat.split,FUN=average.over.realizations,
                         input.fields=input.fields,
                         output.fields=output.fields)
sdat.realization.avg<- as.data.frame(do.call("rbind",sdat.split.tmp))

sdat.realization.avg$Rannoyed <- ifelse(is.nan(sdat.realization.avg$Rannoyed), 0, sdat.realization.avg$Rannoyed)
sdat.realization.avg$hideable.Rannoyed <- ifelse(is.nan(sdat.realization.avg$hideable.Rannoyed), 0, sdat.realization.avg$hideable.Rannoyed)

remove.these <- NULL

#inds <- which(input.fields %in% remove.these)

model.input.heatmap.data <- NULL
all.io.names <- get.full.names()

cart.tree.params <- input.fields
#Important global variable. do not delete
lhs.field.name <- NULL
main.outputs <- c('tax.gap.percent', 'hideable.Rannoyed', 'hideable.R0', 'hideable.R3', 
                  'mean.per.audit.rate', 'comp.rate.rmse', 'total.persistence')

other.outputs <- c('tax.gap.percent', 'mean.per.audit.rate', 'mean.per.penalty.rate', 
                   'mean.perc.hideable.income.reported')

#First generate heatmap by averaging scores from both CART and Random Forest Fits
retv.both <- generate.heatmap(sdat.realization.avg, input.fields, 
                              main.outputs, top.how.many = 6, 
                              which.fit = "both", plots.dir = sens.dir,
                              small = 14,big = 18, font.reduction.factor=0.75,
                              do.other.plots=TRUE)
save.plots(retv.both[[1]], "heatmap_important_outputs_ww_method_both", width = 10, height = 6, plots.dir = sens.dir)
save.plots(retv.both[[2]], "heatmap_important_outputs_rest_method_both", width = 10, height = 6, plots.dir = sens.dir)
save.plots(retv.both[[3]], "heatmap_important_outputs_all_method_both", width = 10, height = 6, plots.dir = sens.dir)

#Generate heatmaps using the Random Forest Fit Only
retv.rf <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, 
                            top.how.many = 6, 
                            which.fit = "RF", plots.dir = sens.dir,
                            small = 14,big = 18, font.reduction.factor=0.75)
save.plots(retv.rf[[1]], "heatmap_important_outputs_ww_method_RF", width = 10, height = 6, plots.dir = sens.dir)
save.plots(retv.rf[[2]], "heatmap_important_outputs_rest_method_RF", width = 10, height = 6, plots.dir = sens.dir)
save.plots(retv.rf[[3]], "heatmap_important_outputs_all_method_RF", width = 10, height = 6, plots.dir = sens.dir)

#Generate heatmap using the CART Fit only
retv.cart <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs,
                              top.how.many = 6, 
                              which.fit = "CART", plots.dir = sens.dir)
save.plots(retv.cart[[1]], "heatmap_important_outputs_ww_method_CART", width = 10, height = 6, plots.dir = sens.dir)
save.plots(retv.cart[[2]], "heatmap_important_outputs_rest_method_CART", width = 10, height = 6, plots.dir = sens.dir)
save.plots(retv.cart[[3]], "heatmap_important_outputs_all_method_CART", width = 10, height = 6, plots.dir = sens.dir)


#Saves a nice CART Tree
cf <- get.cart.plot(sdat.realization.avg, lhs.field = 'tax.gap.percent',
                    rhs.fields = c('audit.rate', 'beta.personal', 
                                   'tax.rate.delta', 'v.PP'),
                    plots.dir = sens.dir)


###################################################
##     Sensitivity Analysis with Fixed Params    ##
###################################################

fixed.params.sens.dir <- "SensitivityAnalysis/figures/sensitivity/FixedParams/"
sensitivity.data <- read.csv("Outputs/SensFixedParam/sens_with_fixed_params.csv", stringsAsFactors = F)
sensitivity.data[, 'comp.rate.rmse'] <- calc.comp.rate.rmse(sensitivity.data)

remove.these <- c("full.tendency.factor", "return.weight")

tmp <- !(sensitivity.data$network.model%in%FALSE)
sensitivity.data$network.model <- tmp

exp.design.params <- read.csv("Inputs/Sensitivity.Exp.Design.model.parameters.csv", stringsAsFactors = F)
derived.and.point.params.inds <- which(exp.design.params$pdf %in% c('Derived', 'Point'))
derived.and.point.params.names <- exp.design.params$X[derived.and.point.params.inds]
input.fields <- exp.design.params$X[-derived.and.point.params.inds]
output.fields<- colnames(sensitivity.data)[!(colnames(sensitivity.data)%in%input.fields)]

sdat.split <- split(sensitivity.data, sensitivity.data$case)

sdat.split.tmp <- lapply(sdat.split,FUN=average.over.realizations,
                         input.fields=input.fields,
                         output.fields=output.fields)
sdat.realization.avg<- as.data.frame(do.call("rbind",sdat.split.tmp))

sdat.realization.avg$Rannoyed <- ifelse(is.nan(sdat.realization.avg$Rannoyed), 0, sdat.realization.avg$Rannoyed)
sdat.realization.avg$hideable.Rannoyed <- ifelse(is.nan(sdat.realization.avg$hideable.Rannoyed), 0, sdat.realization.avg$hideable.Rannoyed)


inds <- which(input.fields %in% remove.these)
input.fields <- input.fields[-inds]

model.input.heatmap.data <- NULL
all.io.names <- get.full.names()

cart.tree.params <- input.fields
#Important global variable. do not delete
lhs.field.name <- NULL
main.outputs <- c('tax.gap.percent', 'hideable.Rannoyed', 'hideable.R0', 'hideable.R3', 
                  'mean.per.audit.rate', 'comp.rate.rmse', 'total.persistence')

other.outputs <- c('tax.gap.percent', 'mean.per.audit.rate', 'mean.per.penalty.rate', 
                   'mean.perc.hideable.income.reported')

#First generate heatmap by averaging scores from both CART and Random Forest Fits
retv.both <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, 
                              top.how.many = 6, 
                              which.fit = "both", 
                              plots.dir = fixed.params.sens.dir,
                              small = 14,big = 18, font.reduction.factor=0.75)
save.plots(retv.both[[1]], "heatmap_important_outputs_ww_method_both", width = 10, height = 8, plots.dir = fixed.params.sens.dir)
save.plots(retv.both[[2]], "heatmap_important_outputs_rest_method_both", width = 10, height = 8, plots.dir = fixed.params.sens.dir)
save.plots(retv.both[[3]], "heatmap_important_outputs_all_method_both", width = 10, height = 8, plots.dir = fixed.params.sens.dir)

#Generate heatmaps using the Random Forest Fit Only
retv.rf <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, top.how.many = 6, 
                            which.fit = "RF", plots.dir = fixed.params.sens.dir,small = 14,big = 18, font.reduction.factor=0.75)
save.plots(retv.rf[[1]], "heatmap_important_outputs_ww_method_RF", width = 10, height = 8, plots.dir = fixed.params.sens.dir)
save.plots(retv.rf[[2]], "heatmap_important_outputs_rest_method_RF", width = 10, height = 8, plots.dir = fixed.params.sens.dir)
save.plots(retv.rf[[3]], "heatmap_important_outputs_all_method_RF", width = 10, height = 8, plots.dir = fixed.params.sens.dir)

#Generate heatmap using the CART Fit only
retv.cart <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, top.how.many = 6, 
                              which.fit = "CART", plots.dir = fixed.params.sens.dir,small = 14,big = 18, font.reduction.factor=0.75)
save.plots(retv.cart[[1]], "heatmap_important_outputs_ww_method_CART", width = 10, height = 8, plots.dir = fixed.params.sens.dir)
save.plots(retv.cart[[2]], "heatmap_important_outputs_rest_method_CART", width = 10, height = 8, plots.dir = fixed.params.sens.dir)
save.plots(retv.cart[[3]], "heatmap_important_outputs_all_method_CART", width = 10, height = 8, plots.dir = fixed.params.sens.dir)


#Saves a nice CART Tree
cf <- get.cart.plot(sdat.realization.avg, lhs.field = 'tax.gap.percent',
                    rhs.fields = c('audit.rate', 'beta.personal', 'detection.eff', 'v.PP'),
                    plots.dir = fixed.params.sens.dir)


##############################################
##   Sensitivity to Policy Levers           ##
##############################################
policy.levers.sens.dir <- "SensitivityAnalysis/figures/sensitivity/PolicyLevers/"
policy.levers <- c('audit.rate', 'penalty.rate', 'tax.rate.delta', 'detection.eff')

sensitivity.data <- read.csv("Outputs/SensPolicyLevers/sens_policy_levers.csv", stringsAsFactors = F)
sensitivity.data[, 'comp.rate.rmse'] <- calc.comp.rate.rmse(sensitivity.data)

tmp <- !(sensitivity.data$network.model%in%FALSE)
sensitivity.data$network.model <- tmp

exp.design.params <- read.csv("Inputs/Sensitivity.Exp.Design.model.parameters.csv", stringsAsFactors = F)
input.fields <- policy.levers
output.fields<- colnames(sensitivity.data)[!(colnames(sensitivity.data)%in%input.fields)]

sdat.split <- split(sensitivity.data, sensitivity.data$case)

sdat.split.tmp <- lapply(sdat.split,FUN=average.over.realizations,
                         input.fields=input.fields,
                         output.fields=output.fields)
sdat.realization.avg<- as.data.frame(do.call("rbind",sdat.split.tmp))

sdat.realization.avg$Rannoyed <- ifelse(is.nan(sdat.realization.avg$Rannoyed), 0, sdat.realization.avg$Rannoyed)
sdat.realization.avg$hideable.Rannoyed <- ifelse(is.nan(sdat.realization.avg$hideable.Rannoyed), 0, sdat.realization.avg$hideable.Rannoyed)


model.input.heatmap.data <- NULL
all.io.names <- get.full.names()

cart.tree.params <- input.fields
#Important global variable. do not delete
lhs.field.name <- NULL
main.outputs <- c('tax.gap.percent', 'hideable.Rannoyed', 'hideable.R0', 'hideable.R3', 
                  'mean.per.audit.rate', 'comp.rate.rmse', 'total.persistence')

other.outputs <- c('tax.gap.percent', 'mean.per.audit.rate', 'mean.per.penalty.rate', 
                   'mean.perc.hideable.income.reported')

#First generate heatmap by averaging scores from both CART and Random Forest Fits
retv.both <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, top.how.many = 4, 
                              which.fit = "both", plots.dir = policy.levers.sens.dir)
save.plots(retv.both[[1]], "heatmap_important_outputs_ww_method_both", width = 10, height = 8, plots.dir = policy.levers.sens.dir)
save.plots(retv.both[[2]], "heatmap_important_outputs_rest_method_both", width = 10, height = 8, plots.dir = policy.levers.sens.dir)
save.plots(retv.both[[3]], "heatmap_important_outputs_all_method_both", width = 10, height = 8, plots.dir = policy.levers.sens.dir)

#Generate heatmaps using the Random Forest Fit Only
retv.rf <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, top.how.many = 4, 
                            which.fit = "RF", plots.dir = policy.levers.sens.dir)
save.plots(retv.rf[[1]], "heatmap_important_outputs_ww_method_RF", width = 10, height = 8, plots.dir = policy.levers.sens.dir)
save.plots(retv.rf[[2]], "heatmap_important_outputs_rest_method_RF", width = 10, height = 8, plots.dir = policy.levers.sens.dir)
save.plots(retv.rf[[3]], "heatmap_important_outputs_all_method_RF", width = 10, height = 8, plots.dir = policy.levers.sens.dir)

#Generate heatmap using the CART Fit only
retv.cart <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, top.how.many = 4, 
                              which.fit = "CART", plots.dir = policy.levers.sens.dir)
save.plots(retv.cart[[1]], "heatmap_important_outputs_ww_method_CART", width = 10, height = 8, plots.dir = policy.levers.sens.dir)
save.plots(retv.cart[[2]], "heatmap_important_outputs_rest_method_CART", width = 10, height = 8, plots.dir = policy.levers.sens.dir)
save.plots(retv.cart[[3]], "heatmap_important_outputs_all_method_CART", width = 10, height = 8, plots.dir = policy.levers.sens.dir)


#Saves a nice CART Tree
cf <- get.cart.plot(sdat.realization.avg, lhs.field = 'tax.gap.percent',
                    rhs.fields = policy.levers[-1],
                    plots.dir = policy.levers.sens.dir)

#Since audit.rate is washing away all other policy levers, removing it for this analysis
input.fields <- c('penalty.rate', 'tax.rate.delta', 'detection.eff')
retv.both <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, top.how.many = 3, 
                              which.fit = "both", plots.dir = policy.levers.sens.dir)
save.plots(retv.both[[3]], "heatmap_no_audit_rate_all_method_both", width = 10, height = 8, plots.dir = policy.levers.sens.dir)

#Generate heatmaps using the Random Forest Fit Only
retv.rf <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, top.how.many = 3, 
                            which.fit = "RF", plots.dir = policy.levers.sens.dir)
save.plots(retv.rf[[3]], "heatmap_no_audit_rate_all_method_RF", width = 10, height = 8, plots.dir = policy.levers.sens.dir)

#Generate heatmap using the CART Fit only
retv.cart <- generate.heatmap(sdat.realization.avg, input.fields, main.outputs, top.how.many = 3, 
                              which.fit = "CART", plots.dir = policy.levers.sens.dir)
save.plots(retv.cart[[3]], "heatmap_no_audit_rate_all_method_CART", width = 10, height = 8, plots.dir = policy.levers.sens.dir)

