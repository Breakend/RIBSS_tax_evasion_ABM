#Run a cart analysis and Random Forest analysis and tabulate the sensitivity results

cart.forest.tabulate <- function(formula, dataset)
{
  #Cart Sensitivity analysis
  CART.fit <- rpart(formula, dataset, method="anova") ## , method="class" didn't work well: histograms seemed wrong
  ### Do Random Forest Sensitivity Analysis
  RF.fit <- randomForest(formula,data = dataset,importance = TRUE)
  ### Arrange the Sensitivity Analysis table 
  sensi.dat <- get.sensitivity.table(CART.fit,RF.fit)
  sensi.dat <- sensi.dat[order(sensi.dat$IncNodePurity, decreasing = T), ]
  #sensi.dat$option <- ordered(sensi.dat$option, levels=sensi.dat$option)
  
  return (list(cf=CART.fit, rf=RF.fit, tab=sensi.dat))
}