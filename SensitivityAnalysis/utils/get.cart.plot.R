get.cart.plot <- function(dat.realization.avg, lhs.field, rhs.fields, tweak = 1,
                          width=8, height=4,
                          plots.dir = "SensitivityAnalysis/figures/sensitivity/") {
  
  lhs.field.name <<- lhs.field
  formula<- paste(lhs.field,"~",paste(rhs.fields,collapse=" + "),sep="")
  formula <-eval(parse(text=formula))
  CART.fit <- rpart(formula, dat.realization.avg, method="anova") ## , method="class" didn't work well: histograms seemed wrong.
  #RF.fit <- randomForest(formula,data = dat.realization.avg,importance = TRUE)
  
  pdf(tf <- paste0(plots.dir, "cart_plot_", gsub('\\.', '_', lhs.field), ".pdf"),
       width=width, height=height)
  rp <- rpart.plot(CART.fit, box.palette="GnYlRd",#box.palette="GnBu",
                   branch.lty=3, shadow.col="gray", 
                   tweak = tweak,
                   split.fun = split.fun,
                   node.fun = node.label.fun)
  dev.off()
  lhs.field.name <<- NULL
  invisible(CART.fit)
}

node.label.fun <- function(x, labs, digits, varlen) {
  full.names <- get.full.names()
  prefx <- full.names[lhs.field.name]
  labs[1] <- paste(prefx, "=", labs[1])
  labs
}

split.fun <- function(x, labs, digits, varlen, faclen) {
  full.names <- get.full.names()
  first.names <- sapply(labs, function(lab) {
    #Split label names
    lab.splits <- str_split(lab, " ")
    lab.splits[[1]][1] #Return the first label so that it can be replaced with full.name
  })
  #first.names <- unique(first.names)
  labs <- sapply(1:length(labs), function(i) {
    gsub(first.names[i], full.names[first.names[i]], labs[i])
  })
  labs
}
