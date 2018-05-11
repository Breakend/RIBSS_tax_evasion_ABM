#Get a particular config from test cases file using model.opton and case as indices

prepare.config <- function(config)
{
  config <- t(config)
  config <- data.frame(config.vars = row.names(config), value = config, stringsAsFactors = F)
  names(config) <- c("config.vars", "value")
  row.names(config) <- NULL
  
  return(config)
}

get.config.from.test.cases <- function(model.option, case, test.cases.file="Tax data/test.cases.csv")
{
  all.test.cases <- read.csv(test.cases.file, stringsAsFactors = F)
  config <- all.test.cases[all.test.cases$model.option==model.option & all.test.cases$case==case, ]
  config <- prepare.config(config)
  return(config)
}

