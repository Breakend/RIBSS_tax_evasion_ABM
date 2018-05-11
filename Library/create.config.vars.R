
create.config.vars <- function(config)
{
  for(i in 1:dim(config)[1])
  {
    #config.row <- config[i, ]
    config.var <- config[i, 'config.vars']
    value <- get.config.param(config, config.var)
    assign(config.var, value, envir = parent.frame())
  }
}