# Get the parameter value from the configuration file
get.config.param <- function(config, param.name)
{
  param.value <- config[config$config.vars == param.name, 'value']

  if(length(param.value) == 0)
  {
    print(paste("Config var:", param.name, "not found"))
  }
  if(param.value == "TRUE" || param.value == "FALSE") 
  {
    param.value = as.logical(param.value)
  } else if(param.value %in% c("ER","SW","BA","Portland"))
  {
    prarm.value = as.character(param.value)
  } else
  {
    param.value = as.numeric(param.value)
  }
  return(param.value)
}