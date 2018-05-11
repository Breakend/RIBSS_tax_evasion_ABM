#Function to prepare the population data for the simulation

get.population.data <- function(file.name)
{
  pop.data <- read.csv(file.name, stringsAsFactors = F)
  return(pop.data)
}

get.graph.from.network.data <- function(file.name)
{
  netw.data <- read.csv(file.name, stringsAsFactors = F)
  return(netw.data)
}