
#Massage the network data

netw.data.file <- "Network Data/PN1.contact.RData"
pop.data.file <- "Network Data/PN1.demog.person.RData"
pop.data.file.output <- "Tax data/population_data_1k.csv"
netw.data.file.output <- "Tax data/network_data_1k.csv"

# netw.data.file <- "Network Data/PN10.contact.RData"
# pop.data.file <- "Network Data/PN10.demog.person.RData"
# pop.data.file.output <- "Tax data/population_data_10k.csv"
# netw.data.file.output <- "Tax data/network_data_10k.csv"



pop.data <- readRDS(pop.data.file)
names(pop.data) <- tolower(names(pop.data))

pop.data[, 'tax.ids'] <- 1:nrow(pop.data)

eff.tax.rate <- read.csv("Tax data/US_Income_Tax_Rates_2016.csv", stringsAsFactors = F)

pop.data[, 'tax.rate'] <- sapply(pop.data[, "tax.ids"], function(id){
  inc.f.stat <- pop.data[id, c("income", "filing.status")]
  sub.set <- eff.tax.rate[eff.tax.rate$filing.status == inc.f.stat$filing.status, ]
  interval.row <- findInterval(inc.f.stat$income, sub.set$min, rightmost.closed = T)
  sub.set[interval.row, 'tax.rate']
})

i <- which(names(pop.data) == "perceivedauditrate")
names(pop.data)[i] <- "per.audit.rate"

i <- which(names(pop.data) == "perceivedpenaltyrate")
names(pop.data)[i] <- "per.penalty.rate"

write.csv(pop.data, pop.data.file.output, row.names = FALSE)


netw.data <- readRDS(netw.data.file)

netw.data[, 'id1'] <- match(netw.data$Person.Id.1, pop.data$person.id)
netw.data[, 'id2'] <- match(netw.data$Person.Id.2, pop.data$person.id)



write.csv(netw.data, netw.data.file.output, row.names = F)
