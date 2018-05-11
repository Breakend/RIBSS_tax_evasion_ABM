

get.portland.network.demo.details<- function(Person.Id){
  o.Household.Id <- demog.person[match(Person.Id,demog.person$Person.Id),
                                 "Household.Id"]
  o.Household.Income.Cat<- as.numeric(as.character(demog.household[match(o.Household.Id,
                                                 demog.household$Household.Id),
                                           "Household.Income.Cat"]))
  o.Household.Size<- as.numeric(as.character(demog.household[match(o.Household.Id,
                                demog.household$Household.Id),
                                          "Household.Size"]))
  
  o.Household.Workers<- as.numeric(as.character(demog.household[match(o.Household.Id,
                                demog.household$Household.Id),"Workers"]))
  
  o.Home.Location.Id<- demog.household[match(o.Household.Id,
                                             demog.household$Household.Id),
                                       "Home.Location.Id"]
  
  o.zipcode <- as.numeric(as.character(location.Id.lookup[match(o.Home.Location.Id,
                                        location.Id.lookup$Location.Id),
                                  "zipcode"]))
  
  o.inc.zip <- o.Household.Income.Cat+as.numeric(o.zipcode)/10^5
  
  R<- as.data.frame(cbind(Household.Id=o.Household.Id, 
                          Household.Income.Cat=o.Household.Income.Cat,
                          Household.Size=o.Household.Size,
                          Household.Workers=o.Household.Workers,
                          zipcode=o.zipcode))
  return(R)
}


#############################################################
###                                                       ###
###     Get Income and other attributes of the agents.             
###                                                       ###
#############################################################

edge.list <- contact.new.sub.rewired

Person.ID.set<-unique(c(edge.list$Person.Id.1,edge.list$Person.Id.2))

attributes <- get.portland.network.demo.details(Person.ID.set)

hist(attributes$Household.Income.Cat)

household.income.lookup


