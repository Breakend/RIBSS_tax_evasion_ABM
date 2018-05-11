get.portland.network.demo.details<- function(Person.Id){
  o.Household.Id <- demog.person[match(Person.Id,demog.person$Person.Id),
                                 "Household.Id"]
  o.Household.Income.Cat<- demog.household[match(o.Household.Id,
                                                 demog.household$Household.Id),
                                           "Household.Income.Cat"]
  o.Home.Location.Id<- demog.household[match(o.Household.Id,
                                             demog.household$Household.Id),
                                       "Home.Location.Id"]
  
  o.zipcode <- location.Id.lookup[match(o.Home.Location.Id,
                                        location.Id.lookup$Location.Id),
                                  "zipcode"]
  
  o.inc.zip <- o.Household.Income.Cat+as.numeric(o.zipcode)/10^5
  
  R<- as.data.frame(cbind(Household.Income.Cat=o.Household.Income.Cat,zipcode=o.zipcode))
  return(R)
  
}