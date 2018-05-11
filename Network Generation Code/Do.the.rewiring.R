
#############################################################
###                                                       ###
###        Constructed a connected, representative Subnetwork            
###                                                       ###
#############################################################

rownames(contact.new) <- 1:nrow(contact.new)

separation <- 3
## starting.node  chosen with separation <- 4 and looking at  new.nodes.to.add
## e.g. start with 1 node as starting.node then after running with separation <- 4
## find sample(new.nodes.to.add,20)
#non.se.sa<- sample(boh,2) #2000183,2825069
starting.node <- c(2000183,2825069) ## set separation <- 2 for node ~1000
starting.node <- c(2000183) ### set seperation <- 3 for nodes ~10000

for(i in 1:separation){
  nn <- (contact.new$Person.Id.1%in%starting.node) | (contact.new$Person.Id.2%in%starting.node)
  contact.tmp <- contact.new[nn,]
  
  new.nodes.to.add <- unique(c(contact.tmp$Person.Id.1,contact.tmp$Person.Id.2))
  new.nodes.to.add <- new.nodes.to.add[!(new.nodes.to.add%in%starting.node)]
  starting.node <- c(starting.node, new.nodes.to.add)
  
  print(paste(length(starting.node),nrow(contact.tmp),sep=" "))
  
}

contact.sub <- contact.tmp


nn <- (contact.new$Person.Id.1%in%starting.node) | (contact.new$Person.Id.2%in%starting.node)
contact.tmp <- contact.new[nn,] ## All edges of our set of vertices

tmp <- rownames(contact.tmp)[!(rownames(contact.tmp)%in%rownames(contact.sub))]
dangling.bonds <- contact.tmp[tmp,] ### All edges A-B with eith A or B out of sample.
periphery.nodes <- unique(c(dangling.bonds$Person.Id.1,dangling.bonds$Person.Id.2))

nodes <- starting.node
outside.nodes <- periphery.nodes[!(periphery.nodes%in%nodes)]
periphery.nodes<- periphery.nodes[(periphery.nodes%in%nodes)]

### remove bonds connecting perifery nodes to get the truly dangling nodes
tmp <- dangling.bonds$Person.Id.1%in%periphery.nodes & dangling.bonds$Person.Id.2%in%periphery.nodes
non.dangling.bonds <- dangling.bonds[tmp,]
dangling.bonds <- dangling.bonds[!tmp,]

inside.edges <- rbind(contact.sub,non.dangling.bonds)


### Output Edge statistics
dim(contact.sub)
dim(contact.tmp)
dim(non.dangling.bonds)
dim(dangling.bonds)
dim(inside.edges) 

### Output Node statistics
length(periphery.nodes)
length(outside.nodes)
length(nodes)

#############################################################
###        Low degree Outside nodes only connected to the inside              
#############################################################

## As explained later Perifery nodes will have edges rewired within. 
## we loose low degree nodes this way. To recover some lets
## get all outside nodes that have contacts with periefery nodes only and bring them inside. 
## thus lets get these nodes and recover their edges and include them in the inside.edges list


### Note this was worth a try - but didn't yield quite as many outsidenodes that are 
### solemly connected to the inside nodes. Thus we commen this process out 
# tmp <- (outside.nodes%in%dangling.bonds$Person.Id.1 | outside.nodes%in%dangling.bonds$Person.Id.2)
# rownames.dangling.bonds <- rownames(dangling.bonds)
# recover.these.outside.nodes<-NULL
# for(i in 1:length(outside.nodes)){
#   ii <- outside.nodes[i]
#   all.contacts.of.ii <-(contact.new$Person.Id.1%in%ii | contact.new$Person.Id.2%in%ii)
#   tmp <- rownames(contact.new[all.contacts.of.ii,])
#   
#   if(all(tmp%in%rownames.dangling.bonds)){
#     recover.these.outside.nodes<-c(recover.these.outside.nodes,ii)
#   }
#   
#   if(i%%100){
#     p<- paste(i/length(outside.nodes),length(recover.these.outside.nodes)/length(outside.nodes), sep="   ")
#     print(p)
#     }
# }




#############################################################
###        Prepare for Rewiring            
#############################################################

### We will need to rewire the dangling bonds togeher. So if there are 10 dangling bonds, by rewiring these so that they are to and from perifery nodes, we will effectively be connecting these dangling bonds in couples - thus destroying 10 dangling bonds for 5 new bonds. 


### Consider periphery node A that has a dangling bond A-C where node C is outside our sample.  now we will rewire the dangling bond A-C by finding a periphery node B that also has dangling bonds that need to be rewired. We will find node B such that we preseve (in order of importance) 1. income mixing (i.e., income B is similar to that of C) 2. degree of the nodes with dangling bonds (i.e., degree B is similar to that of C) 3. minimize the difference in the bond weight (i.e., the bond weight A-C is similar to that of B-D) 


info.periphery.nodes<- list() 
list.of.outside.nodes<- list()
for(i in 1:length(periphery.nodes)){
  if(i%%100){print(i/length(periphery.nodes))}
  
  ego<- periphery.nodes[i]
  
  d.P1<- dangling.bonds$Person.Id.1%in%ego
  d.P2<- dangling.bonds$Person.Id.2%in%ego
  
  nd.P1 <- dangling.bonds$Person.Id.2[d.P1]
  nd.P2 <- dangling.bonds$Person.Id.1[d.P2]
  
  nd <- unique(c(nd.P1,nd.P2))
  nd <- nd[!(nd%in%nodes)]
  
  list.of.outside.nodes[[as.character(ego)]] <- nd
  
  inc.degree <- length(nd)
  Household.Id <- demog.person[match(ego,demog.person$Person.Id),"Household.Id"]
  Household.Income.Cat<- demog.household[match(Household.Id,
                                               demog.household$Household.Id),
                                         "Household.Income.Cat"]
  Home.Location.Id<- demog.household[match(Household.Id,
                                           demog.household$Household.Id),
                                     "Home.Location.Id"]
  
  zipcode <- location.Id.lookup[match(Home.Location.Id,
                                      location.Id.lookup$Location.Id),
                                "zipcode"]
  info.periphery.nodes[[i]]<- c(Person.Id=ego,inc.degree=inc.degree,
                                Household.Income.Cat=Household.Income.Cat,
                                zipcode =zipcode )
}


info.periphery.nodes<- as.data.frame(do.call("rbind",info.periphery.nodes))
info.periphery.nodes$Person.Id<- as.numeric(as.character(info.periphery.nodes$Person.Id))
info.periphery.nodes$inc.degree<- as.numeric(as.character(info.periphery.nodes$inc.degree))

info.periphery.nodes<-info.periphery.nodes[info.periphery.nodes$inc.degree>0,]


#############################################################
###        Sanity Checks              
#############################################################
tmp <- do.call("c",list.of.outside.nodes) ## concatenate outside node IDs
length(tmp) ### This number should be equal to the next one
sum(info.periphery.nodes$inc.degree) ## sum of degree to the outside
length(outside.nodes) ## However the number of outside nodes can be less than this  
length(unique(tmp)) ### this number should be the same as the number of outside nodes 

### A non zero number of un.matched.rewires is to be expected! 
un.matched.rewires <- info.periphery.nodes$inc.degree[2]-info.periphery.nodes$inc.degree[1]
for(i in 3:nrow(info.periphery.nodes)){
  un.matched.rewires <- info.periphery.nodes$inc.degree[i]-un.matched.rewires
}
un.matched.rewires <- abs(un.matched.rewires)


#############################################################
###        Rewire             
#############################################################


ipn <- info.periphery.nodes
ipn$Household.Income.Cat<-as.numeric(as.character(ipn$Household.Income.Cat))
ipn<- ipn[order(ipn$Household.Income.Cat),]
ipn$inc.zip<- as.numeric(as.character(ipn$Household.Income.Cat))+as.numeric(as.character(ipn$zipcode))/10^5


lon<- list.of.outside.nodes

reassignment<- list()
n<- 0
for(i in 1:nrow(info.periphery.nodes)){
  if(i%%1000){print(i/length(periphery.nodes))}
  
  ego <- info.periphery.nodes$Person.Id[i]
  outside.alters <- lon[[as.character(ego)]]
  
  all.but.ego <- !(ipn$Person.Id%in%ego)
  ipn.tmp <- ipn[all.but.ego,]
  list.of.new.inside.alter<- NULL
  
  if(length(outside.alters)>0 & nrow(ipn.tmp)>0){
    k.max <- min(length(outside.alters), nrow(ipn.tmp))
    for(k in 1:k.max){
      o.alter <-outside.alters[k]
      
      o.Household.Id <- demog.person[match(o.alter,demog.person$Person.Id),
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
      
      ### Find a periphery.node with similar income
      j<-which.is.max(-abs( o.inc.zip -ipn.tmp$inc.zip))
      new.inside.alter <- ipn.tmp[j,"Person.Id"]
      list.of.new.inside.alter<- c(list.of.new.inside.alter,new.inside.alter)
      ipn.tmp<-ipn.tmp[-j,]
      
      lon[[as.character(ego)]] <- lon[[as.character(ego)]][-1]
      lon[[as.character(new.inside.alter)]] <- lon[[as.character(new.inside.alter)]][-1]
      
      n<- n+1
      reassignment[[n]]<-
        c(ego=ego, o.alter=o.alter, new.inside.alter= new.inside.alter)
    }
    
    ipn[ipn$Person.Id%in%ego,"inc.degree"]<-ipn[ipn$Person.Id%in%ego,"inc.degree"]-length(list.of.new.inside.alter)
    ipn[ipn$Person.Id%in%list.of.new.inside.alter,"inc.degree"]<-ipn[ipn$Person.Id%in%
                                                                       list.of.new.inside.alter,"inc.degree"]-1
    ipn<-ipn[ipn$inc.degree>0,]
  }
}

### Do residual. 
rows.to.sampl.from <-1:nrow(info.periphery.nodes)
for(i in 1:nrow(ipn)){
  ego<-ipn[i,"Person.Id"]
  num.edges.to.sample <- round(ipn[i,"inc.degree"]/2,0)
  
  j<- match(ego,info.periphery.nodes$Person.Id) 
  
  j <- sample(rows.to.sampl.from[-j], num.edges.to.sample)
  
  o.alter<- lon[[as.character(ego)]]
  new.inside.alter<- info.periphery.nodes[j,"Person.Id"]
  
  for(k in 1:num.edges.to.sample){
    n<- n+1
    reassignment[[n]]<-
      c(ego=ego, o.alter=o.alter[k], new.inside.alter= new.inside.alter[k])
  }
}

reassignment<- as.data.frame(do.call("rbind",reassignment))
reassignment$tmp <- paste(reassignment$ego,reassignment$new.inside.alter,sep="-")

nrow(reassignment)
length(unique(reassignment$tmp))


### Test 
# 
# Person.ID.set<-reassignment[16951,1:3]
# Person.Id<- reassignment[16951,3]
# 
# for(Person.Id in Person.ID.set){
#   o.Household.Id <- demog.person[match(Person.Id,demog.person$Person.Id),
#                                  "Household.Id"]
#   o.Household.Income.Cat<- demog.household[match(o.Household.Id,
#                                                  demog.household$Household.Id),
#                                            "Household.Income.Cat"]
#   o.Home.Location.Id<- demog.household[match(o.Household.Id,
#                                              demog.household$Household.Id),
#                                        "Home.Location.Id"]
#   
#   o.zipcode <- location.Id.lookup[match(o.Home.Location.Id,
#                                         location.Id.lookup$Location.Id),
#                                   "zipcode"]
#   
#   o.inc.zip <- o.Household.Income.Cat+as.numeric(o.zipcode)/10^5
#   
#   print(o.inc.zip)
#   #print(paste(o.Household.Income.Cat,o.zipcode),sep=" ")
#   
# }
# 

#############################################################
###        Pass over the weights             
#############################################################

correct.ordering <- (reassignment$ego>reassignment$o.alter)

reassignment[correct.ordering,"tmp"]<- paste(reassignment$ego,reassignment$o.alter,sep="-")[correct.ordering]

reassignment[!correct.ordering,"tmp"]<- paste(reassignment$o.alter,reassignment$ego,sep="-")[!correct.ordering]

dangling.bonds$tmp<- paste(dangling.bonds$Person.Id.1,dangling.bonds$Person.Id.2,sep="-") 

table(reassignment$tmp%in%dangling.bonds$tmp)
table(dangling.bonds$tmp%in%reassignment$tmp)


pass.over.weight<-dangling.bonds[match(reassignment$tmp,dangling.bonds$tmp),"weight"]

reassignment$weight<- pass.over.weight

#############################################################
###        Merge with inside Edges             
#############################################################

reassignment.details<-reassignment

reassignment<- reassignment[,c("ego","new.inside.alter","weight")]
colnames(reassignment) <- colnames(inside.edges) 

correct.ordering <- (reassignment$Person.Id.1>reassignment$Person.Id.2)

tmp <- reassignment[!correct.ordering,]
tmp <- tmp[,c(2,1,3)]
colnames(tmp)<- colnames(inside.edges) 

reassignment <- rbind(tmp,reassignment[correct.ordering,])

dim(inside.edges)

dim(reassignment)


contact.new.sub.rewired<- rbind(inside.edges,reassignment)

### Sanity Check
length(nodes)
length(unique(c(contact.new.sub.rewired$Person.Id.1,contact.new.sub.rewired$Person.Id.2)))


### Lets fudge this to get the right degeee dist. 

tmp <-sample(1:nrow(reassignment),size=1*nrow(reassignment)/3)
fudged.reassignment<- reassignment[tmp,]
contact.new.sub.rewired<- rbind(inside.edges,fudged.reassignment)

#############################################################
###                                                       ###
###        Do Mixing Matrix            
###                                                       ###
#############################################################


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

Person.Id.1_details<- get.portland.network.demo.details(contact.new.sub.rewired$Person.Id.1)
Person.Id.2_details<- get.portland.network.demo.details(contact.new.sub.rewired$Person.Id.2)

Household.Income.Cat.contact.new.sub.rewired <- cbind(
  as.numeric(as.character(Person.Id.1_details$Household.Income.Cat)),
  as.numeric(as.character(Person.Id.2_details$Household.Income.Cat)))

table.Income.Cat.contact.new.sub.rewired<- table(Household.Income.Cat.contact.new.sub.rewired[,1],Household.Income.Cat.contact.new.sub.rewired[,2])
tmp <- apply(table.Income.Cat.contact.new.sub.rewired, MARGIN=1, FUN = function(x){return(round(x/sum(x),2))})
table.Income.Cat.contact.new.sub.rewired <- t(tmp)

round((table.Income.Cat.contact.new.sub.rewired-table.Income.Cat.contact.new)^2/table.Income.Cat.contact.new,2)


#############################################################
###       Plot degree dist             
#############################################################
g.small <- graph.data.frame(contact.new.sub.rewired, directed=FALSE, vertices=NULL)
### remove single dyads and non-conected
tmp <- clusters(g.small)$membership
tmp <- names(tmp)[tmp==1]
g.small<- induced_subgraph(g.small, tmp)

dist<- degree_distribution(g.small)
summary(degree(g.small))
text.label <- summary(degree(g.small))
tmp <- eigen_centrality(g.small)
summary(tmp$vector)
dist <- cbind(x=c(1:length(dist)), y=dist)
dist<- as.data.frame(dist)


### colors used magenta4, navy, firebrick4
degree.dist.plot <-ggplot(dist) +
  geom_line(aes(x=x, y=y),alpha=1,size=2, color="firebrick4")+
  xlab("Degree") +
  ylab("Prob Density")+
  ggtitle(paste("Degree Distribution for the Reduced Portland Data with", vcount(g.small), "nodes and", ecount(g.small),"edges",sep=" "))+
  xlim(0,120)+
  ylim(0,0.045)+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=14 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))+
  annotate("text", label = c(paste(names(text.label),text.label, sep="=")),
           x=100,y=0.02+c(1:6)*0.004, size = 8, colour = "black",angle = 0)

print(degree.dist.plot)
ggsave(degree.dist.plot,
       file=paste(figures.dir,"Porland_Degree_dist_ReducedContacts10000Nodes"
                  ,".pdf",sep=""),  width=10, height=5)

head(contact.new.sub.rewired)

Person.ID.set <- unique(c(contact.new.sub.rewired$Person.Id.1,contact.new.sub.rewired$Person.Id.2 ))

demog.person.sub <- demog.person[demog.person$Person.Id%in%Person.ID.set,]

demog.person.sub <- merge(demog.person.sub, 
                          demog.household[,c("Household.Id","lon","lat")],by="Household.Id" )
text.label <- summary(signif(demog.person.sub$Income,2))

income.plot<-ggplot(demog.person.sub) +
  geom_histogram(aes(x=Income,y=100*..count../sum(..count..)),bins = 100,color="black",fill="firebrick4") +
  theme_bw() +
  xlim(0,250000)+
  xlab("Income ($)") +
  ylab("Percent of Households")+
  ggtitle(paste("Income Distribution for the Reduced Portland Data with", vcount(g.small), "nodes and", ecount(g.small),"edges",sep=" "))+
  theme(axis.text.x=element_text(size=16 ), 
        axis.text.y=element_text(size=16 ) ,
        title = element_text( size=14 ) ,
        strip.text=element_text( size=12 ) ,
        axis.title.x = element_text( size=18 ) ,
        axis.title.y = element_text( size=18 ),
        legend.text = element_text( size = 12))+
  annotate("text", label = c(paste(names(text.label),text.label, sep="=")),
           x=100000,y=1.5+c(1:6)*0.5, size = 8, colour = "black",angle = 0)

print(income.plot)
ggsave(income.plot,
       file=paste(figures.dir,"Porland_HouseHold_Income_dist_ReducedContacts10000Nodes",".pdf",sep=""),  width=10, height=5)

saveRDS(contact.new.sub.rewired ,file="../Construct the Population/ReducedContacts10000Nodes.RData")

