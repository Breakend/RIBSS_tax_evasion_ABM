generate.igraph.networks <- function(contact,demog.person){
  
  set.seed(55279)
#   contact<-readRDS(file="../Construct the Population/PN1.contact.RData")
#   demog.person<-readRDS(file="../Construct the Population/PN460.demog.person.RData")
  
  g <- graph.data.frame(contact, directed=FALSE, vertices=NULL)
  ### remove single dyads and non-conected
  #tmp <- clusters(g )$membership
  #tmp <- names(tmp)[tmp==1]
  #g <- induced_subgraph(g, tmp)
  
  
  Person.ID.set <-names(V(g))
  
  df <- demog.person[,c("Person.Id","Income","zipcode")]
  df<- df[df$Person.Id%in%Person.ID.set,]
  df <- df[!duplicated(df), ]
  df<-df[order(df$zipcode,df$Income,decreasing=TRUE),]
  
  k<-summary(degree(g))
  k.mean<- as.numeric(k["Mean"])

  ### sample 1000 nodes and calculate shortest path between them
  tmp <- sample(V(g),1000,replace=F)
  dd <- distances(g, v =tmp, to = tmp,weights=NA)
  diag(dd) <- NA
  d.mean<- mean(dd,na.rm = T) ### =3.3 for PN10 = 2.64 for PN1

  ### create Ring network with same <k> 
  g.regular <- sample_smallworld(1, vcount(g),round(k.mean/2,0) ,0)
  V(g.regular)$name<- df$Person.Id
  
  N<- vcount(g)
  c<- round(k.mean/2,0)
  
  ### create SW network with same <k> and  same <l> as d.mean ~5.
  ### See eqn 7 of http://www.scholarpedia.org/article/Small-world_network
  p<- 0.08
  if(N== 1195) p <- 0.15
  if(N==10112) p<- 0.155
  g.smallworld <- sample_smallworld(1, N,c ,p=p)
  V(g.smallworld)$name<- df$Person.Id
  
#   tmp <- sample(V(g.smallworld),1000,replace=F)
#   dd <- distances(g.smallworld, v =tmp, to = tmp,weights=NA)
#   diag(dd) <- NA
#   d.mean2<- mean(dd,na.rm = T)
#   d.mean2
  
  
  ### create ER network with same <k> 
  g.erdos.renyi <-erdos.renyi.game(N, k.mean/N, type ="gnp")
  V(g.erdos.renyi)$name<- df$Person.Id
  
  ### create BA network with same <l> as d.mean ~5. power =1.14
  ### for Pn10 Power = 1.26 as d.mean= 3.3
  ### for Pn1 Power = 1.28 as d.mean= 2.6
  g.barabasi <- barabasi.game(N,power=1.38)
  
#     tmp <- sample(V(g.barabasi),1000,replace=F)
#     dd <- distances(g.barabasi, v =tmp, to = tmp,weights=NA)
#     diag(dd) <- NA
#     d.mean2<- mean(dd,na.rm = T)
#     d.mean2
  
  V(g.barabasi)$name<- 1:N
  tmp <-V(g.barabasi)$name[order(degree(g.barabasi),decreasing=TRUE)]
  df<-df[order(df$Income,decreasing=TRUE),]
  V(g.barabasi)$name[tmp]<- df$Person.Id

  
  R <- list(g.empirical=g, 
            g.regular=g.regular, 
            g.smallworld=g.smallworld,
            g.erdos.renyi=g.erdos.renyi,
            g.barabasi=g.barabasi)
  
#  saveRDS(R ,file="../Construct the Population/igraphs1.contact.RData")
  
  return(R)
}