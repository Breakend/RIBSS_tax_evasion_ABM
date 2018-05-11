remove(list = ls(all = TRUE))
gc()

# Changing the directory to the one where this script exists
# This section should not be executed (because it does not work) 
# if you are running this script interactively
#source.dir <- dirname(sys.frame(1)$ofile)
#setwd(source.dir)


### Load Libraries: please ensure you have installed these R packages.
library(ggplot2)
library(Hmisc)
library(igraph)
library(xlsx)
library(reshape2)
library(timeDate)
library(Rcpp)
library(rgdal)
library(zipcode)
library(ggmap)
library(rgeos)
library(plyr)
library(RColorBrewer)
library(grid)
library(nnet)
library(maptools)

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

library.dir   <- "Library/"
library <- file.path(library.dir, "library.R")
source(library)



#############################################################
###                                                       ###
###        Load Google maps of Porland              
###                                                       ###
#############################################################

maps.dir <- "Network Data/"
PortlandMap.file<- paste(maps.dir,"PortlandMap.RData",sep="")
if(file.exists(PortlandMap.file)){
  PortlandMap<- readRDS(file=PortlandMap.file)
}else{
  ### Note this is blocked by RAND's firewall - need to step outside RAND.
  PortlandMap<- list()
  PortlandMap[["9"]] <- get_map(location = 'Portland', zoom = 9)
  PortlandMap[["10"]]<-  get_map(location = 'Portland', zoom = 10)
  PortlandMap[["11"]]<-  get_map(location = 'Portland', zoom = 11)
  PortlandMap[["12"]]<-  get_map(location = 'Portland', zoom = 12)
  saveRDS(PortlandMap,file=PortlandMap.file)
}


#############################################################
###                                                       ###
###        Load Shape files of Portland              
###                                                       ###
#############################################################

P.df<-readRDS(file=paste(maps.dir,"Portland.Zip.Code.Boundaries.RData",sep=""))


ggmap(PortlandMap[["10"]]) + 
  geom_polygon(aes(fill = postcode.values, x = long, y = lat, group = group), 
               data = Portland.df,
               alpha = 0.6, 
               color = "black",
               size = 0.2)+
  geom_point(data = tmp.demog.household, aes(x=lon,y=lat,color = Vehicles),  size = 0.5, alpha = 0.1)+
    scale_fill_continuous(guide = FALSE) +
  scale_color_continuous(guide = FALSE) 



ggmap(PortlandMap[["10"]]) + 
  geom_polygon(aes(fill = Income, x = long, y = lat, group = group), 
               data = P.df,
               alpha = 0.75, 
               color = "black",
               size = 0.4)+
  geom_point(data = demog.household, 
             aes(x=lon,y=lat),  
             size = 0.05, alpha = 0.1,color="magenta4")+
  scale_fill_continuous(guide = FALSE)+
  scale_color_continuous(guide = FALSE)

#############################################################
###                                                       ###
###        Plot reduced set of Households            
###                                                       ###
#############################################################


PN460.demog.person<- readRDS(file=paste(maps.dir,"PN460.demog.person.RData",sep=""))
PN1.contact<- readRDS(file=paste(maps.dir,"PN1.contact.RData",sep=""))
PN10.contact<- readRDS(file=paste(maps.dir,"PN10.contact.RData",sep=""))

Person.ID.set <- unique(c(PN1.contact$Person.Id.1,PN1.contact$Person.Id.2 ))

demog.person.sub <- PN460.demog.person[PN460.demog.person$Person.Id%in%Person.ID.set,]


ggmap(PortlandMap[["10"]]) + 
  geom_polygon(aes(fill = Income, x = long, y = lat, group = group), 
               data = P.df,
               alpha = 0.75, 
               color = "black",
               size = 0.4)+
  geom_point(data = demog.person.sub, 
             aes(x=lon,y=lat),  
             size = 0.4, alpha = 1,color="navy")+ 
  ## colors used magenta4, navy, firebrick4
  scale_fill_continuous(guide = FALSE)+
  scale_color_continuous(guide = FALSE)



#############################################################
###                                                       ###
###        Degree Plot             
###                                                       ###
#############################################################


g.small <- graph.data.frame(PN1.contact, directed=FALSE, vertices=NULL)
### remove single dyads and non-conected
tmp <- clusters(g.small )$membership
tmp <- names(tmp)[tmp==1]
g.small <- induced_subgraph(g.small, tmp)

dist<- degree_distribution(g.small)
summary(degree(g.small))
text.label <- summary(degree(g.small))
#tmp <- eigen_centrality(g.small)
#summary(tmp$vector)
dist <- cbind(x=c(1:length(dist)), y=dist)
dist<- as.data.frame(dist)

degree.dist.plot <-ggplot(dist) +
  geom_line(aes(x=x, y=y),alpha=1,size=2, color="navy")+
  xlab("Degree") +
  ylab("Prob Density")+
  ggtitle(paste("Degree Distribution for the Full Portland Data with",vcount(g.small) ,"nodes and", nrow(contact.new),"edges",sep=" "))+
  xlim(0,120)+
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
#ggsave(degree.dist.plot,file=paste(figures.dir,"Porland_Degree_dist_ReducedContacts",".pdf",sep=""),  width=10, height=5)


#############################################################
###                                                       ###
###        Income Plot             
###                                                       ###
#############################################################


text.label <- summary(signif(demog.person.sub$Income,2))

income.plot<-ggplot(demog.person.sub) +
  geom_histogram(aes(x=Income,y=100*..count../sum(..count..)),bins = 100,color="black",fill="navy") +
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
           x=100000,y=2+c(1:6)*0.4, size = 8, colour = "black",angle = 0)

print(income.plot)
#ggsave(income.plot,file=paste(figures.dir,"Porland_HouseHold_Income_dist_ReducedContacts",".pdf",sep=""),  width=10, height=5)

  




