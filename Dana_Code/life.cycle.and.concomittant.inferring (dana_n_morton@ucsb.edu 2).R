setwd("C://Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/")

library(stringr)
library(data.table)
library(dplyr)


## for each trophically transmitted parasite:
    #create list of that parasite's hosts
    #then create lists of that host's prey
    #compare prey of hosts to see which prey are shared
    #compare taxonomy of shared prey with known hosts for parasite or parasite group

edge <- read.csv("all.edges.no.zeros.w.sp.IDs.csv", header=T)
nodes <- read.csv("Nodes.w.Sp.IDs.meta.data.csv", header=T)

# # subset edge list for Andracantha.phalacrocoracis.496.4
# edge.subset <- subset.data.frame(edge, Consumer.Sp.stage == 496.4) 
# 
# #subset full edge list to only include the hosts of the parasite
# edges.prey <- setDT(edge)[Consumer.Sp.stage %in% edge.subset$Resource.Sp.stage]
# 
# #tally number of hosts for each prey species
# counts <- count(edges.prey, resourceSP, resourceName)
# 
# 
# 
# 
# # subset edge list for Corynosoma.strumosum.497.4
# sp = 497.4
# edge.subset <- subset.data.frame(edge, Consumer.Sp.stage == sp) 
# 
# #subset full edge list to only include the hosts of the parasite
# edges.prey <- setDT(edge)[Consumer.Sp.stage %in% edge.subset$Resource.Sp.stage]
# 
# #tally number of hosts for each prey species
# counts <- count(edges.prey, resourceSP, resourceName)
# 


#create list of all parasite IDs
ps.edges <- subset.data.frame(edge, trophic.type == 12) #edge list of only trophically transmitted parasites
ps.nodes <- setDT(nodes)[Node.ID %in% ps.edges$consumers] #node list of only trophically transmitted parasites
psites <- ps.nodes$Sp.stage

write.csv(ps.edges, "trophically.transmitted.parasite.edges.csv", row.names=F)
write.csv(ps.nodes, "trophically.transmitted.parasite.nodes.csv", row.names = F)

dir2 <- "C://Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/Prey.of.Hosts/"

#loop to tabulate for all TT parasites

for (i in psites) {
# subset edge list
sp = i   #will set sp as Sp.stage ID number at position i from psites list
edge.subset <- subset.data.frame(edge, Consumer.Sp.stage == sp) 

#subset full edge list to only include the hosts of the parasite
edges.prey <- setDT(edge)[Consumer.Sp.stage %in% edge.subset$Resource.Sp.stage]

#tally number of hosts for each prey species
counts <- count(edges.prey, resources, resourceName, resourceSP, Resource.Sp.stage)

#add some useful info to counts file
counts$Family <- nodes$Family.x[match(counts$resources,nodes$Node.ID)]
counts$Group <- nodes$Organismal.Group[match(counts$resources,nodes$Node.ID)]
counts$WorkingGroup <- nodes$Working.Group[match(counts$resources,nodes$Node.ID)]
counts <- counts[c(1,3,4,7,8,6,2,5)]
p <- nodes$Name.Sp.stage[nodes$Sp.stage==i]  #grab parasite name for file name
p <- gsub("\\?", "", p)  #remove random special characters that will interfere with file saving
p <- gsub("\\/", "_", p)

write.table(counts, file = paste(dir2,"Prey.of.hosts.of.",p,".csv",sep=""), 
            sep = ",", row.names = F)
}





## loop to tabulate all predators of parasite hosts (for inferring both concomittant predation and additional hosts)

#  note which edge and node lists are used

edge <- read.csv("all.edges.no.zeros.w.sp.IDs.V16.csv", header=T)
nodes <- read.csv("Nodes.w.Sp.IDs.meta.data.V16.csv", header=T)

#create list of all parasite IDs
ps.edges <- subset.data.frame(edge, trophic.type == 12) #edge list of only trophically transmitted parasites
ps.nodes <- setDT(nodes)[Node.ID %in% unique(ps.edges$consumers)] #node list of only trophically transmitted parasites
psites <- ps.nodes$Sp.stage

dir3 <- "/Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/Life.Cycles.Inferred.Predators.of.Hosts/"

for (i in psites) {
  # subset edge list
  #will set sp as Sp.stage ID number at position i from psites list
  edge.subset <- subset.data.frame(edge, Consumer.Sp.stage == i) 
  
  #subset full edge list to only include hosts as resources
  edges.prey <- setDT(edge)[Resource.Sp.stage %in% edge.subset$Resource.Sp.stage]
  #edges.prey <- subset.data.frame(edges.prey, trophic.type == 1)
  
  #tally number of predators for each host species and aggregate prey (hosts) to column
 counts <- count(edges.prey, consumers, consumerName, Consumer.Sp.stage, consumerSP)
 hosts <-  aggregate(resourceName~consumers, data= edges.prey, FUN=paste, collapse= "; ")
 counts <- right_join(hosts,counts, by = "consumers", keep = T)
 
  #add some useful info to counts file
  counts$Family <- nodes$Family.x[match(counts$consumers,nodes$Node.ID)]
  counts$Group <- nodes$Organismal.Group[match(counts$consumers,nodes$Node.ID)]
  counts$WorkingGroup <- nodes$Working.Group[match(counts$consumers,nodes$Node.ID)]
  counts$Type <- nodes$Type[match(counts$consumers,nodes$Node.ID)]
  
  counts <- counts[c(10,8,9,7,1,3,4,5,2,6)] #reorder for easier viewing
  
  p <- nodes$Name.Sp.stage[nodes$Sp.stage==i]  #grab parasite name for file name
  p <- gsub("\\?", "", p)  #remove random special characters that will interfere with file saving
  p <- gsub("\\/", "_", p)
    
  write.table(counts, file = paste(dir3,"Predators.of.hosts.of.",p,".csv",sep=""), 
              sep = ",", row.names = F)
 
  }



######

## Generate edge list of concomittant links on adult parasites: when parasite is adult, automatically assume concomittant predation if host is eaten. 
##creates new edge list with parasite as resource, predator of host as conusmer. 

#note which edge and node lists are used
edge <- read.csv("all.edges.no.zeros.w.sp.IDs.V16.csv", header=T)
nodes <- read.csv("Nodes.w.Sp.IDs.meta.data.V16.csv", header=T)

#create list of all parasite IDs
ps.edges <- subset.data.frame(edge, trophic.type == 12) #edge list of only trophically transmitted parasites
ps.nodes <- setDT(nodes)[Node.ID %in% unique(ps.edges$consumers)] #node list of only trophically transmitted parasites

ps.adults <- subset.data.frame(ps.nodes, stageID == 1)  #select stage of parasite adult parasites, for concomittant links

psites <- ps.adults$Sp.stage

dir4 <- "/Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/Concomittant.edges/"

N = ps.edges[FALSE,]  #set up empty data frame to bind to

for (i in psites) {
  # subset edge list
  sp = i   #will set sp as Sp.stage ID number at position i from psites list
  edge.subset <- subset.data.frame(edge, Consumer.Sp.stage == sp) 
  
  #subset full edge list to only include hosts as resources
  edges.prey <- setDT(edge)[Resource.Sp.stage %in% edge.subset$Resource.Sp.stage]
  edges.prey <- subset.data.frame(edges.prey, trophic.type == 1)
  n = as.numeric(nrow(edges.prey))
  if (n == 0) next
 
#collect info to be put into new edges   
 #collapse refs and localities for each predator 
refs<-  aggregate(reference~consumers, data= edges.prey, FUN=paste, collapse= "; ")
localities <- aggregate(locality~consumers, data= edges.prey, FUN=paste, collapse= "; ")

#average the confidence in the host-parasite link and the predator-host link
#round up to nearest integer
#choose the minimum give best (lowest numerical) confidence out of all predator-host links
edges.prey$psite.conf <- edge.subset$confidence[match(edges.prey$resources, edge.subset$resources)]
edges.prey$conf <- ceiling((edges.prey$psite.conf + edges.prey$confidence)/2)
confidence <- aggregate(conf~consumers, data= edges.prey, FUN=min)


confidence$justification <- 11  #add column with justification, see metadata for codes

#join lists
new.links <- left_join(refs, localities, by= "consumers", keep = T)
new.links <- left_join(confidence, new.links, by = "consumers", keep =T)

#add consumer ID columns
new.links$consumerName <- edges.prey$consumerName[match(new.links$consumers,edges.prey$consumers)]
new.links$Consumer.Sp.stage <- edges.prey$Consumer.Sp.stage[match(new.links$consumers,edges.prey$consumers)]
new.links$consumerSP <- edges.prey$consumerSP[match(new.links$consumers,edges.prey$consumers)]
new.links$consumerStage <- edges.prey$consumerStage[match(new.links$consumers,edges.prey$consumers)]
new.links$trophic.type <- 14  #for concomittant predation, see metadata for codes

## grab parasite info
new.links$resources <- unique(edge.subset$consumers)
new.links$resourceName <- unique(edge.subset$consumerName)
new.links$Resource.Sp.stage <- unique(edge.subset$Consumer.Sp.stage)
new.links$resourceSP <- unique(edge.subset$consumerSP)
new.links$resourceStage <- unique(edge.subset$consumerStage)

#reorder so matches other edge lists
new.links <- new.links[c(1,6,7,8,9,11,12,13,14,15,10,2,3,4,5)]

  #to write each file individually
p <- nodes$Name.Sp.stage[nodes$Sp.stage==i]  #grab parasite name for file name
p <- gsub("\\?", "", p)  #remove random special characters that will interfere with file saving
p <- gsub("\\/", "_", p)
write.table(new.links, file = paste(dir4,"Concomittant.edges.of.",p,".xls",sep=""),
sep="\t",dec='.', row.names = F)
  
 N <-  rbind(N, new.links)  #bind new edge list


}
write.table(N, file = paste(dir4,"Concomittant.edges.adult.parasites.xls",sep=""),
            sep="\t",dec='.', row.names = F)


### repeat this process for different larval stages and either enter concomittant links, or new parasite-host links

#### for links that need to be reviewed as either concomittant, or potential inferred hosts

#note which edge and node lists are used
edge <- read.csv("all.edges.no.zeros.w.sp.IDs.V16.csv", header=T)
nodes <- read.csv("Nodes.w.Sp.IDs.meta.data.V16.csv", header=T)

#create list of all parasite IDs
ps.edges <- subset.data.frame(edge, trophic.type == 12) #edge list of only trophically transmitted parasites
ps.nodes <- setDT(nodes)[Node.ID %in% unique(ps.edges$consumers)] #node list of only trophically transmitted parasites



ps.larvae <- subset.data.frame(ps.nodes, stageID != 1)  #select stage of parasite adult parasites, for concomittant links

psites <- ps.larvae$Sp.stage

dir5 <- "/Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/Possible.Concomittant.edges/"

N = ps.edges[FALSE,]  #set up empty data frame to bind to

for (i in psites) {
  # subset edge list
  sp = i   #will set sp as Sp.stage ID number at position i from psites list
  edge.subset <- subset.data.frame(edge, Consumer.Sp.stage == sp) 
  
  #subset full edge list to only include hosts as resources
  edges.prey <- setDT(edge)[Resource.Sp.stage %in% edge.subset$Resource.Sp.stage]
  edges.prey <- subset.data.frame(edges.prey, trophic.type == 1)
  n = as.numeric(nrow(edges.prey))
  if (n == 0) next
  
  #collect info to be put into new edges   
  #collapse refs and localities for each predator 
  refs<-  aggregate(reference~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  localities <- aggregate(locality~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  
  #pick lowest confidence (highest value) confidence between host-parasite link and the predator-host link
  #the choose the minimum give best (lowest numerical) confidence out of all predator-host links
  edges.prey$psite.conf <- edge.subset$confidence[match(edges.prey$resources, edge.subset$resources)]
  edges.prey <- transform(edges.prey, conf = pmax(psite.conf,confidence))
  confidence <- aggregate(conf~consumers, data= edges.prey, FUN=min)
  
  
  confidence$justification <- 11  #add column with justification, see metadata for codes
  
  #join lists
  new.links <- left_join(refs, localities, by= "consumers", keep = T)
  new.links <- left_join(confidence, new.links, by = "consumers", keep =T)
  
  #add consumer ID columns
  new.links$consumerName <- edges.prey$consumerName[match(new.links$consumers,edges.prey$consumers)]
  new.links$Consumer.Sp.stage <- edges.prey$Consumer.Sp.stage[match(new.links$consumers,edges.prey$consumers)]
  new.links$consumerSP <- edges.prey$consumerSP[match(new.links$consumers,edges.prey$consumers)]
  new.links$consumerStage <- edges.prey$consumerStage[match(new.links$consumers,edges.prey$consumers)]
  new.links$trophic.type <- 14  #for concomittant predation, see metadata for codes
  
  ## grab parasite info
  new.links$resources <- unique(edge.subset$consumers)
  new.links$resourceName <- unique(edge.subset$consumerName)
  new.links$Resource.Sp.stage <- unique(edge.subset$Consumer.Sp.stage)
  new.links$resourceSP <- unique(edge.subset$consumerSP)
  new.links$resourceStage <- unique(edge.subset$consumerStage)
  
  
  #reorder so matches other edge lists
  new.links <- new.links[c(1,6,7,8,9,11,12,13,14,15,10,2,3,4,5)]
  
  #to write each file individually
  p <- nodes$Name.Sp.stage[nodes$Sp.stage==i]  #grab parasite name for file name
  p <- gsub("\\?", "", p)  #remove random special characters that will interfere with file saving
  p <- gsub("\\/", "_", p)
  write.table(new.links, file = paste(dir5,"Possible.Concomittant.edges.of.",p,".xls",sep=""),
              sep="\t",dec='.', row.names = F)
  
  N <-  rbind(N, new.links)  #bind new edge list
  
  
}
write.table(N, file = paste(dir5,"Concomittant.edges.to.review.larval.parasites.xls",sep=""),
            sep="\t",dec='.', row.names = F)



####for parthenitae and other castrators
### for links that need to be reviewed as either concomittant, or potential inferred hosts

#note which edge and node lists are used
edge <- read.csv("all.edges.no.zeros.w.sp.IDs.V16.csv", header=T)
nodes <- read.csv("Nodes.w.Sp.IDs.meta.data.V16.csv", header=T)

#create list of all parasite IDs
ps.edges <- subset.data.frame(edge,trophic.type == 4) #edge list of only castrators parasites
ps.nodes <- setDT(nodes)[Node.ID %in% unique(ps.edges$consumers)] #node list of only trophically transmitted parasites


psites <- ps.nodes$Sp.stage

dir6 <- "/Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/concomittant.edges.parthenitae/"

N = ps.edges[FALSE,]  #set up empty data frame to bind to

for (i in psites) {
  # subset edge list
  sp = i   #will set sp as Sp.stage ID number at position i from psites list
  edge.subset <- subset.data.frame(edge, Consumer.Sp.stage == sp) 
  
  #subset full edge list to only include hosts as resources
  edges.prey <- setDT(edge)[Resource.Sp.stage %in% edge.subset$Resource.Sp.stage]
  edges.prey <- subset.data.frame(edges.prey, trophic.type == 1)
  n = as.numeric(nrow(edges.prey))
  if (n == 0) next
  
  #collect info to be put into new edges   
  #collapse refs and localities for each predator 
  refs<-  aggregate(reference~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  localities <- aggregate(locality~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  
  #pick lowest confidence (highest value) confidence between host-parasite link and the predator-host link
  #the choose the minimum give best (lowest numerical) confidence out of all predator-host links
  edges.prey$psite.conf <- edge.subset$confidence[match(edges.prey$resources, edge.subset$resources)]
  edges.prey <- transform(edges.prey, conf = pmax(psite.conf,confidence))
  confidence <- aggregate(conf~consumers, data= edges.prey, FUN=min)
  
  
  confidence$justification <- 11  #add column with justification, see metadata for codes
  
  #join lists
  new.links <- left_join(refs, localities, by= "consumers", keep = T)
  new.links <- left_join(confidence, new.links, by = "consumers", keep =T)
  
  #add consumer ID columns
  new.links$consumerName <- edges.prey$consumerName[match(new.links$consumers,edges.prey$consumers)]
  new.links$Consumer.Sp.stage <- edges.prey$Consumer.Sp.stage[match(new.links$consumers,edges.prey$consumers)]
  new.links$consumerSP <- edges.prey$consumerSP[match(new.links$consumers,edges.prey$consumers)]
  new.links$consumerStage <- edges.prey$consumerStage[match(new.links$consumers,edges.prey$consumers)]
  new.links$trophic.type <- 14  #for concomittant predation, see metadata for codes
  
  ## grab parasite info
  new.links$resources <- unique(edge.subset$consumers)
  new.links$resourceName <- unique(edge.subset$consumerName)
  new.links$Resource.Sp.stage <- unique(edge.subset$Consumer.Sp.stage)
  new.links$resourceSP <- unique(edge.subset$consumerSP)
  new.links$resourceStage <- unique(edge.subset$consumerStage)
  
  
  #reorder so matches other edge lists
  new.links <- new.links[c(1,6,7,8,9,11,12,13,14,15,10,2,3,4,5)]
  colnames(new.links)[12] <- "confidence"
  
  #to write each file individually
  p <- nodes$Name.Sp.stage[nodes$Sp.stage==i]  #grab parasite name for file name
  p <- gsub("\\?", "", p)  #remove random special characters that will interfere with file saving
  p <- gsub("\\/", "_", p)
  write.table(new.links, file = paste(dir6,"Possible.Concomittant.edges.of.",p,".xls",sep=""),
              sep="\t",dec='.', row.names = F)
  
 N <-  rbind(N, new.links)  #bind new edge list
  
  
}
write.table(N, file = paste(dir6,"Concomittant.edges.to.review.castrators.xls",sep=""),
            sep="\t",dec='.', row.names = F)


### for links that need to be reviewed as either concomittant, or potential inferred hosts

#note which edge and node lists are used
edge <- read.csv("all.edges.no.zeros.w.sp.IDs.V17.w.inferred.paratenic.csv", header=T)
nodes <- read.csv("Nodes.w.Sp.IDs.meta.data.V17.csv", header=T)

#create list of all parasite IDs
C <-  25  #enter number of trophic interaciton of interest
ps.edges <- subset.data.frame(edge,trophic.type == C) #indicate type(s) of trophic interaction. see metadata for codes
ps.nodes <- setDT(nodes)[Node.ID %in% unique(ps.edges$consumers)] #node list of only trophically transmitted parasites


psites <- ps.nodes$Sp.stage

dir6 <- "/Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/concomittant.edges.the.rest/"

N = ps.edges[FALSE,]  #set up empty data frame to bind to

for (i in psites) {
  # subset edge list
  sp = i   #will set sp as Sp.stage ID number at position i from psites list
  edge.subset <- subset.data.frame(edge, Consumer.Sp.stage == sp) 
  edge.subset <- subset.data.frame(edge.subset, trophic.type == C) 
  
  #subset full edge list to only include hosts as resources
  edges.prey <- setDT(edge)[Resource.Sp.stage %in% edge.subset$Resource.Sp.stage]
  edges.prey <- subset.data.frame(edges.prey, trophic.type %in% c(1,3) )
  n = as.numeric(nrow(edges.prey))
  if (n == 0) next
  
  #collect info to be put into new edges   
  #collapse refs and localities for each predator 
  refs<-  aggregate(reference~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  localities <- aggregate(locality~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  
  #pick lowest confidence (highest value) confidence between host-parasite link and the predator-host link
  #the choose the minimum give best (lowest numerical) confidence out of all predator-host links
  edges.prey$psite.conf <- edge.subset$confidence[match(edges.prey$resources, edge.subset$resources)]
  edges.prey <- transform(edges.prey, conf = pmax(psite.conf,confidence))
  confidence <- aggregate(conf~consumers, data= edges.prey, FUN=min)
  
  
  confidence$justification <- 11  #add column with justification, see metadata for codes
  
  #join lists
  new.links <- left_join(refs, localities, by= "consumers", keep = T)
  new.links <- left_join(confidence, new.links, by = "consumers", keep =T)
  
  #add consumer ID columns
  new.links$consumerName <- edges.prey$consumerName[match(new.links$consumers,edges.prey$consumers)]
  new.links$Consumer.Sp.stage <- edges.prey$Consumer.Sp.stage[match(new.links$consumers,edges.prey$consumers)]
  new.links$consumerSP <- edges.prey$consumerSP[match(new.links$consumers,edges.prey$consumers)]
  new.links$consumerStage <- edges.prey$consumerStage[match(new.links$consumers,edges.prey$consumers)]
  new.links$trophic.type <- 14  #for concomittant predation, see metadata for codes
  
  ## grab parasite info
  new.links$resources <- unique(edge.subset$consumers)
  new.links$resourceName <- unique(edge.subset$consumerName)
  new.links$Resource.Sp.stage <- unique(edge.subset$Consumer.Sp.stage)
  new.links$resourceSP <- unique(edge.subset$consumerSP)
  new.links$resourceStage <- unique(edge.subset$consumerStage)
  
  
  #reorder so matches other edge lists
  new.links <- new.links[c(1,6,7,8,9,11,12,13,14,15,10,2,3,4,5)]
  colnames(new.links)[12] <- "confidence"
  
  #to write each file individually
  p <- nodes$Name.Sp.stage[nodes$Sp.stage==i]  #grab parasite name for file name
  p <- gsub("\\?", "", p)  #remove random special characters that will interfere with file saving
  p <- gsub("\\/", "_", p)
  write.table(new.links, file = paste(dir6,"Concomittant.edges.of.",p,".xls",sep=""),
              sep="\t",dec='.', row.names = F)
  
  N <-  rbind(N, new.links)  #bind new edge list
  
  
}
write.table(N, file = paste(dir6,"Concomittant.edges.Type.",C,".xls",sep=""),
            sep="\t",dec='.', row.names = F)



#### to bind all files of concomittant links into one
folder = "/Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/Concomittant.to.bind/"
files = list.files(folder, pattern = "*.csv", full.names=T)

tables <- lapply(files, read.csv, header = TRUE, stringsAsFactors = FALSE)


combined.df <- do.call(rbind , tables)

write.table(combined.df, file = paste("/Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/Concomittant.Edges.xls"), 
            sep = "\t", row.names = F)


#bind concomittant edge list with full edge list
edge <- read.csv("all.edges.no.zeros.w.sp.IDs.V17.w.inferred.paratenic.csv", header=T)
conc <- read.csv("Concomittant.Edges.V17.csv", header=T)

edge.all <- rbind(edge,conc)

# write.table(edge.all, file = paste("/Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/Data_Current_Compiled/all.Edges.w.concomittant.V17.xls"), 
            sep = "\t", row.names = F)
