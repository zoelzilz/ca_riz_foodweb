
#round 2 of concomitant link checking. Done after additional links added (FN's, etc)

dir1 <- "/Users/danamorton/Box Sync/Dana Only/Kelp.Forest.Food.Web/"
#dir1 <- "C://Users/Dana/Box Sync/Dana Only/Kelp.Forest.Food.Web/"

setwd(paste(dir1,"Data_Current_Compiled/",sep=""))
      
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)
library(data.table)

#---------------get edges and nodes ready to use-----------------# 

#load master node and edge lists
kelp.nodes.all <- read.csv("Nodes.V22.w.Tax.meta.data.NHM.Query.csv") 
kelp.edge.all <- read.csv("Edge.list.v23.CLEANED.csv", header = T)
colnames(kelp.edge.all)[1] <- "consumers"

###subset the appropriate nodes of interest

#only keep interactions that aren't NOT interactions
kelp.edge.all <- subset.data.frame(kelp.edge.all, trophic.type != 0) 

#subset to remove concomitant links. see metadata for codes
kelp.edge.no.conc <- subset.data.frame(kelp.edge.all, trophic.type != 14) 
#keep only free-living trophic interactions
kelp.free.living <- subset.data.frame(kelp.edge.no.conc, trophic.type %in% c(1,3))

#remove "removed" and protozoan nodes and links
kelp.nodes <- subset.data.frame(kelp.nodes.all, Presence.code !=  "R")
kelp.nodes <- subset.data.frame(kelp.nodes, Presence.code !=  "P")
#remove Non-local nodes and links
kelp.node.local <- subset.data.frame(kelp.nodes, Presence.code != "N")
#remove "extinct" nodes and links
kelp.node.extant <- subset.data.frame(kelp.node.local, Presence.code != "E")

#create free-living only node list
free.nodes <- subset.data.frame(kelp.node.extant, Type != "symbiont")

#creates parasite node list
ps.nodes <- subset.data.frame(kelp.node.extant, Type == "symbiont")

#rename node and edge list to versions you will work with
nodes <- kelp.node.extant
kelp.edge <- kelp.edge.all

#subset edge list to include same sp as node list
kelp.edge <- subset.data.frame(kelp.edge, consumers %in% nodes$Node.ID)
kelp.edge <- subset.data.frame(kelp.edge, resources %in% nodes$Node.ID)



### for links that need to be reviewed as either concomittant, or potential inferred hosts

#specify edge and node lists to be used
edge <- kelp.edge
nodes <- nodes

#create list of all parasite IDs
C <-  26  #enter number of trophic interaciton of interest
ps.edges <- subset.data.frame(edge,trophic.type == C) #indicate type(s) of trophic interaction. see metadata for codes
ps.nodes <- setDT(nodes)[Node.ID %in% unique(ps.edges$consumers)] #node list of only trophically transmitted parasites

psites <- ps.nodes$Sp.stage  #if not specifying a life stage

#or to specify life stage:
# ps.stage <- subset.data.frame(ps.nodes, stageID == 1)  #select stage of parasite adult parasites, for concomittant links
# psites <- ps.stage$Sp.stage

dir6 <- paste(dir1, "/Data_Current_Compiled/Concomitant.Round2/", sep="")

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
  refs<-  aggregate(ref.ID~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  localities <- aggregate(localities~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  
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
  
  N <-  rbind(N, new.links, fill=T)  #bind new edge list
  
}

hyper.ps <- N
write.table(N, file = paste(dir6,"Concomittant.edges.Type.",C,".xls",sep=""),
            sep="\t",dec='.', row.names = F)

combined.links <- rbind(Adult.trophic, castrators, commensal, ecto.commensal, Ectos, facultative.ps, eggpred, hyper.ps, mutualist, parasitoid, pathogen5, true.parasite6)

write.table(combined.links, file = paste(dir6,"Concomittant.edges.combined.links.xls",sep=""),
            sep="\t",dec='.', row.names = F)


#-----now compare list possible concomitant with current concomitant in edge list to look for additions

new.conc <- combined.links
conc.edges <- subset.data.frame(edge, trophic.type == 14)

#finds links that are in both lists and drops the rest
matches<- inner_join(new.conc, conc.edges, by=c("consumerName", "resourceName"), keep=F)

#finds links in new conc list that are NOT in current list
mismatches<- anti_join(new.conc, conc.edges, by=c("consumerName", "resourceName"))

#----- cleaning up the edges to remove duplicate references and localites
library(stringr)
library(splitstackshape)

mismatches$num <- seq.int(nrow(mismatches))

E <- cSplit(mismatches, "ref.ID", ";", direction = "long")
E1 <- cSplit(E, "localities", ";", direction = "long")

collapse_unique <- function(x) {
  paste(unique(x), collapse = "; ")
}

Yay <- aggregate(E1, by=list(E1$num), collapse_unique)

write.table(Yay, file = paste(dir6,"New.concomittant.edges.to.review.xls",sep=""),
            sep="\t",dec='.', row.names = F)



#-------- for larvae trophic stages -----#

#specify edge and node lists to be used
edge <- kelp.edge
nodes <- nodes

#create list of all parasite IDs
C <-  12  #enter number of trophic interaciton of interest
ps.edges <- subset.data.frame(edge,trophic.type == C) #indicate type(s) of trophic interaction. see metadata for codes
ps.nodes <- setDT(nodes)[Node.ID %in% unique(ps.edges$consumers)] #node list of only trophically transmitted parasites


#to specify life stage:
ps.stage <- subset.data.frame(ps.nodes, stageID != 1)  #not-adult parasites
psites <- ps.stage$Sp.stage

dir6 <- paste(dir1, "/Data_Current_Compiled/Concomitant.Round2/", sep="")

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
  refs<-  aggregate(ref.ID~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  localities <- aggregate(localities~consumers, data= edges.prey, FUN=paste, collapse= "; ")
  
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
  
  N <-  rbind(N, new.links, fill=T)  #bind new edge list
  
}

trophic.ps <- N
write.table(N, file = paste(dir6,"Concomittant.edges.Larval.Type.",C,".xls",sep=""),
            sep="\t",dec='.', row.names = F)

# compare lists
new.conc <- trophic.ps
conc.edges <- subset.data.frame(edge, trophic.type == 14)

#finds links that are in both lists and drops the rest
matches<- inner_join(new.conc, conc.edges, by=c("consumerName", "resourceName"), keep=F)

#finds links in new conc list that are NOT in current list
mismatches<- anti_join(new.conc, conc.edges, by=c("consumerName", "resourceName"))

mismatches$num <- seq.int(nrow(mismatches))

E <- cSplit(mismatches, "ref.ID", ";", direction = "long")
E1 <- cSplit(E, "localities", ";", direction = "long")

collapse_unique <- function(x) {
  paste(unique(x), collapse = "; ")
}

Yay1 <- aggregate(E1, by=list(E1$num), collapse_unique)

write.table(Yay1, file = paste(dir6,"New.larval.trophic.concomittant.edges.to.review.xls",sep=""),
            sep="\t",dec='.', row.names = F)


#find conc. links already in web
new.conc <- trophic.ps
conc.edges <- subset.data.frame(edge, trophic.type == 14)


#finds links that are in both lists and drops the rest
matches<- inner_join(new.conc, conc.edges, by=c("consumerName", "resourceName"), keep=F)

#finds links in new conc list that are NOT in current list
mismatches<- anti_join(new.conc, conc.edges, by=c("consumerName", "resourceName"))

mismatches$num <- seq.int(nrow(mismatches))



#find matches that are already in web as 
transmissiontrans.fliped <- transmission.edges[,c(4,9)]
trans.fliped.cons1.res2 <- trans.fliped
colnames(trans.fliped.cons1.res2) <- c("one", "two")

mismatches2 <- mismatches[,c(4,9)]

mismatch.flip <- mismatches2[,c(2,1)]
colnames(mismatch.flip) <- c("one", "two")
mismatch.flip.res1.cons2 <- mismatch.flip

trans.matches <- intersect(trans.fliped.cons1.res2, mismatch.flip.res1.cons2)

trans.matches$trans <- "in web"
colnames(trans.matches)[1:2] <- c("resourceSP", "consumerSP") 


test <- left_join(mismatches, trans.matches, by=c("resourceSP", "consumerSP"), keep=T)

write.table(test, file = paste(dir6,"New.larval.trophic.concomittant.edges.to.check.w.lifecyclematches.xls",sep=""),
            sep="\t",dec='.', row.names = F)
