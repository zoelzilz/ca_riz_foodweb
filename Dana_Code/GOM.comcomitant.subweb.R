#### generate concomitant links (predator-parasite subweb) and identify parasite transmission pathways for GOM food web
### author: Dana Morton



library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)
library(data.table)


dir1 <- "/Users/danamorton/Box Sync/GOM/GOM food web/"

nodes <- read.csv("GOM.V2.nodes.cleaned.csv")
edge <- read.csv("GOM.Links.Compiled.Cleaned.pre.comcomitant.csv")


#create list of all parasite IDs
ps.edges <- subset.data.frame(edge,Interaction_code == 2) #indicate type(s) of trophic interaction. see metadata for codes
ps.nodes <- setDT(nodes)[Node.ID %in% unique(ps.edges$species_1_Node.ID)] #node list of all parasites in that are in hosts

psites <- ps.nodes$Node.ID  #if not specifying a life stage

#or to specify life stage:
# ps.stage <- subset.data.frame(ps.nodes, stageID == 1)  #select stage of parasite adult parasites, for concomittant links
# psites <- ps.stage$Sp.stage

dir6 <- paste(dir1, "/Concomitant/", sep="")

N = ps.edges[FALSE,]  #set up empty data frame to bind to



for (i in 1:100) {
  # subset edge list
  
  edge.subset <- subset.data.frame(edge, species_1_Node.ID == psites[i]) 
  edge.subset <- subset.data.frame(edge.subset, Interaction_code == 2) 
  
  #subset full edge list to only include hosts as resources
  edges.prey <- setDT(edge)[species_2_Node.ID %in% edge.subset$species_2_Node.ID]
  edges.prey <- subset.data.frame(edges.prey, Interaction_code == 1)
  n = as.numeric(nrow(edges.prey))
  if (n == 0) next
  
  #collect info to be put into new edges   
  #collapse refs and localities for each predator 
  refs<-  aggregate(Reference~species_1_Node.ID, data= edges.prey, FUN=paste, collapse= "; ")
  DOI <- aggregate(DOI~species_1_Node.ID, data= edges.prey, FUN=paste, collapse= "; ")
  Author <- aggregate(last_name_first_author~species_1_Node.ID, data= edges.prey, FUN=paste, collapse= "; ")
  Year.published <- aggregate(year_published~species_1_Node.ID, data= edges.prey, FUN=paste, collapse= "; ")
  Notes <- aggregate(species_2.accepted.name~species_1_Node.ID, data=edges.prey, FUN=paste, collapse="; ")
  colnames(Notes)[2] <- "Notes"
  Notes$Notes <- paste("Hosts eaten:", Notes$Notes, sep=" ")
  refs$Justification <- 11  #add column with justification, see metadata for codes
  
  #join lists
  new.links <- left_join(refs, Author, by= "species_1_Node.ID")
  new.links <- left_join(new.links,DOI,  by = "species_1_Node.ID")
  new.links <- left_join(new.links,Year.published,  by = "species_1_Node.ID")
  new.links<- left_join(new.links, Notes, by= "species_1_Node.ID")
  
  #add consumer ID columns
  new.links$species_1.accepted.name <- edges.prey$species_1.accepted.name[match(new.links$species_1_Node.ID,edges.prey$species_1_Node.ID)]
  new.links$Sp1.Species.Stage.ID <- edges.prey$Sp1.Species.Stage.ID[match(new.links$species_1_Node.ID,edges.prey$species_1_Node.ID)]
  new.links$species_1_id <- edges.prey$species_1_id[match(new.links$species_1_Node.ID,edges.prey$species_1_Node.ID)]
  new.links$species_1_stage.ID <- edges.prey$species_1_stage.ID[match(new.links$species_1_Node.ID,edges.prey$species_1_Node.ID)]
  new.links$species_1_stage <- edges.prey$species_1_stage[match(new.links$species_1_Node.ID,edges.prey$species_1_Node.ID)]
  
  new.links$Interaction_code <- 3  #for concomittant predation, see metadata for codes
  
  ## grab parasite info
  new.links$species_2_Node.ID <- psites[i]
  new.links$species_2.accepted.name <- nodes$Accepted.species.name[match(new.links$species_2_Node.ID,nodes$Node.ID)]
  new.links$Sp2.Species.Stage.ID <- nodes$Species.Stage.ID[match(new.links$species_2_Node.ID,nodes$Node.ID)]
  new.links$species_2_id <- nodes$Species.ID[match(new.links$species_2_Node.ID,nodes$Node.ID)]
  new.links$species_2_stage.ID <- nodes$stage_ID[match(new.links$species_2_Node.ID,nodes$Node.ID)]
  new.links$species_2_stage <- nodes$stage[match(new.links$species_2_Node.ID,nodes$Node.ID)]
  
  
  # #reorder so its easier to match with other edge lists
  # new.links <- new.links[c(1,8,7,9,6,11,      )]

  
  #to write each file individually
  # p <- nodes$Name.Sp.stage[nodes$Sp.stage==i]  #grab parasite name for file name
  # p <- gsub("\\?", "", p)  #remove random special characters that will interfere with file saving
  # p <- gsub("\\/", "_", p)
  # write.table(new.links, file = paste(dir6,"Concomittant.edges.of.",p,".xls",sep=""),
  #             sep="\t",dec='.', row.names = F)
  
  N <-  rbind.fill(N, new.links)  #bind new edge list
  
}
N$Presence.code = ""
N$data.source = "Trophic.Relationships"
N$interaction_strength = "No"
N$Link.ID = paste(N$species_1_Node.ID, N$species_2_Node.ID, sep = ";")
N$interaction_type = "concomitant"
N$interaction_sign = -1
N$initials = "DNM"
N1 <- N[,c(1:9,43,10:15,44, 19:21, 42,24, 16:18, 22,23,25:41)]

write.csv(N1, "Possible.concomitant.or.transmission.3.csv", row.names = F)

#manually reviewed links to determine which links should be concomitant and which should be converted to transmission/host-parasite links

#reload in link list and separate out the concomitant links to keep and the ones to convert
moar.links <- read.csv("Possible.concomitant.or.transmission.3.csv")

moar.links1 <- subset.data.frame(moar.links,Presence.code != "remove") #indicate type(s) of trophic interaction. see metadata for codes

conc.links <- subset.data.frame(moar.links1,interaction_type != "transmission" )

transmission <- subset.data.frame(moar.links1, interaction_type == "transmission")

#next create a new data-frame with Sp 1 and Sp 2 swapped 

new.sp1 <- transmission[,c(11:17)]
new.sp2 <- transmission[,c(4:10)]

new.meta <- transmission[,c(1,3,18:44)]
new.meta$interaction_type <- "parasitism"
new.meta$Interaction_code <- 2

colnames(new.sp1) <- c("species_1_Node.ID", "species_1_id" , "Sp1.Species.Stage.ID", "species_1_stage.ID",
                       "species_1_stage", "species_1", "species_1.accepted.name")

colnames(new.sp2) <- c("species_2_Node.ID", "species_2_id", "Sp2.Species.Stage.ID",
                       "species_2_stage.ID", "species_2_stage", "species_2", "species_2.accepted.name")

new.hp <- cbind(new.sp1, new.sp2, new.meta)
new.hp$Link.ID <- paste(new.hp$species_1_Node.ID, new.hp$species_2_Node.ID, sep = ";")
new.hp$Notes <- paste("trophic.transmission due to", new.hp$Notes, sep=" ")

write.csv(new.hp, "New.host.parasite.links.to.review1.csv", row.names = F)

#manually assigned correct life stages and confirmed links made sense

#load in new host-parasite links to merge

new.ph <- read.csv("New.host.parasite.links.reviewed.csv")
new.ph1 <- subset.data.frame(new.ph, Presence.code != "remove")
new.ph1 <- subset.data.frame(new.ph1, Presence.code != "revert to concomitant")

#combine with concomitant
conc.plus <- rbind(new.ph1, conc.links)

write.csv(conc.plus, "Conc.and.Trophic.Trans.links.to.bind.csv", row.names = F)

