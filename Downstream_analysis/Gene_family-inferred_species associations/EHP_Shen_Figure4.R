#Figure 4 in the paper

rm(list=ls())
setwd("/Users/yikeshen/Desktop/metagenome")
library(dplyr)
library(ggplot2)
library(tidyverse)
speciesraw <- read.csv(file='sig_species_ccpair_all_jan28.csv', header = TRUE)
speciesraw[80,4] <- 'g__Ruminococcus.s__Ruminococcus_lactaris'

speciesprocessing <- speciesraw %>% separate(gene, into=c("genus","species"), sep="([\\.])")
speciesprocessing$species <- gsub( "s__", "",as.character(speciesprocessing$species))
speciesprocessing$species <- gsub( "_", " ",as.character(speciesprocessing$species))

speciesprocessing$metadata <- gsub("Cd_c", "Childhood Blood Cadmium (nmol/L)", speciesprocessing$metadata)
speciesprocessing$metadata <- gsub("Hg_c", "Childhood Blood Mercury (nmol/L)", speciesprocessing$metadata)
speciesprocessing$metadata <- gsub("Hg_D_c", "Perinatal Blood Mercury (nmol/L)", speciesprocessing$metadata)
speciesprocessing$metadata <- gsub("Pb_c", "Childhood Blood Lead (umol/L)", speciesprocessing$metadata)

cadmiumspecies <- speciesprocessing[1:77,]
Speciesrange_Cd <- aggregate(cadmiumspecies[, 6:9], list(cadmiumspecies$species), range) %>% as.matrix() %>% as.data.frame()
mercuryspecies <- speciesprocessing[78:80,]
Speciesrange_Hg <- aggregate(mercuryspecies[, 6:9], list(mercuryspecies$species), range) %>% as.matrix() %>% as.data.frame()
restspecies <- speciesprocessing[81:82,]
Speciesrange_rest <- aggregate(restspecies[, 6:9], list(restspecies$species), range) %>% as.matrix() %>% as.data.frame()
Summarytable <- rbind(Speciesrange_Cd,Speciesrange_Hg,Speciesrange_rest)
Summarytable_numeric <- Summarytable
Summarytable_numeric[,2:9] <- lapply(Summarytable_numeric[,2:9], function(x) as.numeric(as.character(x)))
Summarytable_numeric <- Summarytable_numeric[,-1] %>% round(3)
Summarytable_final <- cbind(Summarytable$Group.1,Summarytable_numeric)


plotallspecies_bw <- speciesprocessing %>% ggplot(aes(x=species,y=coef),color="black",stat="identity")+
  geom_point(size = 4)+
  labs(x="Species", y=expression("Effect estimate per unit increase of blood metals"))+
  theme_bw()+
  scale_color_manual(breaks = c("beneficial","harmful", "uncertain"),
                     values=c("red","black", "blue"))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size=22, hjust=0.5),
        text = element_text(size = 16),
        axis.text.y = element_text(face = "italic"))+
  coord_flip()

plotallspecies_bw

