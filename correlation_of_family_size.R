# load library
library(dplyr)
library(data.table)
# load function
setwd("/Users/luke/Documents/genome_simulations/code/R/")
source("get_extended_family.R")

# load lon lat coordinates
locations <- fread("../../misc/watershed_locations_feb2022.csv") %>% dplyr::rename(lieum = lieu)
#locations <- fread("watershed_locations_feb2022.csv") %>% dplyr::rename(lieum = lieu)
#pedigree <- fread("tout_balsac.csv") %>% left_join(locations, by ="lieum")
# load pedigree
pedigree <- fread("../tout_balsac.csv") %>% left_join(locations, by ="lieum")
pedigree$decade <- pedigree$datem - pedigree$datem %% 10

# three columns for extended family function
three_col_ped <- pedigree %>% select(ind, mother, father)



  #summarise(correl = cor(focal_family_size,relative_family_size))

library(ggplot2)

out <- 
  ggplot() +
  geom_jitter(data=grand_mother_family_size_correlation, 
             aes(x=focal_family_size, 
                 y = relative_family_size),
             size = 0.05) +
  theme_bw() + 
  facet_wrap(.~ a, scales = "free")
  
