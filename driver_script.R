setwd("/home/luke1111/projects/ctb-sgravel/luke1111/family_size/")
# load library
library(dplyr)
library(data.table)
# load function
source("get_extended_family.R")
source("get_relative_family_size.R")
# load files
locations <- fread("watershed_locations_feb2022.csv") %>% dplyr::rename(lieum = lieu)
pedigree <- fread("tout_balsac.csv") %>% left_join(locations, by ="lieum")
pedigree$decade <- pedigree$datem - pedigree$datem %% 10
# get individuals of interest
list_of_parents <- pedigree %>% filter(ind %in% pedigree$mother | ind %in% pedigree$father)
# get the family size of their ancestors (parents, grandparents and great-grandparents)
relative_family_size <- get_relative_family_size(pedigree, list_of_parents)

# write a single massive file
fwrite(avg_over_couple, file = "ind_relative_family_size.csv")
# write separate files for each region
relative_family_size %>% group_by(a) %>% do(fwrite(., paste0(unique(.$a), "_ind_relative_family_size.csv")))