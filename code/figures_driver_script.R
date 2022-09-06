setwd("/Users/luke/Documents/family_size/code/")
# load function
source("make_figures_of_summary_stats.R")

regions <- c("Eastern","North Central","Northern Quebec","Ottawa River","Saguenay","South Central", "St. Lawrence")

for(region in regions) {
  print(region)
  make_summary_stat_plots(name = region, 
                          filename = paste0("/Users/luke/Documents/family_size/data/",region,"_ind_relative_family_size.csv"),
                          output_dir = "/Users/luke/Documents/family_size/figures/")
}
