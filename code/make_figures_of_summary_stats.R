library(dplyr)
library(data.table)
library(ggplot2)
library(ggpubr) # r2 formula on plot

make_summary_stat_plots <- function(name = "Name", 
                                    filename = "/Users/luke/Documents/family_size/data/Saguenay_ind_relative_family_size.csv",
                                    output_dir = "/Users/luke/Documents/family_size/figures/"){
  infile <- 
    fread(filename) %>%
    filter(!is.na(decade)) %>%
    group_by(ind, focal_family_size, t, min_wts_name, decade, great, grand) %>%
    dplyr::summarise(relative_family_size = mean(relative_family_size))
  
  infile$time_bin <- infile$decade - infile$decade %% 50
  infile$group <- paste0(infile$great,".",infile$grand,".","parents_",infile$time_bin)
  
  # Part 1
  town_corr <-
    infile %>%
    group_by(t, great, grand, time_bin) %>%
    dplyr::summarise(correl = cor(focal_family_size, relative_family_size),
                     count = n()) %>%
    ggplot(., aes(x = as.factor(time_bin), y = correl)) +
    geom_hline(yintercept = 0, size = 0.5) +
    geom_jitter(size = 0.01, color = "gray60") +
    geom_boxplot(outlier.shape = NA, fill = NA) +
    guides(color=guide_legend(title="Relative")) +
    ggtitle(name) +
    labs(x="Time") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_grid(.~ paste0(great,".",grand,".","parent"))
  
  ggsave(town_corr, filename = paste0(output_dir,name,"_corr_fs.jpg"), height = 4, width = 7)
  
  # Part 2
  op <-
    ggplot(infile, 
           aes(x = focal_family_size, y = relative_family_size)) +
    geom_jitter(size = 0.01)+
    geom_smooth(method = "lm", se=FALSE, color="royalblue", formula = y ~ x, size = 0.3) +
    scale_x_continuous(limits = c(0,30))+
    scale_y_continuous(limits = c(0,30))+
    theme_bw() +
    facet_grid(as.factor(time_bin)~paste0(great,".",grand,".","parent")) +
    stat_cor(aes(label = ..rr.label..), # paste(..rr.label.., ..p.label.., sep = "~`,`~")
             label.x = 15, r.accuracy = 0.001) +
    ggtitle(name) +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave(op, filename = paste0(output_dir,name,"_ind_relative_family_size.jpg"), height = 8, width = 6)
}