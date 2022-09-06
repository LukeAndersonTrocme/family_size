library(dplyr)
library(data.table)
library(ggplot2)

library(plyr) # r2 formula
library(ggpmisc) # r2 formula on plot
library(cowplot) # multi panel plot

lm_eqn = function(df){
  m = lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

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
    ggplot(., aes(x = as.factor(time_bin), y = correl, color = paste0(great,".",grand,".","parent"))) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(size = 0.01, position = position_jitterdodge()) +
    guides(color=guide_legend(title="Relative")) +
    ggtitle(name) +
    labs(x="Time") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(town_corr, filename = paste0(output_dir,name,"_corr_fs.jpg"), height = 4, width = 7)
  
  # Part 2
  
  eq <- ddply(infile %>% dplyr::rename(x = focal_family_size, y = relative_family_size),.(group),lm_eqn)
  
  label_df <- inner_join(eq, infile) %>% group_by(group, V1) %>% dplyr::summarise(lab_x = 15, lab_y = 29)
  
  op <-
    ggplot(infile, 
           aes(x = focal_family_size, y = relative_family_size)) +
    geom_jitter(size = 0.01)+
    geom_smooth(method = "lm", se=FALSE, color="royalblue", formula = y ~ x, size = 0.3) +
    scale_x_continuous(limits = c(0,30))+
    scale_y_continuous(limits = c(0,30))+
    theme_bw() +
    facet_wrap(.~group, scales = "free_y", labeller = as_labeller(eq, 
                                                                  default = label_parsed)) +
    geom_text(data = label_df, aes(x = lab_x, y = lab_y, label=V1), parse = TRUE, inherit.aes=FALSE) +
    ggtitle(name) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  ggsave(op, filename = paste0(output_dir,name,"_ind_relative_family_size.jpg"), height = 8, width = 12)
}