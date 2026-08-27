library(readxl)
library(plyr)
library(dplyr)
library(ggpubr)
library(ggnewscale)
library(ggplot2)
library(ggpmisc)
library(stringi)
library(rstatix)
library(tidyverse)
library(reshape2)
library(stringr)
library(formattable)
library(r2r)
library(openxlsx)
library(ggforce)
library(sinaplot)
library(lemon)

#map data file paths per plate replicate
plate1 <- read_xlsx("D:/20230308_CC_UK_pathVariants_re-crop_test/summary_cell_individual_20230308_updated20260717.xlsx")
plate2 <- read_xlsx("D:/20230324_CC_UK_pathVariants/summary_cell_individual_naOmit_20230324_updated20260717.xlsx")
plate3 <- read_xlsx("D:/20231121_CC_UK_PathVariants/summary_cell_individual_20231121_updated20260717.xlsx")

#merge all data into one data frame, preserving all values
merged <- merge(plate1, plate2, all = TRUE)
merged <- merge(merged, plate3, all = TRUE)

#create a copy of merged data, removing replicate numbers in Condition and Treatment information
merged_control_renamed <- merged
merged_control_renamed$Condition <- sub("Control Youth( #2)?", "Youth Control", merged_control_renamed$Condition)
merged_control_renamed$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", merged_control_renamed$Treatment)

#select for all data that is indicated as "Untreated Control" for "Youth Control", "P1 (G401S)", or "P2 (G363D)" cell types
merged_untreated <- merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

#reorder data within columns for plotting
merged_untreated$Condition <- factor(merged_untreated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#select for all data that is indicated as "Untreated Control" or "3.5% 1,6-HD (5 minutes)" for "Youth Control", "P1 (G401S)", or "P2 (G363D)" cell types
merged_treated <- merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "Untreated Control") %>%
  group_by(., Condition, Treatment)

#reorder data within columns for plotting
merged_treated$Treatment <- factor(merged_treated$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)"))
merged_treated$Condition <- factor(merged_treated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = merged_untreated, aes_string(x = 'Condition', y = 'mean_field_MFI')) +
       geom_point(data = merged_untreated, aes_string(colour = 'Condition', x = 'Condition', y = 'mean_field_MFI'), size = 2, position = position_jitter(width = 0.15)) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 0.25, colour = "grey50") +
       labs(x = "Cell Type", y = "MFI") +
       geom_violin(width = 1.0, alpha = 0.2) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 315, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))

plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'mean_field_MFI', fill = 'Treatment')) +
       geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_MFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 0.75)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 0.75), width = 0.5, size = 0.25, colour = "grey50") +
       labs(x = "Cell Type", y = "MFI") +
       geom_violin(width = 1.0, alpha = 0.2, position = position_dodge(width = 0.75)) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 315, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "right"))

plot(ggplot(data = merged_untreated, aes_string(x = 'Condition', y = 'mean_field_sdMFI')) +
       geom_point(data = merged_untreated, aes_string(colour = 'Condition', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitter(width = 0.15)) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 0.25, colour = "grey50") +
       labs(x = "Cell Type", y = "sdMFI") +
       geom_violin(width = 1.0, alpha = 0.2) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 315, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))

plot(ggplot(data = merged_untreated, aes_string(x = 'Treatment', y = 'mean_field_MFI', fill = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "MFI") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.5, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
       scale_y_continuous(breaks = c(150,200,250,300,350,400,450), limits = c(150, 450)) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))

#code for computing summary metrics
merged_untreated_summary <- merged_untreated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_MFI = mean(mean_field_MFI), sd_MFI = sd(mean_field_MFI), median_MFI = median(mean_field_MFI), count = n())
#	  250.6706	  33.03597	  244.8736	
#   231.7303	  32.97358	  224.9144	
#   236.8853	  26.82847	  231.4978	

#code for performing one-way ANOVA with post hoc Tukey's HSD test
aov_clustered <- aov(mean_field_MFI ~ Condition, data = merged_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition)



plot(ggplot(data = merged_untreated, aes_string(x = 'Treatment', y = 'mean_field_sdMFI', fill = 'Condition')) + 
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "sdMFI") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.5, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
       scale_y_continuous(breaks = c(50,100,150,200,250,300,350), limits = c(25, 350)) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))

merged_untreated_summary <- merged_untreated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_sdMFI = mean(mean_field_sdMFI), sd_sdMFI = sd(mean_field_sdMFI), median_sdMFI = median(mean_field_sdMFI), count = n())
#   129.3242	  43.07687	  122.8704
#   135.2205	  50.26941	  126.4267
#   155.6785	  50.51039	  148.9008

 aov_clustered <- aov(mean_field_sdMFI ~ Condition, data = merged_untreated)
 resTukeyHSD <- TukeyHSD(aov_clustered)
 df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition)

 
 
 plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'mean_field_MFI', fill = 'Treatment', color = 'Condition')) +
        scale_fill_manual(values = c('#bbbbbb','lightblue')) +
        scale_color_manual(values = c('black','#737373','#1f1f1f')) +
        stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
        stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
        labs(x = "Cell Type", y = "MFI") +
        geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
        stat_sina(size = 1.0, position = position_dodge(width = 1)) +
        stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
        stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
        scale_y_continuous(breaks = c(150,200,250,300,350,400), limits = c(150, 400)) +
        theme(strip.text.x = element_text (size = 6, color = "black"),
              strip.text.y = element_text (size = 6, color = "black"),
              axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
              axis.title.x = element_text(size = 12, color = "black"),
              axis.text.y = element_text(size = 12, color = "black"),
              axis.title.y = element_text(size = 12, color = "black"),
              plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="white"),
              axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
              axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
              legend.position = "none"))
 
 merged_treated_summary <- merged_treated %>%
   group_by(., Condition, Treatment) %>%
   summarise(., mean_MFI = mean(mean_field_MFI), sd_MFI = sd(mean_field_MFI), median_MFI = median(mean_field_MFI), count = n())
#   250.6706	33.03597	244.8736      255.1873	37.99872	245.7797
#   231.7303	32.97358	224.9144      218.6871	22.93310	214.5529
#   236.8853	26.82847	231.4978      230.7208	25.25101	225.8019
 
 aov_clustered <- aov(mean_field_MFI ~ Condition*Treatment, data = merged_treated)
 resTukeyHSD <- TukeyHSD(aov_clustered)
 df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #p-values are 1.270905e-01, 7.778371e-06, 5.535552e-02
 
 
 
 plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'mean_field_sdMFI', fill = 'Treatment', color = 'Condition')) + 
        scale_fill_manual(values = c('#bbbbbb','lightblue')) +
        scale_color_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
        stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
        stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
        labs(x = "Cell Type", y = "sdMFI") +
        geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
        stat_sina(size = 1.0, position = position_dodge(width = 1)) +
        stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
        stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
        scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350), limits = c(0, 350)) +
        theme(strip.text.x = element_text (size = 6, color = "black"),
              strip.text.y = element_text (size = 6, color = "black"),
              axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
              axis.title.x = element_text(size = 12, color = "black"),
              axis.text.y = element_text(size = 12, color = "black"),
              axis.title.y = element_text(size = 12, color = "black"),
              plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="white"),
              axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
              axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
              legend.position = "none"))
 
 merged_treated_summary <- merged_treated %>%
   group_by(., Condition, Treatment) %>%
   summarise(.,  mean_sdMFI = mean(mean_field_sdMFI), sd_sdMFI = sd(mean_field_sdMFI), median_sdMFI = median(mean_field_sdMFI), count = n())
#   129.32418	43.07687	122.87035     119.29931	  43.93850	111.85815
#   135.22048	50.26941	126.42670     77.94254	  27.49726	73.40995
#   155.67848	50.51039	148.90080     106.70966	  44.13801	99.97860
 
 aov_clustered <- aov(mean_field_sdMFI ~ Condition*Treatment, data = merged_treated)
 resTukeyHSD <- TukeyHSD(aov_clustered)
 df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #p-values are 2.142273e-03, 7.437475e-10, 7.437475e-10
 
 
 
 plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'median_field_MdFI', fill = 'Treatment', color = 'Condition')) + 
        scale_fill_manual(values = c('#bbbbbb','lightblue')) +
        scale_color_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
        stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
        stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
        labs(x = "Cell Type", y = "Median Field Intensity") +
        geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
        stat_sina(size = 1.0, position = position_dodge(width = 1)) +
        scale_y_continuous(breaks = c(150,200,250,300,350,400), limits = c(150, 400)) +
        theme(strip.text.x = element_text (size = 6, color = "black"),
              strip.text.y = element_text (size = 6, color = "black"),
              axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
              axis.title.x = element_text(size = 12, color = "black"),
              axis.text.y = element_text(size = 12, color = "black"),
              axis.title.y = element_text(size = 12, color = "black"),
              plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="white"),
              axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
              axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
              legend.position = "none"))
 
plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'CoV', fill = 'Treatment', color = 'Condition')) +
        scale_fill_manual(values = c('#bbbbbb','lightblue')) +
        scale_color_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
        stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
        stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
        labs(x = "Cell Type", y = "Coefficient of Variation") +
        geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
        stat_sina(size = 1.0, position = position_dodge(width = 1)) +
        scale_y_continuous(breaks = c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4), limits = c(0.0, 1.4)) +
        theme(strip.text.x = element_text (size = 6, color = "black"),
              strip.text.y = element_text (size = 6, color = "black"),
              axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
              axis.title.x = element_text(size = 12, color = "black"),
              axis.text.y = element_text(size = 12, color = "black"),
              axis.title.y = element_text(size = 12, color = "black"),
              plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="white"),
              axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
              axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
              legend.position = "none"))
 
 
 
#select for ALL data that is indicated across all cell types and treatments
 merged_treated_all <- merged_control_renamed %>%
   filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "2.5% 1,6-HD (5 minutes)" | Treatment == "2.5% 1,6-HD (20 minutes)" | Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "3.5% 1,6-HD (20 minutes)" | Treatment == "5.0% 1,6-HD (5 minutes)" | Treatment == "5.0% 1,6-HD (20 minutes)")
 
 #reorder data within columns for plotting
 merged_treated_all$Treatment <- factor(merged_treated_all$Treatment, levels = c("Untreated Control", "2.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (5 minutes)", "5.0% 1,6-HD (20 minutes)"))
 merged_treated_all$Condition <- factor(merged_treated_all$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))
 
  #all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
 #each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
 plot(ggplot(data = merged_treated_all, aes_string(x = 'Condition', y = 'mean_field_MFI', fill = 'Treatment', color = 'Condition')) +
        scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
        scale_color_manual(values = c('black','black','black')) +
        stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
        stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
        labs(x = "Cell Type", y = "MFI") +
        geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
        stat_sina(size = 1.0, position = position_dodge(width = 1)) +
        scale_y_continuous(breaks = c(150,200,250,300,350,400), limits = c(150, 400)) +
        theme(strip.text.x = element_text (size = 6, color = "black"),
              strip.text.y = element_text (size = 6, color = "black"),
              axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
              axis.title.x = element_text(size = 12, color = "black"),
              axis.text.y = element_text(size = 12, color = "black"),
              axis.title.y = element_text(size = 12, color = "black"),
              plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="white"),
              axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
              axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
              legend.position = "none"))
 
 merged_treated_summary <- merged_treated_all %>%
   group_by(., Condition, Treatment) %>%
   summarise(., mean_MFI = mean(mean_field_MFI), sd_MFI = sd(mean_field_MFI), count = n())
 
 aov_clustered <- aov(mean_field_MFI ~ Condition*Treatment, data = merged_treated_all)
 resTukeyHSD <- TukeyHSD(aov_clustered)
 df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) 
 
 
 
 plot(ggplot(data = merged_treated_all, aes_string(x = 'Condition', y = 'mean_field_sdMFI', fill = 'Treatment', color = 'Condition')) +
        scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
        scale_color_manual(values = c('black','black','black')) +
        stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
        stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
        labs(x = "Cell Type", y = "sdMFI") +
        geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
        stat_sina(size = 1.0, position = position_dodge(width = 1)) +
        scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350), limits = c(0, 350)) +
        theme(strip.text.x = element_text (size = 6, color = "black"),
              strip.text.y = element_text (size = 6, color = "black"),
              axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
              axis.title.x = element_text(size = 12, color = "black"),
              axis.text.y = element_text(size = 12, color = "black"),
              axis.title.y = element_text(size = 12, color = "black"),
              plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="white"),
              axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
              axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
              legend.position = "none"))
 
 merged_treated_summary <- merged_treated_all %>%
   group_by(., Condition, Treatment) %>%
   summarise(.,  mean_sdMFI = mean(mean_field_sdMFI), sd_sdMFI = sd(mean_field_sdMFI), count = n())
 
 aov_clustered <- aov(mean_field_sdMFI ~ Condition*Treatment, data = merged_treated_all)
 resTukeyHSD <- TukeyHSD(aov_clustered)
 df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)
 
 
 
 
 
#map data file path
plate4 <- read.xlsx("D:/20260708_pathVariant_multiTreat/summary_cell_individual_updated20260806.xlsx")

#create a copy of data, removing replicate numbers in Condition and Treatment information
plate4_renamed <- plate4
plate4_renamed$Condition <- sub("Control Youth( #1)?", "Youth Control", plate4_renamed$Condition)
plate4_renamed$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", plate4_renamed$Treatment)

#select for all data that is indicated as "Untreated Control" for "Youth Control", "P1 (G401S)", and "P2 (G363D)" cell types
plate4_untreated <- plate4_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

#reorder data within columns for plotting
plate4_untreated$Condition <- factor(plate4_untreated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#select for all data that is indicated as "Untreated Control", "Washout (5 minutes)", or "3.5% 1,2,6-HT (5 minutes)" for "Youth Control", "P1 (G401S)", or "P2 (G363D)" cell types
plate4_treated <- plate4_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "Washout (5 minutes)" | Treatment == "3.5% 1,2,6-HT (5 minutes)") %>%
  group_by(., Condition, Treatment)

#plate4_treated$Treatment <- factor(plate4_treated$Treatment, levels = c("Untreated Control", "Washout (5 minutes)", "Washout (20 minutes)", "3.5% 1,2,6-HT (5 minutes)", "3.5% 1,2,6-HT (20 minutes)"))
plate4_treated$Treatment <- factor(plate4_treated$Treatment, levels = c("Untreated Control", "Washout (5 minutes)", "3.5% 1,2,6-HT (5 minutes)"))
plate4_treated$Condition <- factor(plate4_treated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'mean_field_MFI', fill = 'Treatment', color = 'Condition')) + 
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','yellow','#9B870C')) +
       scale_color_manual(values = c('black','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "MFI") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
       scale_y_continuous(breaks = c(400,600,800,1000,1200,1400), limits = c(400, 1400)) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))

plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'mean_field_sdMFI', fill = 'Treatment', color = 'Condition')) + 
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen')) +
       scale_color_manual(values = c('black','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "sdMFI") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
       scale_y_continuous(breaks = c(200,400,600,800,1000,1200,1400), limits = c(200, 1400)) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))

#remove CoV values from plotting data that are below 0.75 or above 0.95 (are all outliers; cleans up plot axes sizing)
plate4_treated_CoV <- plate4_treated %>%
  filter(., CoV > 0.75 & CoV < 0.95)

plot(ggplot(data = plate4_treated_CoV, aes_string(x = 'Condition', y = 'CoV', fill = 'Treatment', color = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','yellow','#9B870C')) +
       scale_color_manual(values = c('black','#737373','#1f1f1f')) + 
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "CoV") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
       scale_y_continuous(breaks = c(0.7, 0.8, 0.9, 1.0), limits = c(0.7, 1.0)) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             #panel.border = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))





#map data file path
plate4 <- read.xlsx("D:/20260611_RPE_U2OS_Hexanediol_Washout_Matthew/summary_cell_individual_updated20260808.xlsx")

#create a copy of data
plate4_renamed <- plate4

#select for all data that is indicated as "RPE" or "U2OS" cell type across all treatments
plate4_treated <- plate4_renamed %>%
  filter(., Condition == "RPE" | Condition == "U2OS") %>%
  group_by(., Condition, Treatment)

#reorder data within columns for plotting
plate4_treated$Treatment <- factor(plate4_treated$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes) and washout (2.5 minutes)"))
plate4_treated$Condition <- factor(plate4_treated$Condition, levels = c("RPE", "U2OS"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'mean_field_sdMFI', fill = 'Treatment', color = 'Condition')) + 
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen')) +
       scale_color_manual(values = c('black','#737373','#1f1f1f')) + 
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "sdMFI") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
       scale_y_continuous(breaks = c(400,600,800,1000,1200,1400), limits = c(400, 1400)) +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
             axis.title.x = element_text(size = 12, color = "black"),
             axis.text.y = element_text(size = 12, color = "black"),
             axis.title.y = element_text(size = 12, color = "black"),
             plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))