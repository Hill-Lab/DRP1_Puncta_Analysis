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

#map data file paths per plate replicate
plate1 <- read_xlsx("D:/20230308_CC_UK_pathVariants_re-crop_test/coloc2_allZ_formatted_manders_20230308.xlsx")
plate2 <- read_xlsx("D:/20230324_CC_UK_pathVariants/coloc2_allZ_formatted_manders_20230324.xlsx")
plate3 <- read_xlsx("D:/20231121_CC_UK_PathVariants/coloc2_allZ_formatted_manders_20231121.xlsx")

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

#remove Pearsons values from plotting data that are below 0.2 or above 0.8 (are all outliers; cleans up plot axes sizing)
merged_untreated <- merged_untreated %>%
  filter(., Pearsons >= 0.2 & Pearsons <= 0.8)

#select for all data that is indicated as "Untreated Control" or "3.5% 1,6-HD (5 minutes)" for "Youth Control", "P1 (G401S)", or "P2 (G363D)" cell types
merged_treated <- merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "3.5% 1,6-HD (5 minutes)"  | Treatment == "Untreated Control") %>%
  group_by(., Condition, Treatment)

#reorder data within columns for plotting
merged_treated$Treatment <- factor(merged_treated$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)"))
merged_treated$Condition <- factor(merged_treated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#remove Pearsons values from plotting data that are below 0.2 or above 0.8 (are all outliers; cleans up plot axes sizing)
merged_treated <- merged_treated %>%
  filter(., Pearsons >= 0.2 & Pearsons <= 0.8)

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = merged_untreated, aes_string(x = 'Treatment', y = 'Pearsons', fill = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Pearson's") +
       scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8), limits = c(0.15,0.85)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1.0), size = 1.5) +
       geom_sina(size = 1.0, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
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
  summarise(., mean_Pearsons = mean(Pearsons), sd_Pearsons = sd(Pearsons), median(Pearsons), count = n())
# 0.6320695   0.06438596  0.64
# 0.5466397   0.07230516  0.55
# 0.5705731   0.07485051  0.58

#code for performing one-way ANOVA with post hoc Tukey's HSD test
aov_clustered <- aov(Pearsons ~ Condition, data = merged_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition)  #0, 0, 3.410845e-09



plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment', color = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Pearsons") +
       scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8), limits = c(0.15,0.85)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       geom_sina(size = 1.0, position = position_dodge(width = 1))) +
        stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
        stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
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
        legend.position = "none")

merged_treated_summary <- merged_treated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_Pearsons = mean(Pearsons), sd_Pearsons = sd(Pearsons),  median(Pearsons), count = n())
#   0.6320695	0.06438596	0.64      0.6254737	0.09561652	0.64	380
#   0.5466397	0.07230516	0.55      0.4497537	0.10473180	0.43	203
#   0.5705731	0.07485051	0.58      0.5153903	0.12729804	0.54	269

aov_clustered <- aov(Pearsons ~ Condition*Treatment, data = merged_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)  #7.339919e-01, 1.157898e-09, 1.157955e-09



#select for ALL data that is indicated across all cell types and treatments
merged_treated_all <- merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "2.5% 1,6-HD (5 minutes)" | Treatment == "2.5% 1,6-HD (20 minutes)" | Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "3.5% 1,6-HD (20 minutes)" | Treatment == "5.0% 1,6-HD (5 minutes)" | Treatment == "5.0% 1,6-HD (20 minutes)")

#reorder data within columns for plotting
merged_treated_all$Treatment <- factor(merged_treated_all$Treatment, levels = c("Untreated Control", "2.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (5 minutes)", "5.0% 1,6-HD (20 minutes)"))
merged_treated_all$Condition <- factor(merged_treated_all$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = merged_treated_all, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment', color = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Pearsons") +
       scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8), limits = c(0.15,0.85)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       geom_sina(size = 1.0, position = position_dodge(width = 1))) +
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
        legend.position = "none")





#map data file path
plate4 <- read.xlsx("D:/20260618_fibro_coloc_panel/coloc2_allZ_formatted_manders_expanded_costes10_20260618.xlsx")

#create a copy of data, removing replicate numbers in Condition and Treatment information
plate4_renamed <- plate4
plate4_renamed$Condition <- sub("Control Youth( #1)?", "Youth Control", plate4_renamed$Condition)

#select for all data that is indicated as "Untreated Control" for "Youth Control", "P1 (G401S)", and "P2 (G363D)" cell types
plate4_untreated <- plate4_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

#reorder data within columns for plotting
plate4_untreated$Condition <- factor(plate4_untreated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#select for all data that across all treatments for "Youth Control", "P1 (G401S)", and "P2 (G363D)" cell types
plate4_treated <- plate4_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)") %>%
  group_by(., Condition, Treatment)

#reorder data within columns for plotting
plate4_treated$Treatment <- factor(plate4_treated$Treatment, levels = c("TOM20", "Calnexin", "LAMP1", "PEX14", "alpha-Tubulin"))
plate4_treated$Condition <- factor(plate4_treated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment', color = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','yellow','#9B870C')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Pearsons") +
       scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8), limits = c(0.15,0.85)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       geom_sina(size = 1.0, position = position_dodge(width = 1))) +
  stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
  stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
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
        legend.position = "none")

plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment', color = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','yellow','#9B870C')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Pearsons") +
       scale_y_continuous(breaks = c(-0.1,0.1,0.3,0.5,0.7), limits = c(-0.1,0.7)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       geom_sina(size = 1.0, position = position_dodge(width = 1))) +
  stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
  stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
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
        legend.position = "none")

plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'Manders.tM1', fill = 'Treatment', color = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','yellow','#9B870C')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Manders' tM1") +
       scale_y_continuous(breaks = c(0.0,0.2,0.4,0.6,0.8,1.0), limits = c(0,1)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       geom_sina(size = 1.0, position = position_dodge(width = 1))) +
  stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
  stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
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
        legend.position = "none")