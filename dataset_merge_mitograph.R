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
data_20230308 <- read_csv(file = "D:/20230308_CC_UK_pathVariants_re-crop_test/MitoGraph_output/_widthSummary_filtered/outputSummary_filtered.csv")
data_20230324 <- read_csv(file = "D:/20230324_CC_UK_pathVariants/MitoGraph_output/_widthSummary_filtered/outputSummary_filtered.csv")
data_20231121 <- read_csv(file = "D:/20231121_CC_UK_pathVariants/MitoGraph_output/_widthSummary_filtered/outputSummary_filtered.csv")

#merge all data into one data frame, preserving all values
merged <- merge(data_20230308, data_20230324, all = TRUE)
merged <- merge(merged, data_20231121, all = TRUE)

#create a copy of merged data, removing replicate numbers in Condition and Treatment information
merged_formatted <- merged
merged_formatted$Condition <- sub("Control Youth( #2)?", "Youth Control", merged_formatted$Condition)
merged_formatted$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", merged_formatted$Treatment)

#select for all data that is indicated as "Untreated Control" for "Youth Control", "P1 (G401S)", or "P2 (G363D)" cell types
merged_formatted_untreated <- merged_formatted %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

#reorder data within columns for plotting
merged_formatted_untreated$Condition <- factor(merged_formatted_untreated$Condition, levels = c("Youth Control", "P1 (G401S)", "P2 (G363D)"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = merged_formatted_untreated, aes_string(x = 'Treatment', y = 'Avg_Connected_Component_Length_um', fill = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Connected Component Length (um)") +
       scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
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
aov_clustered <- aov(Avg_Connected_Component_Length_um ~ Condition, data = merged_formatted_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition) #all p-valyues are zero

#code for performing one-way ANOVA with post hoc Tukey's HSD test
merged_formatted_untreated_summary <- merged_formatted_untreated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_avg_ccl = mean(Avg_Connected_Component_Length_um), sd_avg_ccl = sd(Avg_Connected_Component_Length_um), median_avg_ccl = median(Avg_Connected_Component_Length_um), count = n())
# 6.888473    2.907796    6.209357
# 9.646096    6.339156    7.880738
# 12.416763   6.829433    10.889911



plot(ggplot(data = merged_formatted_untreated, aes_string(x = 'Treatment', y = 'Avg_Degree', fill = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Branching (Avg. Degree)") +
       scale_y_continuous(breaks = c(1,1.5,2,2.5), limits = c(1,2.5)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
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

aov_clustered <- aov(Avg_Degree ~ Condition, data = merged_formatted_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition) #all p-values 0

merged_formatted_untreated_summary <- merged_formatted_untreated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_degree = mean(Avg_Degree), sd_degree = sd(Avg_Degree), median_degree = median(Avg_Degree), count = n())
#   1.700260	0.1523762	1.690121
#   1.800275	0.2116624	1.797710
#   1.908412	0.1936956	1.918599



#remove MitoGraph_Connectivity_Score values from plotting data that are above 6 (are all outliers; cleans up plot axes sizing)
merged_formatted_untreated_MCS <- merged_formatted_untreated %>%
  filter(., MitoGraph_Connectivity_Score < 6)

plot(ggplot(data = merged_formatted_untreated_MCS, aes_string(x = 'Treatment', y = 'MitoGraph_Connectivity_Score', fill = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "MitoGraph Connectivity Score") +
       scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
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

aov_clustered <- aov(MitoGraph_Connectivity_Score ~ Condition, data = merged_formatted_untreated_MCS)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition) #p-values 0, 0, 2.169684e-06

merged_formatted_untreated_MCS_summary <- merged_formatted_untreated_MCS %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_MCS = mean(MitoGraph_Connectivity_Score), sd_MCS = sd(MitoGraph_Connectivity_Score), median_MCS = median(MitoGraph_Connectivity_Score), count = n())
#   2.206394	0.4027582	2.174354
#   2.615377	0.7201576	2.562083
#   2.767865	0.6231028	2.729432



#remove Mito_Volume_Occupancy values from plotting data that are above 0.50 (are all outliers; cleans up plot axes sizing)
merged_formatted_untreated_vol <- merged_formatted_untreated %>%
  filter(., Mito_Volume_Occupancy < 0.50)

plot(ggplot(data = merged_formatted_untreated_vol, aes_string(x = 'Treatment', y = 'Mito_Volume_Occupancy', fill = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Mitochondrial Volume Occupancy") +
       scale_y_continuous(breaks = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40), limits = c(0.10, 0.40)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
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

aov_clustered <- aov(Mito_Volume_Occupancy ~ Condition, data = merged_formatted_untreated_vol)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition) #p-values 0, 0, 0.8178174

merged_formatted_untreated_vol_summary <- merged_formatted_untreated_vol %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_volOcc = mean(Mito_Volume_Occupancy), sd_volOcc = sd(Mito_Volume_Occupancy), median_volOcc = median(Mito_Volume_Occupancy), count = n())
#   0.2013489	0.02714007	0.1986572
#   0.2237918	0.02654070	0.2225414
#   0.2246525	0.02625510	0.2216624





#select for all data that is indicated as "Untreated Control" or 3.5% 1,6-HD (5 minutes)" for "Youth Control", "P1 (G401S)", or "P2 (G363D)" cell types
merged_formatted_treated <- merged_formatted %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "3.5% 1,6-HD (5 minutes)")

#reorder data within columns for plotting
merged_formatted_treated$Treatment <- factor(merged_formatted_treated$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)"))
merged_formatted_treated$Condition <- factor(merged_formatted_treated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = merged_formatted_treated, aes_string(x = 'Condition', y = 'Avg_Degree', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Branching") +
       scale_y_continuous(breaks = c(1,1.5,2,2.5), limits = c(1,2.5)) +
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

#code for computing summary metrics
aov_clustered <- aov(Avg_Degree ~ Condition*Treatment, data = merged_formatted_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #9.353764e-10, 5.584046e-01, 3.903907e-04

#code for performing one-way ANOVA with post hoc Tukey's HSD test
merged_formatted_treated_summary <- merged_formatted_treated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_degree = mean(Avg_Degree), sd_degree = sd(Avg_Degree), median_degree = median(Avg_Degree), count = n())
#   1.700260	0.1523762	1.690121        1.778519	0.1721791	1.775995
#   1.800275	0.2116624	1.797710      	1.825102	0.2153886	1.829370
#   1.908412	0.1936956	1.918599        1.961172	0.1674923	1.973534



plot(ggplot(data = merged_formatted_treated, aes_string(x = 'Condition', y = 'Avg_Connected_Component_Length_um', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Connected Component Length (um)") +
       scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
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

merged_formatted_treated_summary <- merged_formatted_treated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_avg_ccl = mean(Avg_Connected_Component_Length_um), sd_avg_ccl = sd(Avg_Connected_Component_Length_um), median_avg_ccl = median(Avg_Connected_Component_Length_um), count = n())
# 6.888473    2.907796    6.209357        8.731035    4.213814    7.711814
# 9.646096    6.339156    7.880738        9.808224    5.871865    8.209765
# 12.416763   6.829433    10.889911       13.815313   6.334894   12.415004

aov_clustered <- aov(Avg_Connected_Component_Length_um ~ Condition*Treatment, data = merged_formatted_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #2.462553e-08, 9.990724e-01, 1.993526e-03



#remove MitoGraph_Connectivity_Score values from plotting data that are above 6 (are all outliers; cleans up plot axes sizing)
merged_formatted_treated_MCS <- merged_formatted_treated %>%
  filter(., MitoGraph_Connectivity_Score < 6)

plot(ggplot(data = merged_formatted_treated_MCS, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "MitoGraph Connectivity Score") +
       scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
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

aov_clustered <- aov(MitoGraph_Connectivity_Score ~ Condition*Treatment, data = merged_formatted_treated_MCS)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) # 1.817227e-01, 1.456673e-04, 4.468473e-09

merged_formatted_treated_MCS_summary <- merged_formatted_treated_MCS %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_MCS = mean(MitoGraph_Connectivity_Score), sd_MCS = sd(MitoGraph_Connectivity_Score), median_MCS = median(MitoGraph_Connectivity_Score), count = n())
#   2.206394	0.4027582	2.174354       2.131764	0.3453770	2.129690
#   2.615377	0.7201576	2.562083       2.412788	0.7296332	2.280138
#   2.767865	0.6231028	2.729432       2.526516	0.5803635	2.415328



#remove Mito_Volume_Occupancy values from plotting data that are above 0.50 (are all outliers; cleans up plot axes sizing)
merged_formatted_treated_vol <- merged_formatted_treated %>%
  filter(., Mito_Volume_Occupancy < 0.50)

plot(ggplot(data = merged_formatted_treated_vol, aes_string(x = 'Condition', y = 'Mito_Volume_Occupancy', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Mitochondrial Volume Occupancy") +
       scale_y_continuous(breaks = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40), limits = c(0.10, 0.40)) +
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

aov_clustered <- aov(Mito_Volume_Occupancy ~ Condition*Treatment, data = merged_formatted_treated_vol)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #8.369876e-10, 1.176930e-09, 8.370444e-10

merged_formatted_treated_vol_summary <- merged_formatted_treated_vol %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_volOcc = mean(Mito_Volume_Occupancy), sd_volOcc = sd(Mito_Volume_Occupancy), median_volOcc = median(Mito_Volume_Occupancy), count = n())
#   0.2013489	0.02714007	0.1986572     0.2250139	0.03560387	0.2237979
#   0.2237918	0.02654070	0.2225414     0.2394658	0.02717451	0.2383941
#   0.2246525	0.02625510	0.2216624     0.2459483	0.02863276	0.2476685





#select for ALL data that is indicated across all cell types and treatments
merged_formatted_treated_all <- merged_formatted %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "2.5% 1,6-HD (5 minutes)" | Treatment == "2.5% 1,6-HD (20 minutes)" | Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "3.5% 1,6-HD (20 minutes)" | Treatment == "5.0% 1,6-HD (5 minutes)" | Treatment == "5.0% 1,6-HD (20 minutes)")

#reorder data within columns for plotting
merged_formatted_treated_all$Treatment <- factor(merged_formatted_treated_all$Treatment, levels = c("Untreated Control", "2.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (5 minutes)", "5.0% 1,6-HD (20 minutes)"))
merged_formatted_treated_all$Condition <- factor(merged_formatted_treated_all$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = merged_formatted_treated_all, aes_string(x = 'Condition', y = 'Avg_Connected_Component_Length_um', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Connected Component Length (um)") +
       scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
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

#code for computing summary metrics
merged_formatted_treated_summary <- merged_formatted_treated_all %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_avg_ccl = mean(Avg_Connected_Component_Length_um), sd_avg_ccl = sd(Avg_Connected_Component_Length_um), count = n())

#code for performing one-way ANOVA with post hoc Tukey's HSD test
aov_clustered <- aov(Avg_Connected_Component_Length_um ~ Condition*Treatment, data = merged_formatted_treated_all)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)



plot(ggplot(data = merged_formatted_treated_all, aes_string(x = 'Condition', y = 'Mito_Volume_Occupancy', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Mitochondrial Volume Occupancy") +
       scale_y_continuous(breaks = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40), limits = c(0.10, 0.40)) +
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

aov_clustered <- aov(Mito_Volume_Occupancy ~ Condition*Treatment, data = merged_formatted_treated_all)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)