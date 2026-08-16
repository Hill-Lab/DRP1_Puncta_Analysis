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

plate1 <- read_xlsx("D:/20230308_CC_UK_pathVariants_re-crop_test/coloc2_allZ_formatted_manders_20230308.xlsx")
plate2 <- read_xlsx("D:/20230324_CC_UK_pathVariants/coloc2_allZ_formatted_manders_20230324.xlsx")
plate3 <- read_xlsx("D:/20231121_CC_UK_PathVariants/coloc2_allZ_formatted_manders_20231121.xlsx")

merged <- merge(plate1, plate2, all = TRUE)
merged <- merge(merged, plate3, all = TRUE)

merged_control_renamed <- merged
merged_control_renamed$Condition <- sub("Control Youth( #2)?", "Youth Control", merged_control_renamed$Condition)
merged_control_renamed$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", merged_control_renamed$Treatment)

merged_untreated <- merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

merged_untreated$Condition <- factor(merged_untreated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

merged_untreated <- merged_untreated %>%
  filter(., Pearsons >= 0.2 & Pearsons <= 0.8)


merged_treated <- merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "3.5% 1,6-HD (5 minutes)"  | Treatment == "Untreated Control") %>%
  group_by(., Condition, Treatment)

merged_treated$Treatment <- factor(merged_treated$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)"))
merged_treated$Condition <- factor(merged_treated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

merged_treated <- merged_treated %>%
  filter(., Pearsons >= 0.2 & Pearsons <= 0.8)


plot(ggplot(data = merged_untreated, aes_string(x = 'Treatment', y = 'Pearsons', fill = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = raw_formatted_untreated, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', shape = 'Condition'), size = 4) + 
       #scale_shape_manual(values = c(1,19)) +
       #geom_jitter(data = merged_untreated, width = 0.15) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "Pearson's") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
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
             #panel.border = element_blank(),
             panel.background = element_rect(fill="white"),
             axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
             legend.position = "none"))

merged_untreated_summary <- merged_untreated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_Pearsons = mean(Pearsons), sd_Pearsons = sd(Pearsons), median(Pearsons), count = n())
# 0.6320695   0.06438596  0.64
# 0.5466397   0.07230516  0.55
# 0.5705731   0.07485051  0.58

aov_clustered <- aov(Pearsons ~ Condition, data = merged_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition)  #0, 0, 3.410845e-09





plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = raw_formatted_untreated, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', shape = 'Condition'), size = 4) + 
       #scale_shape_manual(values = c(1,19)) +
       #geom_jitter(data = merged_untreated, width = 0.15) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) + 
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "Pearsons") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
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
        #panel.border = element_blank(),
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









merged_treated_all <- merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "2.5% 1,6-HD (5 minutes)" | Treatment == "2.5% 1,6-HD (20 minutes)" | Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "3.5% 1,6-HD (20 minutes)" | Treatment == "5.0% 1,6-HD (5 minutes)" | Treatment == "5.0% 1,6-HD (20 minutes)")

merged_treated_all$Treatment <- factor(merged_treated_all$Treatment, levels = c("Untreated Control", "2.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (5 minutes)", "5.0% 1,6-HD (20 minutes)"))
merged_treated_all$Condition <- factor(merged_treated_all$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))






plot(ggplot(data = merged_treated_all, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = raw_formatted_untreated, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', shape = 'Condition'), size = 4) + 
       #scale_shape_manual(values = c(1,19)) +
       #geom_jitter(data = merged_untreated, width = 0.15) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) + 
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "Pearsons") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
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
        #panel.border = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
        axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
        legend.position = "none")










sigCols <- c("group1", "group2", "p.adj", "y.position")
sig_df <- data.frame(matrix(nrow = 0, ncol = length(sigCols)))
colnames(sig_df) = sigCols
yPos <- max(merged_untreated$Pearsons + 0.075)
count <- 1
index <- 1

for (k in seq(1, rowID_len-1)) {
  for (l in seq(k + 1, rowID_len)) {
    if (as.double(df_resTukeyHSD[count, 4]) < 0.05) {
      sig_df[index, 1] <- 1
      sig_df[index, 2] <- 2
      sig_df[index, 3] <- df_resTukeyHSD[count, 4]
      sig_df[index, 4] <- yPos
      index = index + 1
    }
    count = count + 1
  }
}
sig_df <- add_significance(sig_df, "p.adj")




plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'Pearsons'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 0.75)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 0.75), width = 0.5, size = 0.25, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Pearsons") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.2, position = position_dodge(width = 0.75)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       theme(strip.text.x = element_text (size = 6, color = "black"),
             strip.text.y = element_text (size = 6, color = "black"),
             axis.text.x = element_text(size = 12, color = "black", angle = 315, hjust = 0),
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
             legend.position = "right"))



# t.test(data = merged_untreated, Pearsons ~ Condition)

aov_clustered <- aov(Pearsons ~ Condition*Treatment, data = merged_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)















# merged_treated <- merged_control_renamed %>%
#   filter(., Condition == "Youth Control" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "3.5% 1,6-HD (5 minutes)") %>%
#   group_by(., Condition, Treatment)

#merged_treated$Treatment <- factor(merged_treated$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)"))
merged_treated <- merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P2 (G363D)", Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "Untreated Control") %>%
  group_by(., Condition, Treatment)

merged_treated$Treatment <- factor(merged_treated$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)"))
merged_treated$Condition <- factor(merged_treated$Condition, levels = c("Youth Control", "P2 (G363D)"))


merged_treated <- merged_treated %>%
  filter(., Pearsons >= 0.2 & Pearsons <= 0.9)


plot(ggplot(data = merged_treated, aes_string(x = 'Treatment', y = 'Pearsons', fill = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'Pearsons'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       #scale_shape_manual(values = c(1,19)) +
       scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 0.75)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 0.75), width = 0.5, size = 1.0, colour = "grey50") +
       #tat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "Pearsons' Colocalization Score") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.4, position = position_dodge(width = 0.75), size = 1.5) +
       #geom_sina(aes_string(size = 1, shape = 'Condition')) +
       stat_sina(size = 2, position = position_dodge(width = 0.75)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1.0), limits = c(0.15, 1.0)) +
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
             legend.position = "right"))


aov_clustered <- aov(Pearsons ~ Treatment*Condition, data = merged_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Treatment:Condition`)

sigCols <- c("group1", "group2", "p.adj", "y.position")
sig_df <- data.frame(matrix(nrow = 0, ncol = length(sigCols)))
colnames(sig_df) = sigCols
yPos <- max(merged_treated$Pearsons + 0.075)
count <- 1
index <- 1

for (k in seq(1, 3)) {
  for (l in seq(k + 1, 4)) {
    if (as.double(df_resTukeyHSD[count, 4]) < 0.05) {
      sig_df[index, 1] <- k
      sig_df[index, 2] <- l
      sig_df[index, 3] <- df_resTukeyHSD[count, 4]
      sig_df[index, 4] <- yPos
      index = index + 1
    }
    count = count + 1
  }
}
sig_df <- add_significance(sig_df, "p.adj")










#plate4 <- read.xlsx("D:/20260708_pathVariant_multiTreat/coloc2_allZ_formatted_manders_20260708.xlsx")
plate4 <- read.xlsx("D:/20260618_fibro_coloc_panel/coloc2_allZ_formatted_manders_expanded_costes10_20260618.xlsx")

plate4_renamed <- plate4
plate4_renamed$Condition <- sub("Control Youth( #1)?", "Youth Control", plate4_renamed$Condition)
#plate4_renamed$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", plate4_renamed$Treatment)

plate4_untreated <- plate4_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

plate4_untreated$Condition <- factor(plate4_untreated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))


plate4_treated <- plate4_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)") %>%
  group_by(., Condition, Treatment)

#plate4_treated$Treatment <- factor(plate4_treated$Treatment, levels = c("Untreated Control", "Washout (5 minutes)", "Washout (20 minutes)", "3.5% 1,2,6-HT (5 minutes)", "3.5% 1,2,6-HT (20 minutes)"))
plate4_treated$Treatment <- factor(plate4_treated$Treatment, levels = c("TOM20", "Calnexin", "LAMP1", "PEX14", "alpha-Tubulin"))
plate4_treated$Condition <- factor(plate4_treated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))






plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = raw_formatted_untreated, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', shape = 'Condition'), size = 4) + 
       #scale_shape_manual(values = c(1,19)) +
       #geom_jitter(data = merged_untreated, width = 0.15) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','yellow','#9B870C')) +
       scale_color_manual(values = c('black','black','black')) + 
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "Pearsons") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
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
        #panel.border = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
        axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
        legend.position = "none")








plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'Pearsons', fill = 'Treatment', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = raw_formatted_untreated, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', shape = 'Condition'), size = 4) + 
       #scale_shape_manual(values = c(1,19)) +
       #geom_jitter(data = merged_untreated, width = 0.15) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','yellow','#9B870C')) +
       scale_color_manual(values = c('black','black','black')) + 
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "Pearsons") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
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
        #panel.border = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
        axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
        legend.position = "none")




plot(ggplot(data = plate4_treated, aes_string(x = 'Condition', y = 'Manders.tM1', fill = 'Treatment', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = raw_formatted_untreated, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', shape = 'Condition'), size = 4) + 
       #scale_shape_manual(values = c(1,19)) +
       #geom_jitter(data = merged_untreated, width = 0.15) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','yellow','#9B870C')) +
       scale_color_manual(values = c('black','black','black')) + 
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "Manders' tM1") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
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
        #panel.border = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
        axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
        legend.position = "none")


aov_clustered <- aov(Manders.tM1 ~ Condition*Treatment, data = plate4_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)
