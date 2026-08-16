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

data_20230308 <- read_csv(file = "D:/20230308_CC_UK_pathVariants_re-crop_test/MitoGraph_output/_widthSummary_filtered/outputSummary_filtered.csv")
data_20230324 <- read_csv(file = "D:/20230324_CC_UK_pathVariants/MitoGraph_output/_widthSummary_filtered/outputSummary_filtered.csv")
data_20231121 <- read_csv(file = "D:/20231121_CC_UK_pathVariants/MitoGraph_output/_widthSummary_filtered/outputSummary_filtered.csv")

#data_20250414 <- read_csv(file = "D:/20250414_patient1374_fixed_2/MitoGraph_output/_widthSummary_filtered/outputSummary_filtered.csv")
#data_20250414$Plate <- 1

merged <- merge(data_20230308, data_20230324, all = TRUE)
merged <- merge(merged, data_20231121, all = TRUE)

merged_formatted <- merged
merged_formatted$Condition <- sub("Control Youth( #2)?", "Youth Control", merged_formatted$Condition)
merged_formatted$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", merged_formatted$Treatment)
#merged_formatted <- cbind(ImageNumber = stri_extract_first_regex(merged_formatted[[5]], "(?<=-)\\d{3}(?=_)"), merged_formatted)
#merged_formatted <- cbind(ROINumber = stri_extract_first_regex(merged_formatted[[6]], "(?<=_)[:digit:]{1,2}(?=.gnet)"), merged_formatted)
#merged_formatted <- merged_formatted[c(23,5,6,3,2,1,8:22,7)]
# setwd("D:/20230308_CC_UK_pathVariants_re-crop_test/")
# wb_merged_MitoGraph <- createWorkbook()
# addWorksheet(wb_merged_MitoGraph, sheet = 1)
# writeData(wb_merged_MitoGraph, sheet = 1, merged)
# saveWorkbook(wb_merged_MitoGraph, "merged_data_MitoGraph_formatted.xlsx", overwrite = TRUE)

merged_formatted_untreated <- merged_formatted %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)" | Condition == "P1374 (dex15)", Treatment == "Untreated Control")

merged_formatted_untreated$Condition <- factor(merged_formatted_untreated$Condition, levels = c("Youth Control", "P1 (G401S)", "P2 (G363D)", "P1374 (dex15)"))



merged_formatted_untreated <- merged_formatted_untreated %>%
  filter(., MitoGraph_Connectivity_Score < 6)


plot(ggplot(data = merged_formatted_untreated, aes_string(x = 'Treatment', y = 'MitoGraph_Connectivity_Score', fill = "Condition")) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = raw_formatted_untreated, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', shape = 'Condition'), size = 4) + 
       #scale_shape_manual(values = c(1,19)) +
       #geom_jitter(data = merged_untreated, width = 0.15) +
       scale_fill_manual(values = c('#bbbbbb','#636363','#2f2f2f','#111111')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "MitoGraph Connectivity Score") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       geom_sina(size = 1.5, position = position_dodge(width = 1)) +
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




plot(ggplot(data = merged_formatted_untreated, aes_string(x = 'Treatment', y = 'Avg_Connected_Component_Length_um', fill = "Condition")) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = raw_formatted_untreated, aes_string(x = 'Condition', y = 'MitoGraph_Connectivity_Score', shape = 'Condition'), size = 4) + 
       #scale_shape_manual(values = c(1,19)) +
       #geom_jitter(data = merged_untreated, width = 0.15) +
       scale_fill_manual(values = c('#bbbbbb','#636363','#2f2f2f','#111111')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = NULL, y = "Average Connected Component Length (um)") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       geom_sina(size = 1.5, position = position_dodge(width = 1)) +
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

