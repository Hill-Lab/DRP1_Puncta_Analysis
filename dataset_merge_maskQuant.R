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
library(doParallel)
library(usethis)
library(ggforce)
library(sinaplot)
library(lemon)

pathList_maskQuant <- c("D:/20230308_CC_UK_pathVariants_re-crop_test/data_maskQuant_formatted_20230308.xlsx", 
                        "D:/20230324_CC_UK_pathVariants/data_maskQuant_formatted_20230324.xlsx", 
                        "D:/20231121_CC_UK_PathVariants/data_maskQuant_formatted_20231121.xlsx")

data_readIn <- NULL
formatted_data_maskQuant <- NULL

for (i in pathList_maskQuant) {
  data_readIn <- read.xlsx(i)
  formatted_data_maskQuant <- rbind(formatted_data_maskQuant, data_readIn)
}

condition_hash_plate1 <- hashmap()
treatment_hash_plate1 <- hashmap()

rowID_1 <- c('B','C','D','E','F')
rowID_len_1 <- length(rowID_1)
condition_1 <- c("Control Adult", "Control Youth", "P1 (G401S)", "P2 (G363D)", "P3 (L230dup)")
condition_hash_plate1[rowID_1] <- condition_1
colID_1 <- c(2,3,4,5,6,7,8,9,10,11)
colID_len_1 <- length(colID_1)
treatment_1 <- c("Untreated Control #1", "2.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "5.0% 1,6-HD (5 minutes)", "Untreated Control #2", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (20 minutes)", "Untreated Control #3", "Untreated Control #4")
treatment_hash_plate1[colID_1] <- treatment_1

condition_hash_plate2 <- hashmap()
treatment_hash_plate2 <- hashmap()

rowID_2 <- c('B','C','D','E','F','G')
rowID_len_2 <- length(rowID_2)
condition_2 <- c("Control Youth #2", "P3 (L230dup)", "P2 (G363D)", "P1 (G401S)", "Control Youth", "Control Adult")
condition_hash_plate2[rowID_2] <- condition_2
colID_2 <- c(2,3,4,5,6,7,8,9,10,11)
colID_len_2 <- length(colID_2)
treatment_2 <- c("Untreated Control #4", "Untreated Control #3", "5.0% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)", "2.5% 1,6-HD (20 minutes)", "Untreated Control #2", "5.0% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (5 minutes)", "Untreated Control #1")
treatment_hash_plate2[colID_2] <- treatment_2

condition_hash_plate3 <- hashmap()
treatment_hash_plate3 <- hashmap()

rowID_3 <- c('A','B','C','D')
rowID_len_3 <- length(rowID_3)
condition_3 <- c("Control Youth", "Control Adult", "P1 (G401S)", "P2 (G363D)")
condition_hash_plate3[rowID_3] <- condition_3
colID_3 <- c(1,2,3,4,5,6)
colID_len_3 <- length(colID_3)
treatment_3 <- c("Untreated Control #1", "Untreated Control #2", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)")
treatment_hash_plate3[colID_3] <- treatment_3

condition_hash_list <- list(condition_hash_plate1, condition_hash_plate2, condition_hash_plate3)
treatment_hash_list <- list(treatment_hash_plate1, treatment_hash_plate2, treatment_hash_plate3)

formatted_data_maskQuant$Condition <- ""
formatted_data_maskQuant$Treatment <- ""

n_cols <- ncol(formatted_data_maskQuant)
n_rows <- nrow(formatted_data_maskQuant)

for (i in seq(1, n_rows)) {
  iplate <- formatted_data_maskQuant[i, 1]
  cell <- formatted_data_maskQuant[i, 2]
  irow <- stri_extract_first_regex(cell, "[:alpha:]")
  icol <- stri_extract_first_regex(cell, "[:digit:]{1,2}")
  formatted_data_maskQuant[i, n_cols-1] <- condition_hash_list[[iplate]][irow]
  formatted_data_maskQuant[i, n_cols] <- treatment_hash_list[[iplate]][as.numeric(icol)]
}
formatted_data_maskQuant <- formatted_data_maskQuant %>%
  select(c(1, (n_cols-1), n_cols, (2:(n_cols-2))))



setwd("D:/20230308_CC_UK_pathVariants_re-crop_test")

wb_maskQuant <- createWorkbook()
addWorksheet(wb_maskQuant, sheet = 1)
writeData(wb_maskQuant, sheet = 1, formatted_data_maskQuant)
saveWorkbook(wb_maskQuant, "merged_data_maskQuant_formatted.xlsx", overwrite = TRUE)






formatted_data_maskQuant <- read.xlsx("D:/20230308_CC_UK_pathVariants_re-crop_test/merged_data_maskQuant_formatted.xlsx")

formatted_data_maskQuant_renamed <- formatted_data_maskQuant
formatted_data_maskQuant_renamed$Condition <- sub("Control Youth( #2)?", "Youth Control", formatted_data_maskQuant$Condition)
formatted_data_maskQuant_renamed$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", formatted_data_maskQuant$Treatment)



formatted_data_maskQuant_untreated_255threshold <- formatted_data_maskQuant_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control", ThresholdCutoff_corrected == 255)

formatted_data_maskQuant_untreated_255threshold$Condition <- factor(formatted_data_maskQuant_untreated_255threshold$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))


plot(ggplot(data = formatted_data_maskQuant_untreated_255threshold, aes_string(x = 'Condition', y = 'Mean_corrected', fill = 'MaskType', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Average Intensity") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900), limits = c(100, 900)) +
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
             legend.position = "top"))



formatted_data_maskQuant_untreated_150threshold <- formatted_data_maskQuant_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control", ThresholdCutoff_corrected == 150)

formatted_data_maskQuant_untreated_150threshold$Condition <- factor(formatted_data_maskQuant_untreated_150threshold$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))


plot(ggplot(data = formatted_data_maskQuant_untreated_150threshold, aes_string(x = 'Condition', y = 'Mean_corrected', fill = 'MaskType', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Average Intensity") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900), limits = c(100, 900)) +
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
             legend.position = "top"))



formatted_data_maskQuant_untreated_25.5threshold <- formatted_data_maskQuant_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control", ThresholdCutoff_corrected == 25.5)

formatted_data_maskQuant_untreated_25.5threshold$Condition <- factor(formatted_data_maskQuant_untreated_25.5threshold$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))


plot(ggplot(data = formatted_data_maskQuant_untreated_25.5threshold, aes_string(x = 'Condition', y = 'Mean_corrected', fill = 'MaskType', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Average Intensity") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900), limits = c(100, 900)) +
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
             legend.position = "top"))







formatted_data_maskQuant_treated_255threshold <- formatted_data_maskQuant_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "3.5% 1,6-HD (5 minutes)", ThresholdCutoff_corrected == 255)

formatted_data_maskQuant_treated_255threshold$Condition <- factor(formatted_data_maskQuant_treated_255threshold$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))


plot(ggplot(data = formatted_data_maskQuant_treated_255threshold, aes_string(x = 'Condition', y = 'Mean_corrected', fill = 'MaskType', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Average Intensity") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900), limits = c(100, 900)) +
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
             legend.position = "top"))



formatted_data_maskQuant_treated_150threshold <- formatted_data_maskQuant_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "3.5% 1,6-HD (5 minutes)", ThresholdCutoff_corrected == 150)

formatted_data_maskQuant_treated_150threshold$Condition <- factor(formatted_data_maskQuant_treated_150threshold$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))


plot(ggplot(data = formatted_data_maskQuant_treated_150threshold, aes_string(x = 'Condition', y = 'Mean_corrected', fill = 'MaskType', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Average Intensity") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900), limits = c(100, 900)) +
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
             legend.position = "top"))



formatted_data_maskQuant_untreated_25.5threshold <- formatted_data_maskQuant_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "3.5% 1,6-HD (5 minutes)", ThresholdCutoff_corrected == 25.5)

formatted_data_maskQuant_untreated_25.5threshold$Condition <- factor(formatted_data_maskQuant_untreated_25.5threshold$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))


plot(ggplot(data = formatted_data_maskQuant_untreated_25.5threshold, aes_string(x = 'Condition', y = 'Mean_corrected', fill = 'MaskType', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Average Intensity") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900), limits = c(100, 900)) +
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
             legend.position = "top"))












formatted_data_maskQuant_mito_150threshold <- formatted_data_maskQuant_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", MaskType == "Mito", Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "Untreated Control", ThresholdCutoff_corrected == 150)

formatted_data_maskQuant_mito_150threshold$Condition <- factor(formatted_data_maskQuant_mito_150threshold$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))
formatted_data_maskQuant_mito_150threshold$Treatment <- factor(formatted_data_maskQuant_mito_150threshold$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)"))

plot(ggplot(data = formatted_data_maskQuant_mito_150threshold, aes_string(x = 'Condition', y = 'Mean_corrected', fill = 'Treatment', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Average Intensity") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900), limits = c(100, 900)) +
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
             legend.position = "top"))



formatted_data_maskQuant_cyto_150threshold <- formatted_data_maskQuant_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", MaskType == "Cyto", Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "Untreated Control", ThresholdCutoff_corrected == 150)

formatted_data_maskQuant_cyto_150threshold$Condition <- factor(formatted_data_maskQuant_cyto_150threshold$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))
formatted_data_maskQuant_cyto_150threshold$Treatment <- factor(formatted_data_maskQuant_cyto_150threshold$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)"))

plot(ggplot(data = formatted_data_maskQuant_cyto_150threshold, aes_string(x = 'Condition', y = 'Mean_corrected', fill = 'Treatment', color = 'Condition')) +
       #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
       #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
       #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
       labs(x = "Cell Type", y = "Average Intensity") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.0, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900), limits = c(100, 900)) +
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
             legend.position = "top"))
