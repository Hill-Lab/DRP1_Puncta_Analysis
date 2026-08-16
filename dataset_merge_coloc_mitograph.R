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

plate1 <- read_xlsx("D:/20230308_CC_UK_pathVariants_re-crop_test/coloc2_formatted_20230308_new.xlsx")
plate2 <- read_xlsx("D:/20230324_CC_UK_pathVariants/coloc2_formatted_20230324_new.xlsx")
plate3 <- read_xlsx("D:/20231121_CC_UK_PathVariants/coloc2_formatted_20231121_new.xlsx")

merged <- merge(plate1, plate2, all = TRUE)
merged <- merge(merged, plate3, all = TRUE)

merged_control_renamed <- merged
merged_control_renamed$Condition <- sub("Control Youth( #2)?", "Youth Control", merged_control_renamed$Condition)
merged_control_renamed$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", merged_control_renamed$Treatment)

merged_control_renamed_pad <- merged_control_renamed
merged_control_renamed_pad$ROINumber <-str_pad(merged_control_renamed$ROINumber, 2, pad = "0")



data_raw <- read_csv(file = "E:/KROSS/20230308_CC_UK_pathVariants/JOBS/MitoGraph_output/_widthSummary_filtered/outputSummary_filtered_updated.csv")

raw_formatted <- data_raw
raw_formatted$Condition <- sub("Control Youth( #2)?", "Youth Control", raw_formatted$Condition)
raw_formatted$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", raw_formatted$Treatment)

raw_formatted <- cbind(ImageNumber = toupper(stri_extract_first_regex(raw_formatted[[6]], "(?<=-)\\d{3}(?=_)")), raw_formatted)
raw_formatted <- cbind(ROINumber = toupper(stri_extract_first_regex(raw_formatted[[7]], "(?<=_)[:digit:]{1,2}(?=.gnet)")), raw_formatted)
raw_formatted$Plate <- 1

raw_formatted <- raw_formatted[c(24,6,7,4,2,1,9:23,8)] 



merged_2 <- merge(merged_control_renamed_pad, raw_formatted, by = c("Plate", "Condition", "Treatment", "WellIdentifier", "ImageNumber", "ROINumber"))

merged_2$Avg_Component_Length_um <- merged_2$Total_Length_um/merged_2$Total_Connected_Components


merged_2_untreated <- merged_2 %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

merged_2_untreated$Condition <- factor(merged_2_untreated$Condition, levels = c("Youth Control", "P1 (G401S)", "P2 (G363D)"))

merged_2_untreated <- merged_2_untreated %>%
  filter(., Pearsons >= 0.2 & Pearsons <= 0.8)


# 
# plot(ggplot(data = merged_2_untreated, aes_string(x = 'Pearsons', y = 'Avg_Component_Length_um', fill = 'Condition')) +
#        #scale_y_continuous(breaks = breaks_list, limits = c(round_yMin, round_yMax)) +
#        #geom_point(data = merged_treated, aes_string(colour = 'Treatment', x = 'Condition', y = 'mean_field_sdMFI'), size = 2, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
#        scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
#        #scale_fill_manual(values = c('#52bfd9','#ffa742')) +
#        #scale_colour_gradient2(low = "#391463", mid = "#824574", high = "#fc9086") +
#        stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
#        #geom_boxplot (outlier.size = 0, colour = "grey50", fill = "grey75", position = position_dodge (width = 1), alpha = 0.6) +
#        stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
#        #stat_pvalue_manual(sig_df, label = "p.adj.signif", tip.length = 0.01, step.increase = 0.05) +
#        labs(x = "Cell Type", y = "Pearsons") +
#        #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
#        geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
#        stat_sina(size = 1.5, position = position_dodge(width = 1)) +
#        #facet_wrap(~ Plate, scales = "fixed") +
#        scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8), limits = c(0.1, 0.9)) +
#        theme(strip.text.x = element_text (size = 6, color = "black"),
#              strip.text.y = element_text (size = 6, color = "black"),
#              axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0),
#              axis.title.x = element_text(size = 12, color = "black"),
#              axis.text.y = element_text(size = 12, color = "black"),
#              axis.title.y = element_text(size = 12, color = "black"),
#              plot.title = element_text(size = 12, color = "black", hjust = 0.5, face = "bold"),
#              panel.grid.major = element_blank(),
#              panel.grid.minor = element_blank(),
#              #panel.border = element_blank(),
#              panel.background = element_rect(fill="white"),
#              axis.line.x = element_line(color = "grey75", size = 1, linetype = 1),
#              axis.line.y = element_line(color = "grey75", size = 1, linetype = 1),
#              legend.position = "none"))



plot(ggplot(data = merged_2_untreated, aes_string(x = 'Pearsons', y = 'Avg_Component_Length_um', fill = 'Condition')) +
       geom_point(data = merged_2_untreated, aes_string(colour = 'Condition', fill = 'Condition', x = 'Pearsons', y = 'Avg_Component_Length_um'), size = 2) +
       scale_colour_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       geom_smooth(aes(group = Condition, color = factor(Condition)), method = "lm", formula = y ~ x, se = TRUE) +
       labs(x = "Pearsons", y = "Average Component Length (um)") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       #geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.5, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0, 30)) +
       scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8), limits = c(0.2, 0.8)) +
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
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1)
       ))



plot(ggplot(data = merged_2_untreated, aes_string(x = 'Pearsons', y = 'Avg_Degree', fill = 'Condition')) +
       geom_point(data = merged_2_untreated, aes_string(colour = 'Condition', fill = 'Condition', x = 'Pearsons', y = 'Avg_Degree'), size = 2) +
       scale_colour_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       geom_smooth(aes(group = Condition, color = factor(Condition)), method = "lm", formula = y ~ x, se = TRUE) +
       labs(x = "Pearsons", y = "Average Degree of Branching") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       #geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.5, position = position_dodge(width = 1)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(1.0,1.5,2.0,2.5), limits = c(1.0, 2.5)) +
       scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8), limits = c(0.2, 0.8)) +
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
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1)
       ))






merged_2_treated <- merged_2 %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "3.5% 1,6-HD (5 minutes)"  | Treatment == "Untreated Control") %>%
  group_by(., Condition, Treatment)

merged_2_treated$Treatment <- factor(merged_2_treated$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)"))
merged_2_treated$Condition <- factor(merged_2_treated$Condition, levels = c("Youth Control", "P1 (G401S)", "P2 (G363D)"))

merged_2_treated <- merged_2_treated %>%
  filter(., Pearsons >= 0.2 & Pearsons <= 0.8)






plot(ggplot(data = merged_2_treated, aes_string(x = 'Pearsons', y = 'Avg_Component_Length_um', fill = 'Treatment', color = 'Condition')) +
       geom_point(data = merged_2_treated, aes_string(colour = 'Condition', fill = 'Treatment', x = 'Pearsons', y = 'Avg_Component_Length_um'), size = 2) +
       scale_colour_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       geom_smooth(aes(group = Condition, color = factor(Condition)), method = "lm", formula = y ~ x, se = TRUE) +
       labs(x = "Pearsons", y = "Average Component Length (um)") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       #geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.5, position = position_dodge(width = 1)) +
       facet_wrap(~ Treatment, scales = "fixed") +
       scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0, 30)) +
       scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8), limits = c(0.2, 0.8)) +
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
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1)
       ))



plot(ggplot(data = merged_2_treated, aes_string(x = 'Pearsons', y = 'Avg_Degree', fill = 'Treatment', color = 'Condition')) +
       geom_point(data = merged_2_treated, aes_string(colour = 'Condition', fill = 'Treatment', x = 'Pearsons', y = 'Avg_Degree'), size = 2) +
       scale_colour_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       geom_smooth(aes(group = Condition, color = factor(Condition)), method = "lm", formula = y ~ x, se = TRUE) +
       labs(x = "Pearsons", y = "Average Degree of Branching") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       #geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       #stat_sina(size = 1.5, position = position_dodge(width = 1)) +
       facet_wrap(~ Treatment, scales = "fixed") +
       scale_y_continuous(breaks = c(1.0,1.5,2.0,2.5), limits = c(1.0, 2.5)) +
       scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8), limits = c(0.2, 0.8)) +
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
             axis.line.y = element_line(color = "grey75", size = 1, linetype = 1)
       ))


