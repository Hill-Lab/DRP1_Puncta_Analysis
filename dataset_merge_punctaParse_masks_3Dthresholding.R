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

#seems to help with processing massive amounts of data
cores <- detectCores()
registerDoParallel(cores = cores)

#map '3D Intensity' data file paths
pathList_3DIntensity <- c("D:/20230308_CC_UK_pathVariants_re-crop_test/data_3DIntensity_masks_formatted_20230308.xlsx", 
                         "D:/20230324_CC_UK_pathVariants/data_3DIntensity_masks_formatted_20230324.xlsx", 
                         "D:/20231121_CC_UK_PathVariants/data_3DIntensity_masks_formatted_20231121.xlsx")

#map '3D Compactness' data file paths
pathList_3DCompactness <- c("D:/20230308_CC_UK_pathVariants_re-crop_test/data_3DCompactness_formatted_20230308.xlsx", 
                            "D:/20230324_CC_UK_pathVariants/data_3DCompactness_formatted_20230324.xlsx", 
                            "D:/20231121_CC_UK_PathVariants/data_3DCompactness_formatted_20231121.xlsx")

#map '3D Volume' data file paths
pathList_3DVolume <- c("D:/20230308_CC_UK_pathVariants_re-crop_test/data_3DVolume_formatted_20230308.xlsx", 
                       "D:/20230324_CC_UK_pathVariants/data_3DVolume_formatted_20230324.xlsx", 
                       "D:/20231121_CC_UK_PathVariants/data_3DVolume_formatted_20231121.xlsx")

#initialize variables needed to parse data
data_readIn <- NULL
formatted_data_3DIntensity <- NULL
formatted_data_3DCompactness <- NULL
formatted_data_3DVolume <- NULL

#tterate through tabs within the '3D Intensity' data spreadsheet and combine into a single data frame
for (i in pathList_3DIntensity) {
  sheetNames <- getSheetNames(i)
  
  for (j in sheetNames) {
    data_readIn <- read.xlsx(i, sheet = j)
    formatted_data_3DIntensity <- rbind(formatted_data_3DIntensity, data_readIn)
  }
}

#iterate through tabs within the '3D Compactness' data spreadsheet and combine into a single data frame
for (i in pathList_3DCompactness) {
  sheetNames <- getSheetNames(i)

  for (j in sheetNames) {
    data_readIn <- read.xlsx(i, sheet = j)
    formatted_data_3DCompactness <- rbind(formatted_data_3DCompactness, data_readIn)
  }
}

#iterate through tabs within the '3D Volume' data spreadsheet and combine into a single data frame
for (i in pathList_3DVolume) {
  sheetNames <- getSheetNames(i)

  for (j in sheetNames) {
    data_readIn <- read.xlsx(i, sheet = j)
    formatted_data_3DVolume <- rbind(formatted_data_3DVolume, data_readIn)
  }
}

#setup hash maps for indexing plate well information, per experimental replicate
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

#add additional columns for condition and treatment information
formatted_data_3DIntensity$Condition <- ""
formatted_data_3DIntensity$Treatment <- ""
formatted_data_3DCompactness$Condition <- ""
formatted_data_3DCompactness$Treatment <- ""
formatted_data_3DVolume$Condition <- ""
formatted_data_3DVolume$Treatment <- ""

#track column/row counts
n_cols_I <- ncol(formatted_data_3DIntensity)
n_rows_I <- nrow(formatted_data_3DIntensity)
n_cols_C <- ncol(formatted_data_3DCompactness)
n_rows_C <- nrow(formatted_data_3DCompactness)
n_cols_V <- ncol(formatted_data_3DVolume)
n_rows_V <- nrow(formatted_data_3DVolume)

#iterate through '3D Intensity' data frame and update the 'Condition' and 'Treatment' columns from the hash map information
for (i in seq(1, n_rows_I)) {
  iplate <- formatted_data_3DIntensity[i, 1]
  cell <- formatted_data_3DIntensity[i, 3]
  irow <- stri_extract_first_regex(cell, "[:alpha:]")
  icol <- stri_extract_first_regex(cell, "[:digit:]{1,2}")
  formatted_data_3DIntensity[i, n_cols_I-1] <- condition_hash_list[[iplate]][irow]
  formatted_data_3DIntensity[i, n_cols_I] <- treatment_hash_list[[iplate]][as.numeric(icol)]
}
formatted_data_3DIntensity <- formatted_data_3DIntensity %>%
  select(c(1, (n_cols_I-1), n_cols_I, (2:(n_cols_I-2))))

#iterate through '3D Compactness' data frame and update the 'Condition' and 'Treatment' columns from the hash map information
for (i in seq(1, n_rows_C)) {
  iplate <- formatted_data_3DCompactness[i, 1]
  cell <- formatted_data_3DCompactness[i, 2]
  irow <- stri_extract_first_regex(cell, "[:alpha:]")
  icol <- stri_extract_first_regex(cell, "[:digit:]{1,2}")
  formatted_data_3DCompactness[i, n_cols_C-1] <- condition_hash_list[[iplate]][irow]
  formatted_data_3DCompactness[i, n_cols_C] <- treatment_hash_list[[iplate]][as.numeric(icol)]
}
formatted_data_3DCompactness <- formatted_data_3DCompactness %>%
  select(c(1, (n_cols_C-1), n_cols_C, (2:(n_cols_C-2))))

#iterate through '3D Volume' data frame and update the 'Condition' and 'Treatment' columns from the hash map information
for (i in seq(1, n_rows_V)) {
  iplate <- formatted_data_3DVolume[i, 1]
  cell <- formatted_data_3DVolume[i, 2]
  irow <- stri_extract_first_regex(cell, "[:alpha:]")
  icol <- stri_extract_first_regex(cell, "[:digit:]{1,2}")
  formatted_data_3DVolume[i, n_cols_V-1] <- condition_hash_list[[iplate]][irow]
  formatted_data_3DVolume[i, n_cols_V] <- treatment_hash_list[[iplate]][as.numeric(icol)]
}
formatted_data_3DVolume <- formatted_data_3DVolume %>%
  select(c(1, (n_cols_V-1), n_cols_V, (2:(n_cols_V-2))))

#directory to save Excel Workbooks
setwd("D:/20231121_CC_UK_pathVariants")

#create Excel Workbook objects to store and save data
wb_3DIntensity <- createWorkbook()
wb_3DCompactness <- createWorkbook()
wb_3DVolume <- createWorkbook()

#calculate the number of Excel Workbook tabs needed to store all data
numRows <- nrow(formatted_data_3DIntensity)
maxExcelRows <- 1048575     #this value is one fewer than the maximum number of rows a single tab in an Excel Workbook can store before data is lost
numSheets <- NULL
lowInd <- 1
highInd <- maxExcelRows

if (numRows > maxExcelRows) {
  numSheets <- ceiling(numRows/maxExcelRows)
} else {
  numSheets <- 1
}

#iterate through Excel Workbook tabs and populate with data 
for (i in  seq(1, numSheets)) {
  addWorksheet(wb_3DIntensity, sheet = i)
  subset_df_3DIntensity <- formatted_data_3DIntensity[c(lowInd:highInd),]
  writeData(wb_3DIntensity, sheet = i, subset_df_3DIntensity)

  addWorksheet(wb_3DCompactness, sheet = i)
  subset_df_3DCompactness <- formatted_data_3DCompactness[c(lowInd:highInd),]
  writeData(wb_3DCompactness, sheet = i, subset_df_3DCompactness)
  
  addWorksheet(wb_3DVolume, sheet = i)
  subset_df_3DVolume <- formatted_data_3DVolume[c(lowInd:highInd),]
  writeData(wb_3DVolume, sheet = i, subset_df_3DVolume)
  
  lowInd <- lowInd + maxExcelRows
  highInd <- highInd + maxExcelRows
}

#save Excel Workbooks for each data set
saveWorkbook(wb_3DIntensity, "data_3DIntensity_masks_formatted_annotated_20231121.xlsx", overwrite = TRUE)
saveWorkbook(wb_3DCompactness, "merged_data_3DCompactness_formatted.xlsx", overwrite = TRUE)
saveWorkbook(wb_3DVolume, "merged_data_3DVolume_formatted.xlsx", overwrite = TRUE)



#map file paths of Excel Workbooks saved above (saving Workbooks beforehand is highly recommended given the sheer amount of data; crashes are moroe requent for massive data sets)
data_readIn_annotated <- NULL
formatted_data_3DIntensity_annotated <- NULL
pathList_3DIntensity_annotated <- c("D:/20230308_CC_UK_pathVariants_re-crop_test/data_3DIntensity_masks_formatted_annotated_20230308.xlsx", 
                                    "D:/20230324_CC_UK_pathVariants/data_3DIntensity_masks_formatted_annotated_20230324.xlsx", 
                                    "D:/20231121_CC_UK_PathVariants/data_3DIntensity_masks_formatted_annotated_20231121.xlsx")

#iterate through Excel Workbook tabs and read data into memory
for (i in pathList_3DIntensity_annotated) {
  sheetNames_annotated <- getSheetNames(i)
  
  for (j in sheetNames_annotated) {
    data_readIn_annotated <- read.xlsx(i, sheet = j)
    formatted_data_3DIntensity_annotated <- rbind(formatted_data_3DIntensity_annotated, data_readIn_annotated)
  }
}

#group data by mitochondrial masks
formatted_data_3DIntensity_annotated_MITO <- formatted_data_3DIntensity_annotated %>%
  filter(., MaskType == "MITO") %>%
  group_by(Plate, Condition, Treatment, WellIdentifier, ImageNumber, ROINumber)

formatted_data_3DIntensity_annotated_MITO_summary <- formatted_data_3DIntensity_annotated_MITO %>%
  group_by(Plate, Condition, Treatment, MaskType, WellIdentifier, ImageNumber, ROINumber) %>%
  dplyr::summarise(numPuncta_MITO = dplyr::n(), avgIntensity_MITO = mean(IntensityAvg), sdIntensity_MITO = sd(IntensityAvg)) %>%
  ungroup()

#group data by cytosolic masks
formatted_data_3DIntensity_annotated_CYTO <- formatted_data_3DIntensity_annotated %>%
  filter(., MaskType == "CYTO") %>%
  group_by(Plate, Condition, Treatment, WellIdentifier, ImageNumber, ROINumber)

formatted_data_3DIntensity_annotated_CYTO_summary <- formatted_data_3DIntensity_annotated_CYTO %>%
  group_by(Plate, Condition, Treatment, MaskType, WellIdentifier, ImageNumber, ROINumber) %>%
  dplyr::summarise(numPuncta_CYTO = dplyr::n(), avgIntensity_CYTO = mean(IntensityAvg), sdIntensity_CYTO = sd(IntensityAvg)) %>%
  ungroup()

#merge data by unique identifiers and create new data columns for relevant arithmetic-based metrics
merged <- merge(formatted_data_3DIntensity_annotated_MITO_summary, formatted_data_3DIntensity_annotated_CYTO_summary, by = c("Plate", "Condition", "Treatment", "WellIdentifier", "ImageNumber", "ROINumber"))
merged$percentMito <- merged$numPuncta_MITO / (merged$numPuncta_MITO + merged$numPuncta_CYTO)
merged$totalPuncta <- merged$numPuncta_MITO + merged$numPuncta_CYTO
merged$intensityRatio <- merged$avgIntensity_MITO / merged$avgIntensity_CYTO

#remove Condition and Treatment replicate numbers (redundant with other unique identifiers and makes grouping easier)
merged$Condition <- sub("Control Youth( #2)?", "Youth Control", merged$Condition)
merged$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", merged$Treatment)

#create a merged data set containing all 'Untreated Control' data, factored per cell type
merged_untreated <- merged %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

merged_untreated$Condition <- factor(merged_untreated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))



#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(data = merged_untreated, aes_string(x = 'Treatment', y = 'percentMito', fill = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Percent  Mitochondrial Puncta") +
       scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6), limits = c(0.0,0.6)) +
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
  summarise(., mean_percentMito = mean(percentMito), sd_percentMito = sd(percentMito), median_percentMito = median(percentMito), count = n())
# 0.2829882   0.04031246    0.2810458
# 0.2376990   0.04700077    0.2381988
# 0.2490522   0.04504677    0.2482938

#code for performing one-way ANOVA with post hoc Tukey's HSD test
aov_clustered <- aov(percentMito ~ Condition, data = merged_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition) #p-values are 0, 0, 1.185124e-05



plot(ggplot(data = merged_untreated, aes_string(x = 'Treatment', y = 'totalPuncta', fill = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Total Puncta") +
       scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000), limits = c(0,6000)) +
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

merged_untreated_summary <- merged_untreated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_total = mean(totalPuncta), sd_total = sd(totalPuncta), median_total = median(totalPuncta), count = n())
# 1570.863    1202.5788   1294
# 1591.595    1006.3245   1297
# 1303.182    810.0752    1110

aov_clustered <- aov(totalPuncta ~ Condition, data = merged_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition) #p-values are 9.271995e-01, 4.623662e-08, 4.401909e-06



plot(ggplot(data = merged_untreated, aes_string(x = 'Condition', y = 'avgIntensity_MITO', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Mito Puncta Intensity") +
       scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700,800), limits = c(100,800)) +
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

merged_untreated_summary <- merged_untreated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_avgIntensity_MITO = mean(avgIntensity_MITO), sd_avgIntensity_MITO = sd(avgIntensity_MITO), median_avgIntensity_MITO = median(avgIntensity_MITO), count = n())
#438.8918     91.74529      1237    430.5940
#421.8572     96.32207      494     412.9561
#467.0621     93.84320      831     458.8990

aov_clustered <- aov(avgIntensity_MITO ~ Condition, data = merged_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition) 



plot(ggplot(data = merged_untreated, aes_string(x = 'Condition', y = 'avgIntensity_CYTO', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Cyto Puncta Intensity") +
       scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700,800), limits = c(100,800)) +
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

merged_untreated_summary <- merged_untreated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_avgIntensity_CYTO = mean(avgIntensity_CYTO), sd_avgIntensity_CYTO = sd(avgIntensity_CYTO), median_avgIntensity_CYTO = median(avgIntensity_CYTO), count = n())
#264.5170     28.31143      1237    262.0018
#240.8959     40.23188      494     233.1179
#235.3598     24.71796      831     231.0279

aov_clustered <- aov(avgIntensity_CYTO ~ Condition, data = merged_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition) 



#map single-cell metric data file paths and read into memory
#single-cell metrics are necessary here to compute 'Puncta Density' as it requires a given cell's area that was gathered and saved in a separate Excel Workbook
cell_plate1 <- read_xlsx("D:/20230308_CC_UK_pathVariants_re-crop_test/summary_cell_individual_20230308_updated20260717.xlsx")
cell_plate2 <- read_xlsx("D:/20230324_CC_UK_pathVariants/summary_cell_individual_naOmit_20230324_updated20260717.xlsx")
cell_plate3 <- read_xlsx("D:/20231121_CC_UK_PathVariants/summary_cell_individual_20231121_updated20260717.xlsx")

cell_merged <- merge(cell_plate1, cell_plate2, all = TRUE)
cell_merged <- merge(cell_merged, cell_plate3, all = TRUE)

#remove Condition and Treatment replicate numbers (redundant with other unique identifiers and makes grouping easier)
cell_merged_control_renamed <- cell_merged
cell_merged_control_renamed$Condition <- sub("Control Youth( #2)?", "Youth Control", cell_merged_control_renamed$Condition)
cell_merged_control_renamed$Treatment <- sub("^Untreated Control #[1-4]", "Untreated Control", cell_merged_control_renamed$Treatment)

#create a merged data set containing all 'Untreated Control' data, factored per cell type
cell_merged_untreated <- cell_merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control")

cell_merged_untreated$Condition <- factor(cell_merged_untreated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#pad the 'ROINumber' column with a zero to match formatting between datasheets
cell_merged_untreated$ROINumber <- str_pad(cell_merged_untreated$ROINumber, 2, pad = "0")

#merge data by unique identifiers and create new data columns for relevant arithmetic-based metrics
merged_untreated_grouped_combined <- merge(merged_untreated, cell_merged_untreated, by = c("Plate", "Condition", "Treatment", "WellIdentifier", "ImageNumber", "ROINumber"))
merged_untreated_grouped_combined$punctaDensity <- merged_untreated_grouped_combined$totalPuncta / merged_untreated_grouped_combined$mean_field_Area

merged_untreated_grouped_combined$Condition <- factor(merged_untreated_grouped_combined$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))



plot(ggplot(data = merged_untreated_grouped_combined, aes_string(x = 'Treatment', y = 'punctaDensity', fill = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "Puncta Density per cell") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.5, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
       #facet_wrap(~ Plate, scales = "fixed") +
       scale_y_continuous(breaks = c(0.5,1.0,1.5,2.0,2.5), limits = c(0.5, 2.5)) +
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

aov_clustered <- aov(punctaDensity ~ Condition, data = merged_untreated_grouped_combined)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition)

merged_untreated_grouped_combined_summary <- merged_untreated_grouped_combined %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_punctaDensity = mean(punctaDensity), sd_punctaDensity = sd(punctaDensity), median_punctaDensity = median(punctaDensity))
#1.601246 	0.2883440	  1.569900
#1.410703	  0.2715564	  1.348954      
#1.486344	  0.2328084	  1.451759 



#create a merged data set containing all 'Untreated Control' and '3.5% 1,6-HD (5 minutes)' data, factored per cell type
cell_merged_treated <- cell_merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "3.5% 1,6-HD (5 minutes)")

cell_merged_treated$Condition <- factor(cell_merged_treated$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))

#pad the 'ROINumber' column with a zero to match formatting between datasheets
cell_merged_treated$ROINumber <- str_pad(cell_merged_treated$ROINumber, 2, pad = "0")

#merge data by unique identifiers and create new data columns for relevant arithmetic-based metrics
merged_treated_grouped_combined <- merge(merged_treated, cell_merged_treated, by = c("Plate", "Condition", "Treatment", "WellIdentifier", "ImageNumber", "ROINumber"))
merged_treated_grouped_combined$punctaDensity <- merged_treated_grouped_combined$totalPuncta / merged_treated_grouped_combined$mean_field_Area

merged_treated_grouped_combined$Condition <- factor(merged_treated_grouped_combined$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))
merged_treated_grouped_combined$Treatment <- factor(merged_treated_grouped_combined$Treatment, levels = c("Untreated Control", "3.5% 1,6-HD (5 minutes)"))



plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'percentMito', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Percent  Mitochondrial Puncta") +
       scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6), limits = c(0.0,0.6)) +
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
  summarise(., mean_percentMito = mean(percentMito), sd_percentMito = sd(percentMito), median_percentMito = median(percentMito), count = n())
# 0.2829882   0.04031246    0.2810458        0.2935247   0.04365052   0.2885590
# 0.2376990   0.04700077    0.2381988        0.2430177   0.04757242   0.2437967
# 0.2490522   0.04504677    0.2482938        0.2615600   0.04783091   0.2586667

aov_clustered <- aov(percentMito ~ Condition*Treatment, data = merged_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #5.466670e-04, 6.929827e-01, 7.484353e-04



plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'totalPuncta', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Total Puncta") +
       scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000), limits = c(0,6000)) +
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
  summarise(., mean_total = mean(totalPuncta), sd_total = sd(totalPuncta), median_total = median(totalPuncta), count = n())
# 1570.863    1202.5788   1294.0       1440.312    888.7516   1290.5
# 1591.595    1006.3245   1297.0       1757.294    977.7094   1526.5
# 1303.182    810.0752    1110.0        1407.217    872.6554  1205.0

aov_clustered <- aov(totalPuncta ~ Condition*Treatment, data = merged_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #2.352644e-01, 3.674312e-01, 6.937822e-01



plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'avgIntensity_MITO', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Mito Puncta Intensity") +
       scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700,800), limits = c(100,800)) +
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
  summarise(., mean_avgIntensity_MITO = mean(avgIntensity_MITO), sd_avgIntensity_MITO = sd(avgIntensity_MITO), median_avgIntensity_MITO = median(avgIntensity_MITO))
# 438.8918    91.74529    430.5940        414.8566    103.51692   397.4590
# 421.8572    96.32207    412.9561        305.3751    71.86775    285.3299
# 467.0621    93.84320    458.8990        374.1724    111.28477   356.0765

aov_clustered <- aov(avgIntensity_MITO ~ Condition*Treatment, data = merged_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #2.019623e-04, 8.165455e-10, 8.165455e-10



plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'avgIntensity_CYTO', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Cyto Puncta Intensity") +
       scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700,800), limits = c(100,800)) +
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
  summarise(., mean_avgIntensity_CYTO = mean(avgIntensity_CYTO), sd_avgIntensity_CYTO = sd(avgIntensity_CYTO), median_avgIntensity_CYTO = median(avgIntensity_CYTO))
# 264.5170    28.31143    262.0018        261.6586    34.69589    257.0308
# 240.8959    40.23188    233.1179        240.5543    32.76731    235.7519
# 235.3598    24.71796    231.0279        237.8962    34.53356    229.0502

aov_clustered <- aov(avgIntensity_CYTO ~ Condition*Treatment, data = merged_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #6.111880e-01, 9.999944e-01, 8.555747e-01



plot(ggplot(data = merged_treated_grouped_combined, aes_string(x = 'Condition', y = 'punctaDensity', fill = 'Treatment', color = 'Condition')) +
       scale_color_manual(values = c('#bbbbbb','#737373','#1f1f1f')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "Puncta Density per cell") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.5, position = position_dodge(width = 1)) +
       stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "green", position = position_dodge(width = 1.0), linewidth = 1) +
       stat_summary(fun = "median", geom = "crossbar", width = 0.5, color = "red", position = position_dodge(width = 1.0)) +
       scale_y_continuous(breaks = c(0.5,1.0,1.5,2.0,2.5,3.0), limits = c(0.5, 3.0)) +
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

merged_treated_grouped_combined_summary <- merged_treated_grouped_combined %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_punctaDensity = mean(punctaDensity), sd_punctaDensity = sd(punctaDensity), median_punctaDensity = median(punctaDensity))
#1.601246 	0.2883440	  1.569900      1.723967	  0.3121992	  1.669479
#1.410703	  0.2715564	  1.348954      1.606518	  0.3384844	  1.546084
#1.486344	  0.2328084	  1.451759      1.700312	  0.3229923	  1.653539

aov_clustered <- aov(punctaDensity ~ Condition*Treatment, data = merged_treated_grouped_combined)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #all are zero



plot(ggplot(data = merged_untreated, aes_string(x = 'Treatment', y = 'intensityRatio', fill = "Condition")) +
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
       labs(x = NULL, y = "MITO:CYTO Puncta Intensity Ratio") +
       #scale_x_discrete(breaks = current_row_data, labels = colLabels_2) +
       scale_y_continuous(breaks = c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5), limits = c(0.5,3.5)) +
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
  summarise(., mean_ratio = mean(intensityRatio), sd_ratio = sd(intensityRatio), count = n())

aov_clustered <- aov(intensityRatio ~ Condition, data = merged_untreated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$Condition)



plot(ggplot(data = merged_treated, aes_string(x = 'Condition', y = 'intensityRatio', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightblue')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "MITO:CYTO Puncta Intensity Ratio") +
       scale_y_continuous(breaks = c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), limits = c(0.5,4.0)) +
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

merged_treated_summary <- merged_treated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_percentMito = mean(percentMito), sd_percentMito = sd(percentMito))

aov_clustered <- aov(intensityRatio ~ Condition*Treatment, data = merged_treated)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)



#create a merged data set containing all Treatment data, factored per cell type
merged_treated_all <- merged %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "2.5% 1,6-HD (5 minutes)" | Treatment == "2.5% 1,6-HD (20 minutes)" | Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "3.5% 1,6-HD (20 minutes)" | Treatment == "5.0% 1,6-HD (5 minutes)" | Treatment == "5.0% 1,6-HD (20 minutes)")

merged_treated_all$Treatment <- factor(merged_treated_all$Treatment, levels = c("Untreated Control", "2.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (5 minutes)", "5.0% 1,6-HD (20 minutes)"))
merged_treated_all$Condition <- factor(merged_treated_all$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))



plot(ggplot(data = merged_treated_all, aes_string(x = 'Condition', y = 'percentMito', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Percent  Mitochondrial Puncta") +
       scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6), limits = c(0.0,0.6)) +
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

merged_treated_summary <- merged_treated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_percentMito = mean(percentMito), sd_percentMito = sd(percentMito))

aov_clustered <- aov(percentMito ~ Condition*Treatment, data = merged_treated_all)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) 



plot(ggplot(data = merged_treated_all, aes_string(x = 'Condition', y = 'totalPuncta', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Total Puncta") +
       scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000), limits = c(0,6000)) +
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

merged_treated_summary <- merged_treated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_total = mean(totalPuncta), sd_total = sd(totalPuncta), count = n())

aov_clustered <- aov(totalPuncta ~ Condition*Treatment, data = merged_treated_all)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)



plot(ggplot(data = merged_treated_all, aes_string(x = 'Condition', y = 'avgIntensity_MITO', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Mito Puncta Intensity") +
       scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700,800), limits = c(100,800)) +
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

merged_treated_summary <- merged_treated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_avgIntensity_MITO = mean(avgIntensity_MITO), sd_avgIntensity_MITO = sd(avgIntensity_MITO))

aov_clustered <- aov(avgIntensity_MITO ~ Condition*Treatment, data = merged_treated_all)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)



plot(ggplot(data = merged_treated_all, aes_string(x = 'Condition', y = 'avgIntensity_CYTO', fill = "Treatment", color = "Condition")) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = NULL, y = "Average Cyto Puncta Intensity") +
       scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700,800), limits = c(100,800)) +
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

merged_treated_summary <- merged_treated %>%
  group_by(., Condition, Treatment) %>%
  summarise(., mean_avgIntensity_CYTO = mean(avgIntensity_CYTO), sd_avgIntensity_CYTO = sd(avgIntensity_CYTO))

aov_clustered <- aov(avgIntensity_CYTO ~ Condition*Treatment, data = merged_treated_all)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`)



#create a merged data set containing all Treatment data, factored per cell type, and merged with single-cell metrics to calculate 'Puncta Density'
cell_merged_treated_all <- cell_merged_control_renamed %>%
  filter(., Condition == "Youth Control" | Condition == "P1 (G401S)" | Condition == "P2 (G363D)", Treatment == "Untreated Control" | Treatment == "2.5% 1,6-HD (5 minutes)" | Treatment == "2.5% 1,6-HD (20 minutes)" | Treatment == "3.5% 1,6-HD (5 minutes)" | Treatment == "3.5% 1,6-HD (20 minutes)" | Treatment == "5.0% 1,6-HD (5 minutes)" | Treatment == "5.0% 1,6-HD (20 minutes)")

cell_merged_treated_all$ROINumber <- str_pad(cell_merged_treated_all$ROINumber, 2, pad = "0")

merged_treated_grouped_combined_all <- merge(merged_treated_all, cell_merged_treated_all, by = c("Plate", "Condition", "Treatment", "WellIdentifier", "ImageNumber", "ROINumber"))
merged_treated_grouped_combined_all$punctaDensity <- merged_treated_grouped_combined_all$totalPuncta / merged_treated_grouped_combined_all$mean_field_Area

merged_treated_grouped_combined_all$Condition <- factor(merged_treated_grouped_combined_all$Condition, levels = c("Youth Control", "P2 (G363D)", "P1 (G401S)"))
merged_treated_grouped_combined_all$Treatment <- factor(merged_treated_grouped_combined_all$Treatment, levels = c("Untreated Control", "2.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (5 minutes)", "5.0% 1,6-HD (20 minutes)"))



plot(ggplot(data = merged_treated_grouped_combined_all, aes_string(x = 'Condition', y = 'punctaDensity', fill = 'Treatment', color = 'Condition')) +
       scale_fill_manual(values = c('#bbbbbb','lightgreen','darkgreen','lightblue','darkblue','pink','darkred')) +
       scale_color_manual(values = c('black','black','black')) +
       stat_boxplot(geom = "errorbar", colour = "grey50", width = 0.5, position = position_dodge (width = 1)) +
       stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 1.0, colour = "grey50") +
       labs(x = "Cell Type", y = "Puncta Density per cell") +
       geom_violin(width = 1.0, alpha = 0.6, position = position_dodge(width = 1), size = 1.5) +
       stat_sina(size = 1.5, position = position_dodge(width = 1)) +
              scale_y_continuous(breaks = c(0.5,1.0,1.5,2.0,2.5,3.0), limits = c(0.5, 3.0)) +
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

aov_clustered <- aov(punctaDensity ~ Condition*Treatment, data = merged_treated_grouped_combined_all)
resTukeyHSD <- TukeyHSD(aov_clustered)
df_resTukeyHSD <- as.data.frame(resTukeyHSD$`Condition:Treatment`) #all are zero