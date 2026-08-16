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



cores <- detectCores()
registerDoParallel(cores = cores)

root <<- tcltk::tk_choose.dir()
setwd(root)

data_dir <- paste(root, "/punctaParse_masks/", sep = "")

data_3DIntensity_formatted <-  NULL
data_3DCompactness_formatted <- NULL
data_3DVolume_formatted <- NULL

files_3DIntensity <-  c()
files_3DCompactness <- c()
files_3DVolume <- c()

data_readIn <- c()
raw_data <- NULL
missing_data <- c()


files_3DIntensity <- append(files_3DIntensity, list.files(path = data_dir, pattern = "^.*_3D-Intensity_.*\\.csv$", full.names = FALSE, recursive = FALSE))
#files_3DCompactness <- append(files_3DCompactness, list.files(path = data_dir, pattern = "^.*_3D-Compactness_.*\\.csv$", full.names = FALSE, recursive = FALSE))
#files_3DVolume <- append(files_3DVolume, list.files(path = data_dir, pattern = "^.*_3D-Volume_.*\\.csv$", full.names = FALSE, recursive = FALSE))


setwd(data_dir)
for (i in files_3DIntensity) {
  data_readIn <- read.csv(i)
  if (nrow(data_readIn) > 0) {
    index <- data_readIn[which(str_detect(data_readIn$Label, "XORstack:\\w{4}Mask_mult_cropped"), arr.ind = TRUE)[1],1]
    if (index != 1) {
      data_readIn <- data_readIn[-c(1:index-1),]
      data_readIn <- data_readIn %>% select(c(1,2,14:23))
    }
    data_readIn[13] <- toupper(stri_extract_first_regex(i, "(?<=_)\\w{4}(?=Mask)"))
    data_readIn[14] <- toupper(stri_extract_first_regex(i, "(?<=r?)\\w\\d{1,2}(?=[-\\.])"))
    data_readIn[15] <- toupper(stri_extract_first_regex(i, "(?<=-)\\d{3}(?=_)"))
    data_readIn[16] <- toupper(stri_extract_first_regex(i, "(?<=\\d{3}_)[:digit:]{1,2}(?=_\\w{4}Mask)")) #B10-001_01_CytoMask_3D-Intensity_MSER_STEP
    combined_alg <- stri_extract_first_regex(i, "(?<=-Intensity_)[[:alnum:]_]*")
    split_alg <- strsplit(combined_alg, "_")
    data_readIn[17] <- combined_alg
    data_readIn[18] <- split_alg[[1]][1]
    data_readIn[19] <- split_alg[[1]][2]
    raw_data <- rbind(raw_data, data_readIn)
  }
}

raw_data$Plate <- 3
raw_data <- raw_data %>% select(c(20,13:16,3,17:19,4:12))
colnames(raw_data)[2] <- "MaskType"
colnames(raw_data)[3] <- "WellIdentifier"
colnames(raw_data)[4] <- "ImageNumber"
colnames(raw_data)[5] <- "ROINumber"
colnames(raw_data)[6] <- "ObjectNumber"
colnames(raw_data)[7] <- "Algorithm"
colnames(raw_data)[8] <- "Criteria"
colnames(raw_data)[9] <- "Method"

data_3DIntensity_formatted <- raw_data
raw_data <- NULL

# 
# for (i in files_3DCompactness) {
#   data_readIn <- read.csv(i)
#   if (nrow(data_readIn) > 0) {
#     data_readIn[12] <- toupper(stri_extract_first_regex(i, "(?<=r?)\\w\\d{1,2}(?=[-\\.])"))
#     data_readIn[13] <- toupper(stri_extract_first_regex(i, "(?<=-)\\d{3}(?=_)"))
#     data_readIn[14] <- toupper(stri_extract_first_regex(i, "(?<=\\d{3}_)[:digit:]{1,2}(?=_3D)"))
#     combined_alg <- stri_extract_first_regex(i, "(?<=-Compactness_)[[:alnum:]_]*")
#     split_alg <- strsplit(combined_alg, "_")
#     data_readIn[15] <- combined_alg
#     data_readIn[16] <- split_alg[[1]][1]
#     data_readIn[17] <- split_alg[[1]][2]
#     raw_data <- rbind(raw_data, data_readIn)
#   }
# }
# 
# raw_data$Plate <- 3
# raw_data <- raw_data %>% select(c(18,12:14,3,15:17,4:11))
# colnames(raw_data)[2] <- "WellIdentifier"
# colnames(raw_data)[3] <- "ImageNumber"
# colnames(raw_data)[4] <- "ROINumber"
# colnames(raw_data)[5] <- "ObjectNumber"
# colnames(raw_data)[6] <- "Algorithm"
# colnames(raw_data)[7] <- "Criteria"
# colnames(raw_data)[8] <- "Method"
# 
# data_3DCompactness_formatted <- raw_data
# raw_data <- NULL
# 
# 
# for (i in files_3DVolume) {
#   data_readIn <- read.csv(i)
#   if (nrow(data_readIn) > 0) {
#     data_readIn[6] <- toupper(stri_extract_first_regex(i, "(?<=r?)\\w\\d{1,2}(?=[-\\.])"))
#     data_readIn[7] <- toupper(stri_extract_first_regex(i, "(?<=-)\\d{3}(?=_)"))
#     data_readIn[8] <- toupper(stri_extract_first_regex(i, "(?<=\\d{3}_)[:digit:]{1,2}(?=_3D)"))
#     combined_alg <- stri_extract_first_regex(i, "(?<=-Volume_)[[:alnum:]_]*")
#     split_alg <- strsplit(combined_alg, "_")
#     data_readIn[9] <- combined_alg
#     data_readIn[10] <- split_alg[[1]][1]
#     data_readIn[11] <- split_alg[[1]][2]
#     raw_data <- rbind(raw_data, data_readIn)
#   }
# }
# 
# raw_data$Plate <- 3
# raw_data <- raw_data %>% select(c(12,6:8,3,9:11,4,5))
# colnames(raw_data)[2] <- "WellIdentifier"
# colnames(raw_data)[3] <- "ImageNumber"
# colnames(raw_data)[4] <- "ROINumber"
# colnames(raw_data)[5] <- "ObjectNumber"
# colnames(raw_data)[6] <- "Algorithm"
# colnames(raw_data)[7] <- "Criteria"
# colnames(raw_data)[8] <- "Method"
# 
# data_3DVolume_formatted <- raw_data
# raw_data <- NULL



setwd(root)

wb_3DIntensity <- createWorkbook()
#wb_3DCompactness <- createWorkbook()
#wb_3DVolume <- createWorkbook()

numRows <- nrow(data_3DIntensity_formatted)
# rows_3DIntensity <- nrow(data_3DIntensity_formatted)
# rows_3DCompactness <- nrow(data_3DCompactness_formatted)
# rows_3DVolume <- nrow(data_3DVolume_formatted)

maxExcelRows <- 1048575
numSheets <- NULL
lowInd <- 1
highInd <- maxExcelRows

if (numRows > maxExcelRows) {
  numSheets <- ceiling(numRows/maxExcelRows)
} else {
  numSheets <- 1
}

for (i in  seq(1, numSheets)) {
  addWorksheet(wb_3DIntensity, sheet = i)
  subset_df_3DIntensity <- data_3DIntensity_formatted[c(lowInd:highInd),]
  #subset_df_3DIntensity <- na.omit(subset_df_3DIntensity)
  writeData(wb_3DIntensity, sheet = i, subset_df_3DIntensity)
  
  # addWorksheet(wb_3DCompactness, sheet = i)
  # subset_df_3DCompactness <- data_3DCompactness_formatted[c(lowInd:highInd),]
  # subset_df_3DCompactness <- na.omit(subset_df_3DCompactness)
  # writeData(wb_3DCompactness, sheet = i, subset_df_3DCompactness)
  # 
  # addWorksheet(wb_3DVolume, sheet = i)
  # subset_df_3DVolume <- data_3DVolume_formatted[c(lowInd:highInd),]
  # subset_df_3DVolume <- na.omit(subset_df_3DVolume)
  # writeData(wb_3DVolume, sheet = i, subset_df_3DVolume)
  
  lowInd <- lowInd + maxExcelRows
  highInd <- highInd + maxExcelRows
}
saveWorkbook(wb_3DIntensity, "data_3DIntensity_masks_formatted.xlsx", overwrite = TRUE)
# saveWorkbook(wb_3DCompactness, "data_3DCompactness_formatted.xlsx", overwrite = TRUE)
# saveWorkbook(wb_3DVolume, "data_3DVolume_formatted.xlsx", overwrite = TRUE)




