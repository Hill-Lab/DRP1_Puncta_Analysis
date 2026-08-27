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

#seems to help with processing massive amounts of data
cores <- detectCores()
registerDoParallel(cores = cores)

#user define working root directory
root <<- tcltk::tk_choose.dir()
setwd(root)

#establish sub-directory
data_dir <- paste(root, "/punctaParse_masks/", sep = "")

#initialize variables needed to parse data
data_3DIntensity_formatted <-  NULL
data_3DCompactness_formatted <- NULL
data_3DVolume_formatted <- NULL
files_3DIntensity <-  c()
files_3DCompactness <- c()
files_3DVolume <- c()
data_readIn <- c()
raw_data <- NULL
missing_data <- c()

#gather all files 
files_3DIntensity <- append(files_3DIntensity, list.files(path = data_dir, pattern = "^.*_3D-Intensity_.*\\.csv$", full.names = FALSE, recursive = FALSE))
files_3DCompactness <- append(files_3DCompactness, list.files(path = data_dir, pattern = "^.*_3D-Compactness_.*\\.csv$", full.names = FALSE, recursive = FALSE))
files_3DVolume <- append(files_3DVolume, list.files(path = data_dir, pattern = "^.*_3D-Volume_.*\\.csv$", full.names = FALSE, recursive = FALSE))

#set working directory to "/punctaParse_masks/" folder
setwd(data_dir)

#iterate through 3D Intensity data files
for (i in files_3DIntensity) {
  data_readIn <- read.csv(i)
  
  #ignore exceptions where analysis failed and no data was collected (usually blank/corrupted images)
  if (nrow(data_readIn) > 0) {
    
    #detect first instance of data if not in the first row (this only happens for the 3D Intensity data for some reason)
    index <- data_readIn[which(str_detect(data_readIn$Label, "XORstack:\\w{4}Mask_mult_cropped"), arr.ind = TRUE)[1],1]
    if (index != 1) {
      data_readIn <- data_readIn[-c(1:index-1),]
      data_readIn <- data_readIn %>% select(c(1,2,14:23))
    }
    
    #extract relevant parameters from file name
    data_readIn[13] <- toupper(stri_extract_first_regex(i, "(?<=_)\\w{4}(?=Mask)"))
    data_readIn[14] <- toupper(stri_extract_first_regex(i, "(?<=r?)\\w\\d{1,2}(?=[-\\.])"))
    data_readIn[15] <- toupper(stri_extract_first_regex(i, "(?<=-)\\d{3}(?=_)"))
    data_readIn[16] <- toupper(stri_extract_first_regex(i, "(?<=\\d{3}_)[:digit:]{1,2}(?=_\\w{4}Mask)"))
    combined_alg <- stri_extract_first_regex(i, "(?<=-Intensity_)[[:alnum:]_]*")
    split_alg <- strsplit(combined_alg, "_")
    data_readIn[17] <- combined_alg
    data_readIn[18] <- split_alg[[1]][1]
    data_readIn[19] <- split_alg[[1]][2]
    
    #add extracted parameters and raw data to one data frame
    raw_data <- rbind(raw_data, data_readIn)
  }
}

#add a column for plate replicate
raw_data$Plate <- 1   #UPDATE WITH REPLICATE PLATE NUMBER MANUALLY

#reorder and rename columns
raw_data <- raw_data %>% select(c(20,13:16,3,17:19,4:12))
colnames(raw_data)[2] <- "MaskType"
colnames(raw_data)[3] <- "WellIdentifier"
colnames(raw_data)[4] <- "ImageNumber"
colnames(raw_data)[5] <- "ROINumber"
colnames(raw_data)[6] <- "ObjectNumber"
colnames(raw_data)[7] <- "Algorithm"
colnames(raw_data)[8] <- "Criteria"
colnames(raw_data)[9] <- "Method"

#move data to unique data frame
data_3DIntensity_formatted <- raw_data


#reset raw_data for next batch
raw_data <- NULL

#iterate through 3D Compactness data files
for (i in files_3DCompactness) {
  data_readIn <- read.csv(i)
  
  #ignore exceptions where analysis failed and no data was collected (usually blank/corrupted images)
  if (nrow(data_readIn) > 0) {
    
    #extract relevant parameters from file name
    data_readIn[12] <- toupper(stri_extract_first_regex(i, "(?<=_)\\w{4}(?=Mask)"))
    data_readIn[13] <- toupper(stri_extract_first_regex(i, "(?<=r?)\\w\\d{1,2}(?=[-\\.])"))
    data_readIn[14] <- toupper(stri_extract_first_regex(i, "(?<=-)\\d{3}(?=_)"))
    data_readIn[15] <- toupper(stri_extract_first_regex(i, "(?<=\\d{3}_)[:digit:]{1,2}(?=_\\w{4}Mask)")) #B10-001_01_CytoMask_3D-Intensity_MSER_STEP
    combined_alg <- stri_extract_first_regex(i, "(?<=-Compactness_)[[:alnum:]_]*")
    split_alg <- strsplit(combined_alg, "_")
    data_readIn[16] <- combined_alg
    data_readIn[17] <- split_alg[[1]][1]
    data_readIn[18] <- split_alg[[1]][2]
    
    #add extracted parameters and raw data to one data frame
    raw_data <- rbind(raw_data, data_readIn)
  }
}

#add a column for plate replicate
raw_data$Plate <- 1   #UPDATE WITH REPLICATE PLATE NUMBER MANUALLY

#reorder and rename columns
raw_data <- raw_data %>% select(c(19,12:15,3,16:18,4:11))
colnames(raw_data)[2] <- "MaskType"
colnames(raw_data)[3] <- "WellIdentifier"
colnames(raw_data)[4] <- "ImageNumber"
colnames(raw_data)[5] <- "ROINumber"
colnames(raw_data)[6] <- "ObjectNumber"
colnames(raw_data)[7] <- "Algorithm"
colnames(raw_data)[8] <- "Criteria"
colnames(raw_data)[9] <- "Method"

#move data to unique data frame
data_3DCompactness_formatted <- raw_data


#reset raw_data for next batch
raw_data <- NULL

#iterate through 3D Volume data files
for (i in files_3DVolume) {
  data_readIn <- read.csv(i)
  
  #ignore exceptions where analysis failed and no data was collected (usually blank/corrupted images)
  if (nrow(data_readIn) > 0) {
    
    #extract relevant parameters from file name
    data_readIn[6] <- toupper(stri_extract_first_regex(i, "(?<=_)\\w{4}(?=Mask)"))
    data_readIn[7] <- toupper(stri_extract_first_regex(i, "(?<=r?)\\w\\d{1,2}(?=[-\\.])"))
    data_readIn[8] <- toupper(stri_extract_first_regex(i, "(?<=-)\\d{3}(?=_)"))
    data_readIn[9] <- toupper(stri_extract_first_regex(i, "(?<=\\d{3}_)[:digit:]{1,2}(?=_\\w{4}Mask)")) #B10-001_01_CytoMask_3D-Intensity_MSER_STEP
    combined_alg <- stri_extract_first_regex(i, "(?<=-Volume_)[[:alnum:]_]*")
    split_alg <- strsplit(combined_alg, "_")
    data_readIn[10] <- combined_alg
    data_readIn[11] <- split_alg[[1]][1]
    data_readIn[12] <- split_alg[[1]][2]
    
    #add extracted parameters and raw data to one data frame
    raw_data <- rbind(raw_data, data_readIn)
  }
}

#add a column for plate replicate
raw_data$Plate <- 1   #UPDATE WITH REPLICATE PLATE NUMBER MANUALLY

#reorder and rename columns
raw_data <- raw_data %>% select(c(13,6:9,3,10:12,4,5))
colnames(raw_data)[2] <- "MaskType"
colnames(raw_data)[3] <- "WellIdentifier"
colnames(raw_data)[4] <- "ImageNumber"
colnames(raw_data)[5] <- "ROINumber"
colnames(raw_data)[6] <- "ObjectNumber"
colnames(raw_data)[7] <- "Algorithm"
colnames(raw_data)[8] <- "Criteria"
colnames(raw_data)[9] <- "Method"

#move data to unique data frame
data_3DVolume_formatted <- raw_data


#reset raw_data for next batch
raw_data <- NULL

#move to root directory
setwd(root)

#create Excel Workbook objects to store data
wb_3DIntensity <- createWorkbook()
wb_3DCompactness <- createWorkbook()
wb_3DVolume <- createWorkbook()

#store number of rows; "data_3DIntensity_formatted" is used for this call but all formatted files contain the same number of data points
numRows <- nrow(data_3DIntensity_formatted)

#calculate the number of Excel Workbook tabs needed to store all data
maxExcelRows <- 1048575   #this value is one fewer than the maximum number of rows a single tab in an Excel Workbook can store before data is lost
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
  subset_df_3DIntensity <- data_3DIntensity_formatted[c(lowInd:highInd),]
  writeData(wb_3DIntensity, sheet = i, subset_df_3DIntensity)
  
  addWorksheet(wb_3DCompactness, sheet = i)
  subset_df_3DCompactness <- data_3DCompactness_formatted[c(lowInd:highInd),]
  writeData(wb_3DCompactness, sheet = i, subset_df_3DCompactness)

  addWorksheet(wb_3DVolume, sheet = i)
  subset_df_3DVolume <- data_3DVolume_formatted[c(lowInd:highInd),]
  writeData(wb_3DVolume, sheet = i, subset_df_3DVolume)
  
  #index and track which data has been stored already
  lowInd <- lowInd + maxExcelRows
  highInd <- highInd + maxExcelRows
}

#save Excel Workbooks for each data set
saveWorkbook(wb_3DIntensity, "data_3DIntensity_masks_formatted.xlsx", overwrite = TRUE)
saveWorkbook(wb_3DCompactness, "data_3DCompactness_formatted.xlsx", overwrite = TRUE)
saveWorkbook(wb_3DVolume, "data_3DVolume_formatted.xlsx", overwrite = TRUE)