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
library(lsmeans)
library(emmeans)
library(lme4)
library(lmerTest)

#user define working root directory
root <<- tcltk::tk_choose.dir()
setwd(root)

#initialize variables needed to parse data
path_quant_metrics_allZ <- NULL
path_quant_metrics_maxIP <- NULL
path_quant_puncta_allZ <- NULL
path_quant_puncta_maxIP <- NULL
metrics_allZ_files <- c()
metrics_maxIP_files <- c()
puncta_allZ_files <- c()
puncta_maxIP_files <- c()
data_readIn <- c()
raw_data_metrics_allZ <- c()
raw_data_metrics_maxIP <- c()
raw_data_puncta_allZ <- c()
raw_data_puncta_maxIP <- c()
missing_data <- c()

#identify sub-directories for metrics to be processed; delete the path information if metrics were not acquired for a given set
subDir_list <- c("/quant_metrics_allZ/", "/quant_metrics_maxIP/", "/quant_puncta_allZ/", "/quant_puncta_maxIP/")

#initialize variables needed to parse data
path_list <- NULL
file_list <- NULL
path_files <- list()
raw_data <- NULL
raw_data_list <- list()
workbooks <- list()
workbook_names <- NULL
worksheet_names <- NULL

#find each sub-directory and generate a single data frame from all files within
for (i in seq(1, length(subDir_list))) {
  
  #look for existing workbooks and avoid redundant processing if found; requires renaming of raw data files to bypass detection
  workbook_names[i] <- paste("raw_data", str_replace_all(subDir_list[i], "/", "_"), "formatted.xlsx", sep = "")
  worksheet_names[i] <- paste("raw_data_", str_replace_all(subDir_list[i], "/", ""), sep = "")
  
  if (!file.exists(workbook_names[i])) {
    
    #check to make sure sub-directory is valid
    if (dir.exists(paste(root, subDir_list[i], sep = ""))) {
      path_list[i] <- (paste(root, subDir_list[i], sep = ""))
      
      #identify all .csv files within sub-directory
      file_list <- append(file_list, list.files(path = path_list[i], pattern = "\\.csv$", full.names = FALSE, recursive = FALSE))
      path_files[[i]] <- file_list
      raw_data[[i]] <- c()
      setwd(path_list[i])
      
      #iterate through each file and extract data
      for (j in path_files[[i]]) {
        data_readIn <- read.csv(j)
        if (nrow(data_readIn) > 0) {
          data_readIn[14] <- toupper(stri_extract_first_regex(j, "(?<=r?)\\w\\d{1,2}(?=[-\\.])"))
          data_readIn[15] <- toupper(stri_extract_first_regex(j, "(?<=-)\\d{3}(?=_)"))
          data_readIn[16] <- toupper(stri_extract_first_regex(j, "(?<=ROI_)[:digit:]{1,2}(?=_)"))
          data_readIn[17] <- stri_extract_first_regex(j, "(?<=_)[[:alnum:]-]+(?=_crop)")
          raw_data <- rbind(raw_data, data_readIn)
        } else {
          missing_data <- rbind(missing_data, j)
        }
      }
      
      #merge all files into a single data frame
      raw_data_list[[i]] <- raw_data
      raw_data_list[[i]] <- raw_data_list[[i]] %>% select(c(17,14,15,16,13,3,4,5,6,7,8,9,10,11,12,2))
      colnames(raw_data_list[[i]])[1] <- "Channel"
      colnames(raw_data_list[[i]])[2] <- "WellIdentifier"
      colnames(raw_data_list[[i]])[3] <- "ImageNumber"
      colnames(raw_data_list[[i]])[4] <- "ROINumber"
      
      #move data into individual spreadsheets
      setwd(root)
      workbooks[[i]] <- createWorkbook()
      addWorksheet(workbooks[[i]], worksheet_names[i])
      writeData(workbooks[[i]], sheet = worksheet_names[i], raw_data_list[[i]])
      saveWorkbook(workbooks[[i]], workbook_names[i], overwrite = TRUE)

      raw_data <- NULL
    }
    
  #implemented to avoid redundant data processing and overwriting data accidentally
  } else {
    print(paste("An Excel workbook with the name '", workbook_names[i], "' already exists and will not be reprocessed!", sep = ""))
  }
  
  file_list <- NULL
}