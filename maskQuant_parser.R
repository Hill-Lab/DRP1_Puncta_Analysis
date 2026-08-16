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

root <<- tcltk::tk_choose.dir()
setwd(root)

data_dir <- paste(root, "/mask_Quant/", sep = "")

maskQuant_files <- c()
maskQuant_files <- append(maskQuant_files, list.files(path = data_dir, pattern = ".csv$", full.names = FALSE, recursive = FALSE))

setwd(data_dir)

data_readIn <- c()
raw_data <- NULL

for (i in maskQuant_files) {
  data_readIn <- read.csv(i)
  data_readIn[16] <- toupper(stri_extract_first_regex(i, "(?<=r?)\\w\\d{1,2}(?=[-\\.])"))
  data_readIn[17] <- toupper(stri_extract_first_regex(i, "(?<=-)\\d{3}(?=_)"))
  data_readIn[18] <- toupper(stri_extract_first_regex(i, "(?<=\\d{3}_)[:digit:]{1,2}(?=_mask)"))
  data_readIn[1,19] <- 1.0000
  data_readIn[2,19] <- 0.5582
  data_readIn[3,19] <- 0.1000
  data_readIn[4,19] <- 1.0000
  data_readIn[5,19] <- 0.5582
  data_readIn[6,19] <- 0.1000
  data_readIn[1,20] <- 255.0
  data_readIn[2,20] <- 150.0
  data_readIn[3,20] <- 25.5
  data_readIn[4,20] <- 255.0
  data_readIn[5,20] <- 150.0
  data_readIn[6,20] <- 25.5
  data_readIn[,21] <- stri_extract_first_regex(data_readIn[,2], "^\\w{4}(?=Mask)")
  raw_data <- rbind(raw_data, data_readIn)
}

raw_data$Plate <- 1
raw_data <- raw_data %>% select(c(22,16:18,21,19,20,3:15,2))
colnames(raw_data)[2] <- "WellIdentifier"
colnames(raw_data)[3] <- "ImageNumber"
colnames(raw_data)[4] <- "ROINumber"
colnames(raw_data)[5] <- "MaskType"
colnames(raw_data)[6] <- "ThresholdCutoff"
colnames(raw_data)[7] <- "ThresholdCutoff_corrected"

raw_data$`Mean_corrected` <- raw_data[,9] * 255
raw_data$`StDev_corrected` <- raw_data[,10] * 255
raw_data$`Median_corrected` <- raw_data[,15] * 255
raw_data <- raw_data %>% select(c(1:9,22,10,23,11:15,24,16:21))

maskQuant_formatted <- raw_data

setwd(root)

wb_maskQuant <- createWorkbook()
addWorksheet(wb_maskQuant, sheet = 1)
writeData(wb_maskQuant, sheet = 1, maskQuant_formatted)
saveWorkbook(wb_maskQuant, "data_maskQuant_formatted.xlsx", overwrite = TRUE)