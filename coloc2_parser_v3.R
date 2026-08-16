library(readxl)
library(openxlsx)
library(dplyr)
library(ggpubr)
library(ggnewscale)
library(stringi)
library(rstatix)
library(tidyverse)
library(reshape2)
library(stringr)
library(formattable)
library(r2r)
library(openxlsx)

mainDir <- "D:/20230308_CC_UK_pathVariants_re-crop_test"
subDirList <- c("coloc2_allZ") # , "coloc2_maxIP")
#subDirPlots <- "R_coloc_plots"
fileNameMetric <- c("All Z-slices FIJI") # , "MaxIP FIJI")

labelHash <- hashmap()

# #number of items must match, RENAME
# colID <- c(1,2,3,4,5)
# #c("Untreated Control #4", "Untreated Control #3", "5.0% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)", "2.5% 1,6-HD (20 minutes)", "Untreated Control #2", "5.0% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (5 minutes)", "Untreated Control #1")
# colLabels <- c("Untreated Control #1", "Washout (5 minutes)", "Washout (20 minutes)", "3.5% 1,2,6-HT (5 minutes)", "3.5% 1,2,6-HT (20 minutes)")
# labelHash[colID] <- colLabels
# 
# #number of items must match, RENAME
# rowID <- c('A','C','D')
# rowLabels <- c("Control Youth #1", "P1 (G401S)", "P2 (G363D)")
# labelHash[rowID] <- rowLabels


# #number of items must match, RENAME
# colID <- c(1,2,3,6,7)
# #c("Untreated Control #4", "Untreated Control #3", "5.0% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)", "2.5% 1,6-HD (20 minutes)", "Untreated Control #2", "5.0% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (5 minutes)", "Untreated Control #1")
# colLabels <- c("TOM20", "Calnexin", "LAMP1", "alpha-Tubulin", "PEX14")
# labelHash[colID] <- colLabels
# 
# #number of items must match, RENAME
# rowID <- c('A','C','D')
# rowLabels <- c("Control Youth #1", "P1 (G401S)", "P2 (G363D)")
# labelHash[rowID] <- rowLabels



#number of items must match, RENAME
colID <- c(2,3,4,5,6,7,8,9,10,11)
#c("Untreated Control #4", "Untreated Control #3", "5.0% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)", "2.5% 1,6-HD (20 minutes)", "Untreated Control #2", "5.0% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "2.5% 1,6-HD (5 minutes)", "Untreated Control #1")
colLabels <- c("Untreated Control #1", "2.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "5.0% 1,6-HD (5 minutes)", "Untreated Control #2", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (20 minutes)", "Untreated Control #3", "Untreated Control #4")
labelHash[colID] <- colLabels

#number of items must match, RENAME
rowID <- c('B','C','D','E','F')
rowLabels <- c("Control Adult", "Control Youth", "P1 (G401S)", "P2 (G363D)", "P3 (L230dup)")
labelHash[rowID] <- rowLabels





for (i in seq(1, length(subDirList))) {
  subDir <- subDirList[i]
  colocFolder <- file.path(mainDir, subDir)
  setwd(colocFolder)
  #dir.create(file.path(colocFolder, subDirPlots), showWarnings = FALSE)  
  #outputFolder <- file.path(colocFolder, subDirPlots)	
  
  data_readIn <- NULL
  data_raw <- NULL
  data_formatted <- NULL
  
  data_coloc <- list.files(path = colocFolder, pattern = "\\.txt$", ignore.case = TRUE, full.names = TRUE)
  
    for (j in data_coloc) {
    name_noExt = sub(".txt", "", j)
    lines <- readLines(j)
    
    for (k in seq(1, length(lines))) {
      if (lines[k] == "RESULTS:")
        index = k
    }
    
    data_readIn <- read_delim(j, delim = ",", skip = (index + 1), col_names = FALSE, show_col_types = FALSE)
    data_readIn <- cbind(name_noExt, data_readIn)
    data_raw <- rbind(data_raw, data_readIn)
  }
  
  
  data_formatted <- data_raw %>%
    spread(X1, X2) %>%
    select("name_noExt", "Channel 1 Mean", "Channel 2 Mean", "Pearson's R value (no threshold)", "Pearson's R value (below threshold)", "Pearson's R value (above threshold)", "Manders' M1 (Above zero intensity of Ch2)", "Manders' M2 (Above zero intensity of Ch1)", "Manders' tM1 (Above autothreshold of Ch2)", "Manders' tM2 (Above autothreshold of Ch1)") #, "Costes P-Value", "Costes Shuffled Mean", "Costes Shuffled Std.D.", "Ratio of rand. Pearsons >= actual Pearsons value ") #channel 1 mean is  just DRP1 here
  
  data_formatted <- cbind(WellIdentifier = toupper(stri_extract_first_regex(data_formatted[[1]], "(?<=r?)\\w\\d{1,2}(?=[-\\.])")), data_formatted)
  data_formatted <- cbind(ImageNumber = toupper(stri_extract_first_regex(data_formatted[[2]], "(?<=-)\\d{3}(?=_)")), data_formatted)
  data_formatted <- cbind(ROINumber = toupper(stri_extract_first_regex(data_formatted[[3]], "(?<=_ROI_)[:digit:]{1,2}(?=_anti)")), data_formatted)
  
  
  
  data_formatted <- data_formatted %>%
    #select(c(3,2,1,5,6,7,8,4)) %>%
    select(c(3,2,1,5:13,4)) %>%
    rename("WellIdentifier" = 1, "ImageNumber" = 2, "ROINumber" = 3, "Ch1 Mean" = 4, "Ch2 Mean" = 5, "Pearsons" = 6, "Pearsons (below threshold)" = 7, "Pearsons (above threshold)" = 8, "Manders M1" = 9, "Manders M2" = 10, "Manders tM1" = 11, "Manders tM2" = 12, "FileName" = 13) #Costes P-value" = 13, "Costes Shuffled Mean" = 14, "Costes Shuffled StdDev" = 15, "Random Pearsons Ratio" = 16, "FileName" = 17)
    #rename("WellIdentifier" = 1, "ImageNumber" = 2, "ROINumber" = 3, "Pearsons" = 4, "MeanIntensity" = 5, "Manders" = 6, "MaxThreshold" = 7, "FileName" = 8)
  
  #data_formatted[, 3] <- as.numeric(as.character(data_formatted[, 3]))
  data_formatted[, 4] <- as.numeric(as.character(data_formatted[, 4]))
  data_formatted[, 5] <- as.numeric(as.character(data_formatted[, 5]))
  data_formatted[, 6] <- as.numeric(as.character(data_formatted[, 6]))
  data_formatted[, 7] <- as.numeric(as.character(data_formatted[, 7]))
  data_formatted[, 8] <- as.numeric(as.character(data_formatted[, 8]))
  data_formatted[, 9] <- as.numeric(as.character(data_formatted[, 9]))
  data_formatted[, 10] <- as.numeric(as.character(data_formatted[, 10]))
  data_formatted[, 11] <- as.numeric(as.character(data_formatted[, 11]))
  data_formatted[, 12] <- as.numeric(as.character(data_formatted[, 12]))
  data_formatted[, 13] <- as.numeric(as.character(data_formatted[, 13]))
  #data_formatted[, 14] <- as.numeric(as.character(data_formatted[, 14]))
  #data_formatted[, 15] <- as.numeric(as.character(data_formatted[, 15]))
  #data_formatted[, 16] <- as.numeric(as.character(data_formatted[, 16]))
  
  data_formatted$Plate <- 1
  data_formatted$Condition <- ""
  data_formatted$Treatment <- ""
  
  #data_formatted <- data_formatted[c(9:11,1:8)]
  data_formatted <- data_formatted[c(14,15,16,1:13)]         #data_formatted[c(18,19,20,1:17)]
  
  for (j in seq(1, length(data_formatted$WellIdentifier))) {
    irow <- stri_extract_first_regex(data_formatted[j, 4], "[:alpha:]")
    icol <- stri_extract_first_regex(data_formatted[j, 4], "[:digit:]{1,2}")
    data_formatted[j, 2] <- labelHash[irow]
    data_formatted[j, 3] <- labelHash[as.numeric(icol)]
  }
  
  write.xlsx(data_formatted, paste(mainDir, "/", subDir, "_formatted_manders_expanded_20230308.xlsx", sep = ""))  
}
  
#write.xlsx(data_formatted, paste(mainDir, "/coloc2_formatted_20231121_new.xlsx", sep = ""))  
  
  