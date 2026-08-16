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


cores <- detectCores()
registerDoParallel(cores = cores)

root <<- tcltk::tk_choose.dir()
setwd(root)

data_metrics_maxIP <- read_xlsx("raw_data_quant_metrics_allZ_formatted.xlsx") 
data_puncta_maxIP <- read_xlsx("raw_data_quant_puncta_allZ_formatted.xlsx")

condition_hash <- hashmap()
treatment_hash <- hashmap()
treatmentLength_hash <- hashmap()



# colID <- c(1,2,3,4,5)
# colID_len <- length(colID)
# rowID <- c('A','C','D')
# rowID_len <- length(rowID)
# 
# #Mapped to Row
# condition <- c("Control Youth #1", "P1 (G401S)", "P2 (G363D)")
# #Mapped to Col
# treatment <- c("Untreated Control #1", "Washout (5 minutes)", "Washout (20 minutes)", "3.5% 1,2,6-HT (5 minutes)", "3.5% 1,2,6-HT (20 minutes)")



# colID <- c(1,2,3,6,7)
# colID_len <- length(colID)
# rowID <- c('A','C','D')
# rowID_len <- length(rowID)
# 
# #Mapped to Row
# condition <- c("Control Youth #1", "P1 (G401S)", "P2 (G363D)")
# #Mapped to Col
# treatment <- c("TOM20", "Calnexin", "LAMP1", "alpha-Tubulin", "PEX14)")



# colID <- c(1,2,3)
# colID_len <- length(colID)
# rowID <- c('A','B')
# rowID_len <- length(rowID)
# 
# #Mapped to Row
# condition <- c("RPE", "U2OS")
# #Mapped to Col
# treatment <- c("Untreated Control", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes) and washout (2.5 minutes)")
# 
# 
# 
# condition_hash[rowID] <- condition
# treatment_hash[colID] <- treatment



colID <- c(2,3,4,5,6,7,8,9,10,11)
colID_len <- length(colID)
rowID <- c('B','C','D','E','F')
rowID_len <- length(rowID)

#Mapped to Row
condition <- c("Control Adult", "Control Youth", "P1 (G401S)", "P2 (G363D)", "P3 (L230dup)")
#Mapped to Col
treatment <- c("Untreated Control #1", "2.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "5.0% 1,6-HD (5 minutes)", "Untreated Control #2", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (20 minutes)", "Untreated Control #3", "Untreated Control #4")



condition_hash[rowID] <- condition
treatment_hash[colID] <- treatment





data_metrics_maxIP$Plate <- 1 #CHANGE WITH PLATE NUMBER
data_metrics_maxIP$Condition <- ""
data_metrics_maxIP$Treatment <- ""
n_cols <- ncol(data_metrics_maxIP)
n_rows <- nrow(data_metrics_maxIP)

for (i in seq(1, n_rows)) {
  irow <- stri_extract_first_regex(data_metrics_maxIP[i, 2], "[:alpha:]")
  icol <- stri_extract_first_regex(data_metrics_maxIP[i, 2], "[:digit:]{1,2}")
  data_metrics_maxIP[i, n_cols-1] <- condition_hash[irow]
  data_metrics_maxIP[i, n_cols] <- treatment_hash[as.numeric(icol)]
}
data_metrics_maxIP <- data_metrics_maxIP %>%
  select(c((n_cols-2), (n_cols-1), n_cols, (1:(n_cols-3))))

summary_cell <- data_metrics_maxIP %>%
  group_by(Plate, Condition, Treatment, WellIdentifier, ImageNumber, ROINumber) %>%
  summarise(., mean_field_MFI = mean(Mean), mean_field_sdMFI = mean(StdDev), CoV = (StdDev / Mean), mean_field_Area = mean(Area), StdDevAdj = ((StdDev/IntDen)*1000), median_field_MdFI = mean(Median), IntegratedDensity = mean(IntDen), RawIntegratedDensity = mean(RawIntDen)) %>%
  ungroup()

write.xlsx(summary_cell, paste(root, "summary_cell_individual_updated20260808.xlsx", sep = "/"))


data_puncta_maxIP$Plate <- 1 #CHANGE WITH PLATE NUMBER
data_puncta_maxIP$Condition <- ""
data_puncta_maxIP$Treatment <- ""
n_cols <- ncol(data_puncta_maxIP)
n_rows <- nrow(data_puncta_maxIP)

for (i in seq(1, n_rows)) {
  irow <- stri_extract_first_regex(data_puncta_maxIP[i, 2], "[:alpha:]")
  icol <- stri_extract_first_regex(data_puncta_maxIP[i, 2], "[:digit:]{1,2}")
  data_puncta_maxIP[i, n_cols-1] <- condition_hash[irow]
  data_puncta_maxIP[i, n_cols] <- treatment_hash[as.numeric(icol)]
}
data_puncta_maxIP <- data_puncta_maxIP %>%
  select(c((n_cols-2), (n_cols-1), n_cols, (1:(n_cols-3))))

summary_puncta <- data_puncta_maxIP

write.xlsx(summary_puncta, paste(root, "summary_puncta_all.xlsx", sep = "/"))