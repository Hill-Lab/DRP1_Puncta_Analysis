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
library(RColorBrewer)
library(MetBrewer)

#prevents scientific notation formatting
options(scipen=999) 

#seems to help with processing massive amounts of data
cores <- detectCores()
registerDoParallel(cores = cores)

#map data files
root <- c("D:/20250108_Drp1-WT-G363D-G401S_LLPS", "D:/20250110_Drp1-WT-G363D_LLPS", "D:/20250113_Drp1-WT-G363D_LLPS", "D:/20250205_Drp1-G401S_LLPS", "D:/20250206_Drp1-G401S_LLPS", "D:/20250207_Drp1-G401S_LLPS")

intensity_mean_data <- c()

#iterate through data files and read into memory all 'intensityResults_mean' data (un-thresholded mean)
for (i in seq(1, length(root))) {
  intensity_mean <- file.path(root[i], "intensityResults_mean")
  intensity_mean_files <- c()
  
  #gather files
  intensity_mean_files <- append(intensity_mean_files, list.files(path = intensity_mean, pattern = "^.*_results-.*\\.csv$", full.names = FALSE, recursive = FALSE))
  data_readIn <- c()
  
  setwd(intensity_mean)
  
  #iterate through 'intensityResults_mean' data
  for (j in intensity_mean_files) {
    data_readIn <- read.csv(j)
    data_readIn <- data_readIn[c(1:3),]
    data_readIn[is.na(data_readIn)] <- 0
    
    #parse some information from data
    data_readIn[15] <- toupper(stri_extract_first_regex(j, "^\\w{2,5}(?=_)"))  #protein
    data_readIn[16] <- toupper(stri_extract_first_regex(j, "(?<=_)(\\d\\.)?\\d{1,2}(?=uM)")) #protein conc
    data_readIn[17] <- toupper(stri_extract_first_regex(j, "(?<=_)(\\d\\.)?\\d{1,2}(?=PEG)")) #PEG conc
    data_readIn[18] <- toupper(stri_extract_first_regex(j, "(?<=_)\\d(?=_)")) #tr
    
    #add new columns for various metrics
    data_readIn[1, 19] <- "Field"
    data_readIn[2, 19] <- "Object" #object/background
    data_readIn[3, 19] <- "Background" #object/background
    
    #compute metrics from other column data
    data_readIn[20] <- (data_readIn[2,4]*(data_readIn[2,11]/100))/((data_readIn[3,4]*(data_readIn[3,11]/100)) + (data_readIn[2,4]*(data_readIn[2,11]/100))) #propensity
    data_readIn[21] <- log10(data_readIn[20]) #log propensity
    data_readIn[22] <- data_readIn[2,9]/(data_readIn[3,9] + data_readIn[2,9])
    data_readIn[23] <- log10(data_readIn[22])
    data_readIn[24] <- data_readIn[1,5]/data_readIn[1,4] #CoV
    data_readIn[25] <- i #plate
    intensity_mean_data <- rbind(intensity_mean_data, data_readIn)
  }
}

#reorder and rename columns
intensity_mean_data <- intensity_mean_data %>% select(c(25,15:24,3:14,2))
colnames(intensity_mean_data)[1] <- "Plate"
colnames(intensity_mean_data)[2] <- "Protein"
colnames(intensity_mean_data)[3] <- "Protein Conc"
colnames(intensity_mean_data)[4] <- "PEG Percent"
colnames(intensity_mean_data)[5] <- "Replicate"
colnames(intensity_mean_data)[6] <- "Type"
colnames(intensity_mean_data)[7] <- "Propensity"
colnames(intensity_mean_data)[8] <- "Log Propensity"
colnames(intensity_mean_data)[9] <- "Norm Propensity"
colnames(intensity_mean_data)[10] <- "Log Norm Propensity"
colnames(intensity_mean_data)[11] <- "Coefficient of Variation"
colnames(intensity_mean_data)[20] <- "PercentArea"

#replace N/A  and infinite values with zero
intensity_mean_data[is.na(intensity_mean_data)] <- 0
intensity_mean_data <- intensity_mean_data %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

#group metrics across replicates and compute mean values per population
intensity_mean_data_formatted <- intensity_mean_data %>%
  group_by(., Protein, `Protein Conc`, `PEG Percent`) %>%
  summarise(., mean_Propensity = mean(Propensity), mean_Log_Propensity = log10(mean_Propensity), mean_Norm_Propensity = mean(`Norm Propensity`), mean_Log_Norm_Propensity = log10(mean_Norm_Propensity), mean_CoV = mean(`Coefficient of Variation`), MFI = mean(Mean), sdMFI = mean(StdDev)) %>%
  mutate(., `Protein Conc` = as.factor(`Protein Conc`), `PEG Percent` = as.factor(`PEG Percent`)) %>%
  ungroup()

#reorder data within columns for plotting
intensity_mean_data_formatted$Protein <- factor(intensity_mean_data_formatted$Protein, levels = c("WT", "G401S", "G363D"))
intensity_mean_data_formatted$`Protein Conc` <- factor(intensity_mean_data_formatted$`Protein Conc`, levels = c("0", "0.5", "1", "2.5", "5", "10"))
intensity_mean_data_formatted$`PEG Percent` <- factor(intensity_mean_data_formatted$`PEG Percent`, levels = c("0", "1", "2.5", "5", "10", "20"))

#replace infinite values with zero (introduced by coercion)
intensity_mean_data_formatted <- intensity_mean_data_formatted %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(intensity_mean_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = met.brewer(name = "OKeeffe2", type = "discrete", direction = 1), name = "Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_mean_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Norm_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = met.brewer(name = "OKeeffe2", type = "discrete", direction = 1), name = "Norm Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_mean_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Log_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = met.brewer(name = "OKeeffe2", type = "discrete", direction = 1), name = "Log Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_mean_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Log_Norm_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = met.brewer(name = "OKeeffe2", type = "discrete", direction = 1), name = "Log Norm Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

intensity_400_data <- c()



#iterate through data files and read into memory all 'intensityResults_400' data (thresholded above 400 a.u.)
for (i in seq(1, length(root))) {
  intensity_400 <- file.path(root[i], "intensityResults_400")
  intensity_400_files <- c()
  
  #gather files
  intensity_400_files <- append(intensity_400_files, list.files(path = intensity_400, pattern = "^.*_results-.*\\.csv$", full.names = FALSE, recursive = FALSE))
  data_readIn <- c()
  
  setwd(intensity_400)
  
  #iterate through 'intensityResults_400' data
  for (j in intensity_400_files) {
    data_readIn <- read.csv(j)
    data_readIn <- data_readIn[c(1:3),]
    data_readIn[is.na(data_readIn)] <- 0
    
    #parse some information from data
    data_readIn[15] <- toupper(stri_extract_first_regex(j, "^\\w{2,5}(?=_)"))  #protein
    data_readIn[16] <- toupper(stri_extract_first_regex(j, "(?<=_)(\\d\\.)?\\d{1,2}(?=uM)")) #protein conc
    data_readIn[17] <- toupper(stri_extract_first_regex(j, "(?<=_)(\\d\\.)?\\d{1,2}(?=PEG)")) #PEG conc
    data_readIn[18] <- toupper(stri_extract_first_regex(j, "(?<=_)\\d(?=_)")) #tr
    
    #add new columns for various metrics
    data_readIn[1, 19] <- "Field"
    data_readIn[2, 19] <- "Object" #object/background
    data_readIn[3, 19] <- "Background" #object/background
    
    #compute metrics from other column data
    data_readIn[20] <- (data_readIn[2,4]*(data_readIn[2,11]/100))/((data_readIn[3,4]*(data_readIn[3,11]/100)) + (data_readIn[2,4]*(data_readIn[2,11]/100))) #propensity
    data_readIn[21] <- log10(data_readIn[20]) #log propensity
    data_readIn[22] <- data_readIn[2,9]/(data_readIn[3,9] + data_readIn[2,9])
    data_readIn[23] <- log10(data_readIn[22])
    data_readIn[24] <- data_readIn[1,5]/data_readIn[1,4] #CoV
    data_readIn[25] <- i #plate
    intensity_400_data <- rbind(intensity_400_data, data_readIn)
  }
}

#reorder and rename columns
intensity_400_data <- intensity_400_data %>% select(c(25,15:24,3:14,2))
colnames(intensity_400_data)[1] <- "Plate"
colnames(intensity_400_data)[2] <- "Protein"
colnames(intensity_400_data)[3] <- "Protein Conc"
colnames(intensity_400_data)[4] <- "PEG Percent"
colnames(intensity_400_data)[5] <- "Replicate"
colnames(intensity_400_data)[6] <- "Type"
colnames(intensity_400_data)[7] <- "Propensity"
colnames(intensity_400_data)[8] <- "Log Propensity"
colnames(intensity_400_data)[9] <- "Norm Propensity"
colnames(intensity_400_data)[10] <- "Log Norm Propensity"
colnames(intensity_400_data)[11] <- "Coefficient of Variation"
colnames(intensity_400_data)[20] <- "PercentArea"

#replace N/A  and infinite values with zero
intensity_400_data[is.na(intensity_400_data)] <- 0
intensity_400_data <- intensity_400_data %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

#group metrics across replicates and compute mean values per population
intensity_400_data_formatted <- intensity_400_data %>%
  group_by(., Protein, `Protein Conc`, `PEG Percent`, Type) %>%
  summarise(., mean_Propensity = mean(Propensity), mean_Log_Propensity = log10(mean_Propensity), mean_Norm_Propensity = mean(`Norm Propensity`), mean_Log_Norm_Propensity = log10(mean_Norm_Propensity), mean_CoV = mean(`Coefficient of Variation`), MFI = mean(Mean), sdMFI = mean(StdDev)) %>%
  mutate(., `Protein Conc` = as.factor(`Protein Conc`), `PEG Percent` = as.factor(`PEG Percent`)) %>%
  ungroup()

#reorder data within columns for plotting
intensity_400_data_formatted$Protein <- factor(intensity_400_data_formatted$Protein, levels = c("WT", "G363D", "G401S"))
intensity_400_data_formatted$`Protein Conc` <- factor(intensity_400_data_formatted$`Protein Conc`, levels = c("0", "0.5", "1", "2.5", "5", "10"))
intensity_400_data_formatted$`PEG Percent` <- factor(intensity_400_data_formatted$`PEG Percent`, levels = c("0", "1", "2.5", "5", "10", "20"))

#replace infinite values with zero (introduced by coercion)
intensity_400_data_formatted <- intensity_400_data_formatted %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

#create a subset of data of Type "Field"
intensity_400_data_formatted_field <- intensity_400_data_formatted %>%
  filter(., Type == "Field")

#reorder data within columns for plotting
intensity_400_data_formatted_field$Protein <- factor(intensity_400_data_formatted_field$Protein, levels = c("WT", "G401S", "G363D"))
intensity_400_data_formatted_field$`Protein Conc` <- factor(intensity_400_data_formatted_field$`Protein Conc`, levels = c("0", "0.5", "1", "2.5", "5", "10"))
intensity_400_data_formatted_field$`PEG Percent` <- factor(intensity_400_data_formatted_field$`PEG Percent`, levels = c("0", "1", "2.5", "5", "10", "20"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(intensity_400_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = met.brewer(name = "OKeeffe2", type = "discrete", direction = 1), name = "Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_400_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Norm_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = met.brewer(name = "Cassatt1", type = "discrete", direction = -1), name = "Norm Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_400_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Norm_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.5) +
       scale_fill_gradientn(colors = c("#0f51ad", "#a1c4f0", "#fab1a1", "#fe9185"), name = "Norm Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_400_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_CoV)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = c("#0f51ad", "#a1c4f0", "#fab1a1", "#fe9185"), name = "Norm Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_400_data_formatted_field, aes(x = `Protein Conc`, y = `PEG Percent`, fill = MFI)) +
       geom_tile(colour = "black", linewidth = 0.75) +
       scale_fill_gradientn(colors = c("#3b3b3b", "#a9a9a9", "white"), name = "Field MFI") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

intensity_400_data_formatted_plots_5percent <- intensity_400_data %>%
  group_by(., Protein, `Protein Conc`, `PEG Percent`, Type) %>%
  filter(., Type == "Field", `PEG Percent` == 5) %>%
  mutate(., `Protein Conc` = as.numeric(`Protein Conc`)) %>%
  ungroup()

ggscatter(data = intensity_400_data_formatted_plots_5percent, x = "Protein Conc", y = )

plot(ggplot(intensity_400_data_formatted_field, aes(x = `Protein Conc`, y = `PEG Percent`, fill = sdMFI)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = c("#0f51ad", "#a1c4f0", "#fab1a1", "#fe9185"), name = "Field sdMFI") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_400_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Log_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = met.brewer(name = "OKeeffe2", type = "discrete", direction = 1), name = "Log Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_400_data_formatted_noG401S, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Log_Norm_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = met.brewer(name = "OKeeffe2", type = "discrete", direction = 1), name = "Log Norm Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)



intensity_100_data <- c()

#iterate through data files and read into memory all 'intensityResults_100' data (thresholded above 100 a.u.)
for (i in seq(1, length(root))) {
  intensity_100 <- file.path(root[i], "intensityResults_100")
  intensity_100_files <- c()
  
  #gather files
  intensity_100_files <- append(intensity_100_files, list.files(path = intensity_100, pattern = "^.*_results-.*\\.csv$", full.names = FALSE, recursive = FALSE))
  data_readIn <- c()
  
  setwd(intensity_100)
  
  #iterate through 'intensityResults_100' data
  for (j in intensity_100_files) {
    data_readIn <- read.csv(j)
    data_readIn <- data_readIn[c(1:3),]
    data_readIn[is.na(data_readIn)] <- 0
    
    #parse some information from data
    data_readIn[15] <- toupper(stri_extract_first_regex(j, "^\\w{2,5}(?=_)"))  #protein
    data_readIn[16] <- toupper(stri_extract_first_regex(j, "(?<=_)(\\d\\.)?\\d{1,2}(?=uM)")) #protein conc
    data_readIn[17] <- toupper(stri_extract_first_regex(j, "(?<=_)(\\d\\.)?\\d{1,2}(?=PEG)")) #PEG conc
    data_readIn[18] <- toupper(stri_extract_first_regex(j, "(?<=_)\\d(?=_)")) #tr
    
    #add new columns for various metrics
    data_readIn[1, 19] <- "Field"
    data_readIn[2, 19] <- "Object" #object/background
    data_readIn[3, 19] <- "Background" #object/background
    
    #compute metrics from other column data
    data_readIn[20] <- (data_readIn[2,4]*(data_readIn[2,11]/100))/((data_readIn[3,4]*(data_readIn[3,11]/100)) + (data_readIn[2,4]*(data_readIn[2,11]/100))) #propensity
    data_readIn[21] <- log10(data_readIn[20]) #log propensity
    data_readIn[22] <- data_readIn[2,9]/(data_readIn[3,9] + data_readIn[2,9])
    data_readIn[23] <- log10(data_readIn[22])
    data_readIn[24] <- data_readIn[1,5]/data_readIn[1,4] #CoV
    data_readIn[25] <- i #plate
    intensity_100_data <- rbind(intensity_100_data, data_readIn)
  }
}

#reorder and rename columns
intensity_100_data <- intensity_100_data %>% select(c(25,15:24,3:14,2))
colnames(intensity_100_data)[1] <- "Plate"
colnames(intensity_100_data)[2] <- "Protein"
colnames(intensity_100_data)[3] <- "Protein Conc"
colnames(intensity_100_data)[4] <- "PEG Percent"
colnames(intensity_100_data)[5] <- "Replicate"
colnames(intensity_100_data)[6] <- "Type"
colnames(intensity_100_data)[7] <- "Propensity"
colnames(intensity_100_data)[8] <- "Log Propensity"
colnames(intensity_100_data)[9] <- "Norm Propensity"
colnames(intensity_100_data)[10] <- "Log Norm Propensity"
colnames(intensity_100_data)[11] <- "Coefficient of Variation"
colnames(intensity_100_data)[20] <- "PercentArea"

#replace N/A  and infinite values with zero
intensity_100_data[is.na(intensity_100_data)] <- 0
intensity_100_data <- intensity_100_data %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

#group metrics across replicates and compute mean values per population
intensity_100_data_formatted <- intensity_100_data %>%
  group_by(., Protein, `Protein Conc`, `PEG Percent`, Type) %>%
  summarise(., mean_Propensity = mean(Propensity), mean_Log_Propensity = log10(mean_Propensity), mean_Norm_Propensity = mean(`Norm Propensity`), mean_Log_Norm_Propensity = log10(mean_Norm_Propensity), mean_CoV = mean(`Coefficient of Variation`), MFI = mean(Mean), sdMFI = mean(StdDev)) %>%
  mutate(., `Protein Conc` = as.factor(`Protein Conc`), `PEG Percent` = as.factor(`PEG Percent`)) %>%
  ungroup()

#reorder data within columns for plotting
intensity_100_data_formatted$Protein <- factor(intensity_100_data_formatted$Protein, levels = c("WT", "G401S", "G363D"))
intensity_100_data_formatted$`Protein Conc` <- factor(intensity_100_data_formatted$`Protein Conc`, levels = c("0", "0.5", "1", "2.5", "5", "10"))
intensity_100_data_formatted$`PEG Percent` <- factor(intensity_100_data_formatted$`PEG Percent`, levels = c("0", "1", "2.5", "5", "10", "20"))

#replace infinite values with zero (introduced by coercion)
intensity_100_data_formatted <- intensity_100_data_formatted %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

#create a subset of data of Type "Field"
intensity_100_data_formatted_field <- intensity_100_data_formatted %>%
  filter(., Type == "Field")

#reorder data within columns for plotting
intensity_100_data_formatted_field$Protein <- factor(intensity_100_data_formatted_field$Protein, levels = c("WT", "G401S", "G363D"))
intensity_100_data_formatted_field$`Protein Conc` <- factor(intensity_100_data_formatted_field$`Protein Conc`, levels = c("0", "0.5", "1", "2.5", "5", "10"))
intensity_100_data_formatted_field$`PEG Percent` <- factor(intensity_100_data_formatted_field$`PEG Percent`, levels = c("0", "1", "2.5", "5", "10", "20"))

#all code for generating plots for a desired metric are functionally identical, with some slight changes being made for axes labeling and other spacing parameters
#each block of code corresponding to a plot was run and saved manually, to allow for any small tweaks before final exporting
plot(ggplot(intensity_100_data_formatted_field, aes(x = `Protein Conc`, y = `PEG Percent`, fill = MFI)) +
       geom_tile(colour = "black", linewidth = 0.5) +
       scale_fill_gradientn(colors = c("#0f51ad", "#a1c4f0", "#fab1a1", "#fe9185"), name = "Field MFI") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_100_data_formatted_field, aes(x = `Protein Conc`, y = `PEG Percent`, fill = sdMFI)) +
       geom_tile(colour = "black", linewidth = 0.05) +
       scale_fill_gradientn(colors = c("#0f51ad", "#a1c4f0", "#fab1a1", "#fe9185"), name = "Field sdMFI") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)

plot(ggplot(intensity_100_data_formatted, aes(x = `Protein Conc`, y = `PEG Percent`, fill = mean_Norm_Propensity)) +
       geom_tile(colour = "black", linewidth = 0.5) +
       scale_fill_gradientn(colors = c("#0f51ad", "#a1c4f0", "#fab1a1", "#fe9185"), name = "Norm Propensity") +
       facet_wrap(~Protein, scales = "fixed") +
       labs(x = "Drp1 Conc. (uM)", y = "PEG 8000(%)") +
       theme(axis.text.x = element_text(size = 16, color = "black"),
             axis.title.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             axis.title.y = element_text(size = 16, color = "black"))
)