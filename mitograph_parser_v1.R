library(igraph)
library(tidyverse)
library(reshape2)
library(stringr)
library(stringi)
library(formattable)
library(data.table)
library(dplyr)
library(r2r)
library(janitor)
library(doParallel)
library(usethis)

#seems to help with processing massive amounts of data
cores <- detectCores()
registerDoParallel(cores = cores)

#user define working root directory
root <<- tcltk::tk_choose.dir("Select a MitoGraph_output directory")
setwd(root)

#create directory for "_widthSummary_raw" output
ws_raw <- "_widthSummary_raw"
widthSummaryFolder_raw <- file.path(root, ws_raw)
dir.create(widthSummaryFolder_raw, showWarnings = FALSE)

#create directory for "_widthSummary_filtered" output
ws_filtered <- "_widthSummary_filtered"
widthSummaryFolder_filtered <- file.path(root, ws_filtered)
dir.create(widthSummaryFolder_filtered, showWarnings = FALSE)  

#initialize variables needed to parse data
data_raw <- NULL
data_summary <- NULL

#gather data files and gather raw metrics
textFiles <- list.files(path = root, pattern = "\\.txt$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

#iterate through data files
for (i in textFiles) {
  
  #read in text files and establish column names
  name_noExt = sub(".txt", "", i)
  data_readIn <- read.table(i, sep="\t", skip=1, col.names=c('line_id', 'point_id', 'x', 'y', 'z', 'width_um', 'pixel_intensity'))
  
  #compute an average metric from data
  data_readIn <- data_readIn %>%
    group_by(line_id) %>%
    summarise(Width_um_avg = mean(width_um))
  
  #read in mitograph files and establish column names
  name_mitoExt <- paste0(name_noExt, ".mitograph")
  data_readIn_mitoExt <- read.table(name_mitoExt, sep = "\t", skip = 1, col.names = c("volume_from_voxels_um3", "average_width_raw_um", "std_width_raw_um", "total_length_raw_um", "volume_from_length_um3"))
  
  #read in gnet files and restructure as a data frame
  files_to_import <- paste(c(name_noExt, ".gnet"))
  files_to_import <- as.data.frame(files_to_import)
  files_to_import <- transpose(files_to_import)
  fileName <- unite(files_to_import, file, c(V1, V2), sep = "")
  df_list <- lapply(fileName, 
                    function(j) {read.table(j, sep = "\t", skip = 1, col.names = c('Source','Target','Length'))})
  
  #create a merged data frame of combined metrics parsed above
  data_raw_bind <- bind_rows(df_list)
  data_raw_merge <- cbind(data_raw_bind, data_readIn)
  data_raw_merge <- cbind(data_raw_merge, data_readIn_mitoExt)
  
  #create a graphical network of merged data
  graph_merge <- graph.data.frame(as.data.frame(data_raw_merge, directed = F))
  
  #decompose graphical network into raw components to gather metrics
  data_raw_decomp_width <- NULL
  data_raw_decomp_length <- NULL
  tempList <- decompose(graph_merge)
  for (j in tempList) {
    data_raw_decomp_width <- rbind(data_raw_decomp_width, data.frame(fileName, vcount(j), ecount(j), mean(E(j)$Width_um_avg)))
    data_raw_decomp_length <- rbind(data_raw_decomp_length, data.frame(fileName, vcount(j), ecount(j), mean(E(j)$Length)))
  }
  
  #add graphical network metrics to a standard data frame, purging duplicates
  data_raw_combine <- cbind(data_raw_decomp_width, data_raw_decomp_length)
  data_raw_combine <- data_raw_combine[, !duplicated(colnames(data_raw_combine))]
  
  #rename columns and sort data frame by decreasing length
  colnames(data_raw_combine) <- c('FileName', 'Nodes', 'Edges', 'Width', 'Length')
  data_raw_combine <- data_raw_combine[order(data_raw_combine$Length, decreasing = T),]

  #extract and compute metrics from graphical network
  TotalNodes <- vcount(graph_merge)
  TotalEdges <- ecount(graph_merge)
  TotalLength <- sum(E(graph_merge)$Length)
  MeanWidth <- mean(E(graph_merge)$Width_um_avg)
  ConnectedComponents = length(tempList)
  PHI = max(data_raw_combine$Length) / TotalLength
  AvgEdgeLength = TotalLength / TotalEdges
  TotalEdgeNorm = TotalEdges / TotalLength  	
  TotalNodeNorm = TotalNodes / TotalLength
  TotalCCNorm = ConnectedComponents / TotalLength
  AvgCCLength = TotalLength / ConnectedComponents
  Pk <- degree.distribution(graph_merge)
  FreeEnds = ifelse(is.na(Pk[2]), 0, Pk[2])
  ThreeWayJunct = ifelse(is.na(Pk[4]), 0, Pk[4])
  FourWayJunct = ifelse(is.na(Pk[5]), 0, Pk[5])
  AvgDegree = (FreeEnds * 1) + (ThreeWayJunct * 3) + (FourWayJunct * 4)
  MitoGraphCS = (PHI + AvgEdgeLength + AvgDegree) / (TotalNodeNorm + TotalEdgeNorm + TotalCCNorm)
  EF = TotalEdges / ConnectedComponents
  EFC = (TotalEdges / ConnectedComponents) * (TotalEdges / TotalNodes)
  VoxVol <- mean(E(graph_merge)$volume_from_voxels_um3)
  LengthVol <- mean(E(graph_merge)$volume_from_length_um3)
  VolOcc = LengthVol / VoxVol
  
  #establish names for extracted data
  data_summary_names = data.frame(
    "File_Name" = fileName,
    "Total_Nodes" = TotalNodes,
    "Total_Edges" = TotalEdges,
    "Total_Length_um"= TotalLength,
    "Average_width_um" = MeanWidth,
    "Total_Connected_Components" = ConnectedComponents,
    "PHI" = PHI,
    "Avg_Edge_Length_um" = AvgEdgeLength,
    "Total_Edge_Norm_to_Length_um" = TotalEdgeNorm,
    "Total_Node_Norm_to_Length_um" = TotalNodeNorm,
    "Total_Connected_Components_Norm_to_Length_um" = TotalCCNorm,
    "Avg_Connected_Component_Length_um" = AvgCCLength,
    "Free_Ends" = FreeEnds,
    "three_way_junction" = ThreeWayJunct,
    "four_way_junction" = FourWayJunct,
    "Avg_Degree" = AvgDegree,
    "MitoGraph_Connectivity_Score" = MitoGraphCS,
    "Elongation_Factor" = EF,
    "Elongation_Factor_corrected" = EFC,
    "Total_Voxel_Volume_um3" = VoxVol,
    "Total_Mito_Volume_um3" = LengthVol,
    "Mito_Volume_Occupancy" = VolOcc
  )
  
  #combine decomposed, extracted, and computed metrics into a single data frame with updated names
  data_raw <- rbind(data_raw, data_raw_combine)
  data_summary <- rbind(data_summary, data_summary_names)
}

#create .csv files and save data to them
output <- paste(widthSummaryFolder_raw, "/output.csv", sep = "")
outputSummary <- paste(widthSummaryFolder_raw, "/outputSummary.csv", sep = "")
write.csv(data_raw, file = output, row.names = FALSE)
write.csv(data_summary, file = outputSummary, row.names = FALSE)

#move directories and open the "output.csv" created above
setwd(widthSummaryFolder_raw)
Summary_output <- read_csv("output.csv") 

#plot counts of "Width Connected Component" across all mitochondria ("edges") for visual inspection 
Summary_output %>%
  ggplot(aes(x = Width, color = Length)) +
  geom_histogram(binwidth = 0.001) +
  labs(title = "Histogram Analysis of GNETs",
       x = "Width Connected Component",
       y = "Count") +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey75", size = 0.5, linetype = 1), 
        axis.line.y = element_line(color = "grey75", size = 0.5, linetype = 1), 
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = "right",
        legend.text = element_text(size = 12, color = "black")
  )

#save resulting width plot
ggsave(paste("Histogram_W.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

#plot counts of "Length Connected Component" across all mitochondria ("edges") for visual inspection 
Summary_output %>%
  ggplot(aes(x = Length, color = Width)) +
  geom_histogram(binwidth = .001) +
  xlim(0, 1.5) +
  labs(title = "Histogram Analysis of GNETs",
       x = "Length Connected Component",
       y = "Count") +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey75", size = 0.5, linetype = 1), 
        axis.line.y = element_line(color = "grey75", size = 0.5, linetype = 1), 
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = "right",
        legend.text = element_text(size = 12, color = "black")
  )

#save resulting length plot
ggsave(paste("Histogram_L.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

#plot "Width Connected Component" versus "Length Connected Component" across all mitochondria ("edges") for visual inspection 
Summary_output%>%
  ggplot(aes(x = Length, y = Width, color = Nodes)) +
  geom_point(size=1) +
  labs(title = "Analysis of GNETs",
       x = "Length Connected Component",
       y = "Width") +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey75", size = 0.5, linetype = 1), 
        axis.line.y = element_line(color = "grey75", size = 0.5, linetype = 1), 
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = "right",
        legend.text = element_text(size = 12, color = "black")
  )

#save resulting plot
ggsave(paste("WidthVlength.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

#create .csv files for width and length components independently
table_L <- tabyl(Summary_output$Length, sort = TRUE)
table_W <- tabyl(Summary_output$Width, sort = TRUE)
write_csv(table_L, "output_frequency_L.csv")
write_csv(table_W, "output_frequency_W.csv")



#read length component file
setwd(widthSummaryFolder_raw)
data_freq <- read_csv(file = "output_frequency_L.csv")

setwd(root)

#create a copy of length component file, selecting occurrences of length that appear at a frequency greater than 0.0005 (filtering out errors/noise)
toDelete <- data_freq %>%
  filter(percent > 0.0005) %>%
  select("Summary_output$Length")
toDelete <- as.data.frame(toDelete)

#initialize variables needed to parse data
data_readIn <- NULL
files_to_import <- NULL
df_list <- NULL
data_raw <- NULL
data_raw_bind <- NULL
data_raw_merge <- NULL
data_raw_combine <- NULL
data_summary <- NULL

#gather data files and gather metrics for the filtered data set
for (i in textFiles) {
  
  #read in text files and establish column names
  name_noExt = sub(".txt", "", i)
  data_readIn <- read.table(i, sep="\t", skip=1, col.names=c('line_id', 'point_id', 'x', 'y', 'z', 'width_um', 'pixel_intensity'))
  
  #compute an average metric from data
  data_readIn <- data_readIn %>%
    group_by(line_id) %>%
    summarise(Width_um_avg = mean(width_um))
  
  #read in mitograph files and establish column names
  name_mitoExt <- paste0(name_noExt, ".mitograph")
  data_readIn_mitoExt <- read.table(name_mitoExt, sep = "\t", skip = 1, col.names = c("volume_from_voxels_um3", "average_width_raw_um", "std_width_raw_um", "total_length_raw_um", "volume_from_length_um3"))
  
  #read in gnet files and restructure as a data frame
  files_to_import <- paste(c(name_noExt, ".gnet"))
  files_to_import <- as.data.frame(files_to_import)
  files_to_import <- transpose(files_to_import)
  fileName <- unite(files_to_import, file, c(V1, V2), sep = "")
    df_list <- lapply(fileName, 
                    function(j) {read.table(j, sep = "\t", skip = 1, col.names = c('Source','Target','Length'))})
  
  #create a merged data frame of combined metrics parsed above
  data_raw_bind <- bind_rows(df_list)
  data_raw_merge <- cbind(data_raw_bind, data_readIn)
  data_raw_merge <- cbind(data_raw_merge, data_readIn_mitoExt)
  
  #create a graphical network of merged data
  G <- graph.data.frame(as.data.frame(data_raw_merge, directed = F))
  
  #decompose graphical network, additionally specifying noise and threshold parameters
  connected_comps <- decompose(G)
  V(G)$Noise <- FALSE
  length_threshold <- 50
  
  #iterate through vertices and mark data to be deleted if larger than "length_threshold"
  for (j in connected_comps) {
    if (ecount(j)==1 && vcount(j)==2 && sum(E(j)$Length) > length_threshold) {
      for (n in V(j)$name) {
        V(G)$Noise[which(V(G)$name==n)] <- TRUE
      }
    }
  }
  
  #delete vertices specified above
  G <- delete.vertices(G,V(G)$Noise==TRUE)
  
  #decompose updated graphical network into raw components to gather metrics, resetting the noise parameter
  connected_comps <- decompose(G)
  V(G)$Noise <- FALSE
  
  #iterate through vertices and mark data to be deleted from frequency data in "toDelete"
  for (j in 1:nrow(toDelete)) {
    for (k in connected_comps) {
      if (ecount(k)==1 && vcount(k)==2 && sum(E(k)$Length) == toDelete[j,1]) {
        for (n in V(k)$name) {
          V(G)$Noise[which(V(G)$name==n)] <- TRUE
        }
      }
    }
  }
  
  #delete vertices specified above
  G <- delete.vertices(G,V(G)$Noise==TRUE)
  
  #decompose graphical network into raw components to gather metrics
  data_raw_decomp_width <- NULL
  data_raw_decomp_length <- NULL
  tempList <- decompose(G)
  for (j in tempList) {
    data_raw_decomp_width <- rbind(data_raw_decomp_width, data.frame(fileName, vcount(j), ecount(j), mean(E(j)$Width_um_avg)))
    data_raw_decomp_length <- rbind(data_raw_decomp_length, data.frame(fileName, vcount(j), ecount(j), mean(E(j)$Length)))
  }
  
  #add graphical network metrics to a standard data frame, purging duplicates
  data_raw_combine <- cbind(data_raw_decomp_width, data_raw_decomp_length)
  data_raw_combine <- data_raw_combine[, !duplicated(colnames(data_raw_combine))]
  
  #rename columns and sort data frame by decreasing length
  colnames(data_raw_combine) <- c('FileName', 'Nodes', 'Edges','Width', 'Length')
  data_raw_combine <- data_raw_combine[order(data_raw_combine$Length, decreasing = T),]

  #extract and compute metrics from graphical network
  TotalNodes <- vcount(G)
  TotalEdges <- ecount(G)
  TotalLength <- sum(E(G)$Length)
  MeanWidth <- mean(E(G)$Width_um_avg)
  ConnectedComponents = length(tempList)
  PHI = max(data_raw_combine$Length) / TotalLength
  AvgEdgeLength = TotalLength / TotalEdges
  TotalEdgeNorm = TotalEdges / TotalLength  	
  TotalNodeNorm = TotalNodes / TotalLength
  TotalCCNorm = ConnectedComponents / TotalLength
  AvgCCLength = TotalLength / ConnectedComponents
  Pk <- degree.distribution(G)
  FreeEnds = ifelse(is.na(Pk[2]), 0, Pk[2])
  ThreeWayJunct = ifelse(is.na(Pk[4]), 0, Pk[4])
  FourWayJunct = ifelse(is.na(Pk[5]), 0, Pk[5])
  AvgDegree = (FreeEnds * 1) + (ThreeWayJunct * 3) + (FourWayJunct * 4)
  MitoGraphCS = (PHI + AvgEdgeLength + AvgDegree) / (TotalNodeNorm + TotalEdgeNorm + TotalCCNorm)
  EF = TotalEdges / ConnectedComponents
  EFC = (TotalEdges / ConnectedComponents) * (TotalEdges / TotalNodes)
  VoxVol <- mean(E(G)$volume_from_voxels_um3)
  LengthVol <- mean(E(G)$volume_from_length_um3)
  VolOcc = LengthVol / VoxVol
  
  #establish names for extracted data
  data_summary_names = data.frame(
    "File_Name" = fileName,
    "Total_Nodes" = TotalNodes,
    "Total_Edges" = TotalEdges,
    "Total_Length_um"= TotalLength,
    "Average_width_um" = MeanWidth,
    "Total_Connected_Components" = ConnectedComponents,
    "PHI" = PHI,
    "Avg_Edge_Length_um" = AvgEdgeLength,
    "Total_Edge_Norm_to_Length_um" = TotalEdgeNorm,
    "Total_Node_Norm_to_Length_um" = TotalNodeNorm,
    "Total_Connected_Components_Norm_to_Length_um" = TotalCCNorm,
    "Avg_Connected_Component_Length_um" = AvgCCLength,
    "Free_Ends" = FreeEnds,
    "three_way_junction" = ThreeWayJunct,
    "four_way_junction" = FourWayJunct,
    "Avg_Degree" = AvgDegree,
    "MitoGraph_Connectivity_Score" = MitoGraphCS,
    "Elongation_Factor" = EF,
    "Elongation_Factor_corrected" = EFC,
    "Total_Voxel_Volume_um3" = VoxVol,
    "Total_Mito_Volume_um3" = LengthVol,
    "Mito_Volume_Occupancy" = VolOcc
  )
  
  #combine decomposed, extracted, and computed metrics into a single data frame with updated names
  data_raw <- rbind(data_raw, data_raw_combine)
  data_summary <- rbind(data_summary, data_summary_names)
}



#setup hash maps for indexing plate well information
labelHash <- hashmap()

#define plate column information; make sure NUMBER of entries in "colID" and "colLabels" are IDENTICAL
colID <- c(1,2,3,4,5,6)
colLabels <- c("Untreated Control #1", "Untreated Control #2", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)")
labelHash[colID] <- colLabels

#define plate row information; make sure NUMBER of entries in "rowID" and "rowLabels" are IDENTICAL
rowID <- c('A','B','C','D')
rowLabels <- c("Control Youth", "Control Adult", "P1 (G401S)", "P2 (G363D)")
labelHash[rowID] <- rowLabels

#add a column for plate replicate
plateNum <- 3      #UPDATE WITH REPLICATE PLATE NUMBER MANUALLY

#extract identifying information from "data_summary"
data_summary_formatted <- cbind(WellIdentifier = toupper(stri_replace_all_regex(stri_extract_first_regex(data_summary[[1]], "/r?[:alpha:][:digit:]{1,2}[-\\.]"), "[-/r\\.]", "")), data_summary)

#add additional columns for "CellLine", "Condition", and "Treatment"
data_summary_formatted$CellLine <- "Human Fibroblast"
data_summary_formatted$Condition <- ""
data_summary_formatted$Treatment <- ""

#extract more identifying information from "data_summary"
data_summary_formatted <- cbind(ImageNumber = toupper(stri_extract_first_regex(data_summary_formatted[[2]], "(?<=-)\\d{3}(?=_)")), data_summary_formatted)
data_summary_formatted <- cbind(ROINumber = toupper(stri_extract_first_regex(data_summary_formatted[[3]], "(?<=_)[:digit:]{1,2}(?=.gnet)")), data_summary_formatted)

#add a "Plate" column corresponding to "plateNum"
data_summary_formatted$Plate <- plateNum

#reorder columns
data_summary_formatted <- data_summary_formatted[c(29, 26:28, 3, 2, 1, 5:25, 4)]

#assign "Condition" and "Treatment" information based on hash map indexing
for (i in seq(1, length(data_summary_formatted$WellIdentifier))) {
  irow <- stri_extract_first_regex(data_summary_formatted[i, 5], "[:alpha:]")
  icol <- stri_extract_first_regex(data_summary_formatted[i, 5], "[:digit:]{1,2}")
  data_summary_formatted[i, 3] <- labelHash[irow]
  data_summary_formatted[i, 4] <- labelHash[as.numeric(icol)]
}

#create .csv files for width and length components independently
output <- paste(widthSummaryFolder_filtered, "/output_filtered.csv", sep = "")
outputSummary <- paste(widthSummaryFolder_filtered, "/outputSummary_filtered.csv", sep = "")    #THIS IS THE MAIN DATA OUTPUT USED IN CONTINUED PROCESSING/ANALYSES
write.csv(data_raw, file = output, row.names = FALSE)
write.csv(data_summary_formatted, file = outputSummary, row.names = FALSE)

#read filtered length component file
setwd(widthSummaryFolder_filtered)
Summary_output <- read_csv("output_filtered.csv") 

#plot counts of "Width Connected Component" across all mitochondria ("edges") for visual inspection 
Summary_output %>%
  ggplot(aes(x = Width, color = Length)) +
  geom_histogram(binwidth = 0.001) +
  labs(title = "Histogram Analysis of GNETs",
       x = "Width Connected Component",
       y = "Count") +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey75", size = 0.5, linetype = 1), 
        axis.line.y = element_line(color = "grey75", size = 0.5, linetype = 1), 
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = "right",
        legend.text = element_text(size = 12, color = "black")
  )

#save resulting width plot
ggsave(paste("Histogram_W_filtered.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

#plot counts of "Length Connected Component" across all mitochondria ("edges") for visual inspection 
Summary_output %>%
  ggplot(aes(x = Length, color = Width)) +
  geom_histogram(binwidth = .001) +
  xlim(0, 1.5) +
  labs(title = "Histogram Analysis of GNETs",
       x = "Length Connected Component",
       y = "Count") +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey75", size = 0.5, linetype = 1), 
        axis.line.y = element_line(color = "grey75", size = 0.5, linetype = 1), 
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = "right",
        legend.text = element_text(size = 12, color = "black")
  )

#save resulting length plot
ggsave(paste("Histogram_L_filtered.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

#plot "Width Connected Component" versus "Length Connected Component" across all mitochondria ("edges") for visual inspection 
Summary_output%>%
  ggplot(aes(x = Length, y = Width, color = Nodes)) +
  geom_point(size=1) +
  labs(title = "Analysis of GNETs",
       x = "Length Connected Component",
       y = "Width") +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey75", size = 0.5, linetype = 1), 
        axis.line.y = element_line(color = "grey75", size = 0.5, linetype = 1), 
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = "right",
        legend.text = element_text(size = 12, color = "black")
  )

#save resulting plot
ggsave(paste("WidthVlength_filtered.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

#create .csv files for width and length components independently
table_L <- tabyl(Summary_output$Length, sort = TRUE)
table_W <- tabyl(Summary_output$Width, sort = TRUE)
write_csv(table_L, "output_frequency_L_filtered.csv")
write_csv(table_W, "output_frequency_W_filtered.csv")