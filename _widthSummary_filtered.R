library(igraph)
library(tidyverse)
library(reshape2)
library(stringr)
library(formattable)
library(data.table)
library(dplyr)
library(janitor)

mainDir <- "C:/Users/kster/OneDrive - mcw.edu/1_Labwork/1_Hill Lab Data/2023/20230316_CC_Drp1_pathVariants_patientSamples_1,6-HD"
setwd(mainDir)
subDir <- "_widthSummary_filtered"
widthSummaryFolder_filtered <- file.path(mainDir, subDir)
dir.create(widthSummaryFolder_filtered, showWarnings = FALSE)  

subDir <- "_widthSummary"
widthSummaryFolder <- file.path(mainDir, subDir)

data_raw <- NULL
data_summary <- NULL

textFiles <- list.files(path = mainDir, pattern = "\\.txt$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
gnetsFiles <- list.files(path = mainDir, pattern = "\\.gnet$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

setwd(widthSummaryFolder)
data_freq <- read_csv(file = "output_frequency_L.csv")

setwd(mainDir)

toDelete <- data_freq %>%
	filter(percent > 0.0005) %>%
	select("Summary_output$Length")
toDelete <- as.data.frame(toDelete)

for (i in textFiles) {
	name_noExt = sub(".txt", "", i)
	data_readIn <- read.table(i, sep="\t", skip=1, col.names=c('line_id', 'point_id', 'x', 'y', 'z', 'width_um', 'pixel_intensity'))

	data_readIn <- data_readIn %>%
		group_by(line_id) %>%
		summarise(Width_um_avg = mean(width_um))

	files_to_import <- paste(c(name_noExt, ".gnet"))
	files_to_import <- as.data.frame(files_to_import)
	files_to_import <- transpose(files_to_import)
	fileName <- unite(files_to_import, file, c(V1, V2), sep = "")

	df_list <- lapply(fileName, 
		function(j) {read.table(j, sep = "\t", skip = 1, col.names = c('Source','Target','Length'))})

	data_raw_bind <- bind_rows(df_list)
	data_raw_merge <- cbind(data_raw_bind, data_readIn)

	G <- graph.data.frame(as.data.frame(data_raw_merge, directed = F))

	connected_comps <- decompose(G)
	V(G)$Noise <- FALSE
	length_threshold <- 50

	for (j in connected_comps) {
    	if (ecount(j)==1 && vcount(j)==2 && sum(E(j)$Length) > length_threshold) {
      		for (n in V(j)$name) {
        		V(G)$Noise[which(V(G)$name==n)] <- TRUE
      		}
    	}
  	}

  	G <- delete.vertices(G,V(G)$Noise==TRUE)

  	connected_comps <- decompose(G)
  	V(G)$Noise <- FALSE
  	for (j in 1:nrow(toDelete)) {
    	for (k in connected_comps) {
      		if (ecount(k)==1 && vcount(k)==2 && sum(E(k)$Length) == toDelete[j,1]) {
        		for (n in V(k)$name) {
          			V(G)$Noise[which(V(G)$name==n)] <- TRUE
        		}
      		}
    	}
  	}

  	G <- delete.vertices(G,V(G)$Noise==TRUE)

  	data_raw_decomp_width <- NULL
	data_raw_decomp_length <- NULL
	tempList <- decompose(G)
	for (j in tempList) {
		data_raw_decomp_width <- rbind(data_raw_decomp_width, data.frame(fileName, vcount(j), ecount(j), mean(E(j)$Width_um_avg)))
		data_raw_decomp_length <- rbind(data_raw_decomp_length, data.frame(fileName, vcount(j), ecount(j), mean(E(j)$Length)))
	}

	data_raw_combine <- cbind(data_raw_decomp_width, data_raw_decomp_length)
	data_raw_combine <- data_raw_combine[, !duplicated(colnames(data_raw_combine))]

	colnames(data_raw_combine) <- c('FileName', 'Nodes', 'Edges','Width', 'Length')

	data_raw_combine <- data_raw_combine[order(data_raw_combine$Length, decreasing = T),]

	#data_raw_combine_filter <- data_raw_combine %>%
	#	filter(., Width < 0.22 && Length > 100)

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

  	Pk <- degree.distribution(G)
  	FreeEnds = ifelse(is.na(Pk[2]), 0, Pk[2])
  	ThreeWayJunct = ifelse(is.na(Pk[4]), 0, Pk[4])
  	FourWayJunct = ifelse(is.na(Pk[5]), 0, Pk[5])

  	AvgDegree = (FreeEnds * 1) + (ThreeWayJunct * 3) + (FourWayJunct * 4)

  	MitoGraphCS = (PHI + AvgEdgeLength + AvgDegree) / (TotalNodeNorm + TotalEdgeNorm + TotalCCNorm)

  	data_summary_names = data.frame(
    	"File_Name" = fileName,
    	"Total_Nodes" = TotalNodes,
    	"Total_Edges" = TotalEdges,
    	"Total_Length_um"= TotalLength,
    	"Total_Connected_Components" = ConnectedComponents,
    	"PHI" = PHI,
    	"Avg_Edge_Length_um" = AvgEdgeLength,
    	"Total_Edge_Norm_to_Length_um" = TotalEdgeNorm,
    	"Total_Node_Norm_to_Length_um" = TotalNodeNorm,
    	"Total_Connected_Components_Norm_to_Length_um" = TotalCCNorm,
    	"Free_Ends" = FreeEnds,
    	"three_way_junction" = ThreeWayJunct,
    	"four_way_junction" = FourWayJunct,
    	"Avg_Degree" = AvgDegree,
    	"MitoGraph_Connectivity_Score" = MitoGraphCS,
    	"Average_width_um" = MeanWidth
 	)

 	data_raw <- rbind(data_raw, data_raw_combine)
 	data_summary <- rbind(data_summary, data_summary_names)
}

output <- paste(widthSummaryFolder_filtered, "/output_filtered.csv", sep = "")
outputSummary <- paste(widthSummaryFolder_filtered, "/outputSummary_filtered.csv", sep = "")

write.csv(data_raw, file = output, row.names = FALSE)
write.csv(data_summary, file = outputSummary, row.names = FALSE)

setwd(widthSummaryFolder_filtered)

Summary_output <- read_csv("output_filtered.csv") 


Summary_output %>%
  ggplot(aes(x = Width, color = Length)) +
  geom_histogram(binwidth = 0.001) +
  #stat_bin (binwidth=.001, geom='text', aes(label=..count..), position=position_stack(vjust = 0.5)) +
  #geom_density(alpha=0.6) +
  #geom_vline(aes(xintercept=mean(Nodes), color=strain), linetype="dashed", size=1) +
  #xlim(0, 1.5) +
  #ylim(0, .001) +
  #scale_x_log10() +
  #scale_y_log10() +
  #facet_grid(strain ~ .) +
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

ggsave(paste("Histogram_W_filtered.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

Summary_output %>%
  ggplot(aes(x = Length, color = Width)) +
  geom_histogram(binwidth = .001) +
  #stat_bin (binwidth=.001, geom='text', aes(label=..count..), position=position_stack(vjust = 0.5)) +
  #geom_density(alpha=0.6) +
  #geom_vline(aes(xintercept=mean(Nodes), color=strain), linetype="dashed", size=1) +
  xlim(0, 1.5) +
  #ylim(0, .001) +
  #scale_x_log10() +
  #scale_y_log10() +
  #facet_grid(strain ~ .) +
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

ggsave(paste("Histogram_L_filtered.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

Summary_output%>%
  ggplot(aes(x = Length, y = Width, color = Nodes)) +
  geom_point(size=1) +
  #xlim(0,1.1) +
  #ylim(0, .001) +
  #scale_x_log10() +
  #scale_y_log10() +
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
ggsave(paste("WidthVlength_filtered.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

table_L <- tabyl(Summary_output$Length, sort = TRUE)
table_W <- tabyl(Summary_output$Width, sort = TRUE)

write_csv(table_L, "output_frequency_L_filtered.csv")
write_csv(table_W, "output_frequency_W_filtered.csv")