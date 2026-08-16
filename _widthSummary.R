library(igraph)
library(tidyverse)
library(reshape2)
library(stringr)
library(formattable)
library(data.table)
library(dplyr)
library(janitor)

mainDir <- "E:/20230308_JOBS/MitoGraph_output"
setwd(mainDir)
subDir <- "_widthSummary"
widthSummaryFolder <- file.path(mainDir, subDir)
dir.create(widthSummaryFolder, showWarnings = FALSE)  

data_raw <- NULL
data_summary <- NULL

textFiles <- list.files(path = mainDir, pattern = "\\.txt$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
gnetsFiles <- list.files(path = mainDir, pattern = "\\.gnet$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

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

	graph_merge <- graph.data.frame(as.data.frame(data_raw_merge, directed = F))

	data_raw_decomp_width <- NULL
	data_raw_decomp_length <- NULL
	tempList <- decompose(graph_merge)
	for (j in tempList) {
		data_raw_decomp_width <- rbind(data_raw_decomp_width, data.frame(fileName, vcount(j), ecount(j), mean(E(j)$Width_um_avg)))
		data_raw_decomp_length <- rbind(data_raw_decomp_length, data.frame(fileName, vcount(j), ecount(j), mean(E(j)$Length)))
	}

	data_raw_combine <- cbind(data_raw_decomp_width, data_raw_decomp_length)
	data_raw_combine <- data_raw_combine[, !duplicated(colnames(data_raw_combine))]

	colnames(data_raw_combine) <- c('FileName', 'Nodes', 'Edges','Width', 'Length')

	data_raw_combine <- data_raw_combine[order(data_raw_combine$Length, decreasing = T),]

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

  	Pk <- degree.distribution(graph_merge)
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

output <- paste(widthSummaryFolder, "/output.csv", sep = "")
outputSummary <- paste(widthSummaryFolder, "/outputSummary.csv", sep = "")

write.csv(data_raw, file = output, row.names = FALSE)
write.csv(data_summary, file = outputSummary, row.names = FALSE)

setwd(widthSummaryFolder)

Summary_output <- read_csv("output.csv") 

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

ggsave(paste("Histogram_W.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

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

ggsave(paste("Histogram_L.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

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
ggsave(paste("WidthVlength.png",sep=""), width = 25, height = 16, units = "cm", dpi = 300)

table_L <- tabyl(Summary_output$Length, sort = TRUE)
table_W <- tabyl(Summary_output$Width, sort = TRUE)

write_csv(table_L, "output_frequency_L.csv")
write_csv(table_W, "output_frequency_W.csv")