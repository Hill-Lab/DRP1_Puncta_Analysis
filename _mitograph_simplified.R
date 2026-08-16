library(igraph)
library(tidyverse)
library(reshape2)
library(stringr)
library(stringi)
library(formattable)
library(data.table)
library(dplyr)
library(janitor)
library(r2r)

labelHash <- hashmap()

#number of items must match, RENAME
colID <- c(2,3,4,5,6,7,8,9,10,11)
colLabels <- c("Untreated Control #1", "2.5% 1,6-HD (5 minutes)", "3.5% 1,6-HD (5 minutes)", "5.0% 1,6-HD (5 minutes)", "Untreated Control #2", "2.5% 1,6-HD (20 minutes)", "3.5% 1,6-HD (20 minutes)", "5.0% 1,6-HD (20 minutes)", "Untreated Control #3", "Untreated Control #4")
labelHash[colID] <- colLabels

#number of items must match, RENAME
rowID <- c('B','C','D','E','F')
rowLabels <- c("Control Adult", "Control Youth", "P1 (G401S)", "P2 (G363D)", "P3 (L230dup)")
labelHash[rowID] <- rowLabels

mainDir <- "E:/20230308_JOBS/MitoGraph_output"
subDir <- "_widthSummary"
widthSummaryFolder <- file.path(mainDir, subDir)
setwd(widthSummaryFolder)

data_raw <- read_csv(file = "outputSummary.csv")

data_formatted <- cbind(WellIdentifier = toupper(stri_replace_all_regex(stri_extract_first_regex(data_raw[[1]], "/r?[:alpha:][:digit:]{1,2}[-\\.]"), "[-/r\\.]", "")), data_raw)

#RENAME
data_formatted$CellLine <- "Human Fibroblast"
data_formatted$Condition <- ""
data_formatted$Treatment <- ""
data_formatted <- data_formatted[c(1, 18:20, 2:17)]

for (i in seq(1, length(data_formatted$WellIdentifier))) {
  irow <- stri_extract_first_regex(data_formatted[i, 1], "[:alpha:]")
  icol <- stri_extract_first_regex(data_formatted[i, 1], "[:digit:]{1,2}")
  data_formatted[i, 3] <- labelHash[irow]
  data_formatted[i, 4] <- labelHash[as.numeric(icol)]
}

PlotsToBeMade <- c("Condition",
                   "Treatment",
                   "PHI",
                   "Avg_Edge_Length_um",
                   "Total_Node_Norm_to_Length_um",
                   "Total_Connected_Components_Norm_to_Length_um",
                   "Free_Ends",
                   "three_way_junction",
                   "four_way_junction",
                   "Avg_Degree",
                   "MitoGraph_Connectivity_Score",
                   "Average_width_um")

AxisLabels <- c("Condition",
                "Treatment",
                "PHI",
                "Avg Edge Length um",
                "Total Node Norm to Length um",
                "Total Connected Components Norm to Length um",
                "Free Ends",
                "three way junction",
                "four way junction",
                "Avg Degree",
                "MitoGraph Connectivity Score",
                "Average width um")

Titles <- c("Condition",
            "Treatment",
            "PHI",
            "Avg Edge Length um",
            "Total Node Norm to Length um",
            "Total Connected Components Norm to Length um",
            "Free Ends",
            "Three way junction",
            "Four way junction",
            "Avg Degree",
            "MitoGraph Connectivity Score",
            "Average width um")

# yAxisMinimum <- c(0,
#                   0,
#                   0,
#                   0,
#                   0,
#                   0,
#                   0,
#                   0,
#                   0,
#                   0,
#                   0,
#                   0)
# 
# yAxisMaximum <- c(1,
#                   1,
#                   150,
#                  150,
#                  150,
#                  150,
#                  150,
#                  1.5,
#                  10000,
#                  20000,
#                  10000,
#                  20000)

for (i in seq(3, length(PlotsToBeMade))) {
	yaxis <- PlotsToBeMade[i]
 	xaxis <- PlotsToBeMade[2]
 	fill <- PlotsToBeMade[1]

	plot(ggplot(data=data_formatted,aes_string(x=xaxis, y=yaxis, fill=fill)) + 
        geom_boxplot(outlier.size = 0, colour = "grey10", position = position_dodge (width = 1), size = 0.25) +
        stat_boxplot(geom="errorbar", position = position_dodge (width = 1), width = 0.5, size = 0.25) +
        #scale_fill_manual(values = c("grey", "green", "magenta")) +
        geom_point(data=data_formatted,aes_string(colour = fill, x=xaxis, y=yaxis), pch=20, size=0.01, position=position_jitterdodge(jitter.width=.3, jitter.height=0, dodge.width=1)) +
        #scale_colour_manual(values=c("grey50", "grey50","grey50")) +
        labs(title = Titles[i],
            x = xaxis,
            y = AxisLabels[i]) +
        #ylab(AxisLabels[p]) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 6, color = "black"),
            axis.title.x = element_text(size = 6, color = "black"),
            axis.text.y = element_text(size = 6, color = "black"),
            axis.title.y = element_text(size = 6, color = "black"),
        	plot.title = element_text(size = 8, color = "black", hjust = 0.5, face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line.x = element_line(color = "grey75", size = 0.5, linetype = 1), 
            axis.line.y = element_line(color = "grey75", size = 0.5, linetype = 1), 
            #legend.position = "none"
            legend.title = element_blank(),
            legend.justification = c(0, 1), 
            legend.position = "right",
            legend.text = element_text(size = 6, color = "black")
            ) 
       # Add a "+" above and uncomment the below 2 lines to add custom axis scales. 
       #ylim(yAxisMinimum[p], yAxisMaximum[p])
  )  
  ggsave(paste(widthSummaryFolder, "/Plot-", yaxis, ".eps", sep = ""), width = 40, height = 30, units = "cm", dpi = 300)
}