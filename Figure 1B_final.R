# Heat Map Plot Creation
## Started by JG
## 07-24-18

rm(list=ls())

library(RColorBrewer)
library(Rtsne)
library(ggplot2)
library(splitstackshape)

setwd("~/Your working directory")

load("~Figure1a_subsampled.Rda")
Heat_Map_All<-Subsampled

# Color by protein values for each marker, LAG-3 is shown as an example


LAG_3<-ggplot(Heat_Map_All, aes(x=V1, y=V2, color=`LAG-3`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of LAG-3 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "LAG-3")
LAG_3
LAG_3+scale_color_gradient(low="blue", high="red")
