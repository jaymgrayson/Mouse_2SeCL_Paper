## Figure 3B Grayson et al
## Jason M. Grayson
## Started on 08-20-2018

rm(list=ls())

library(RColorBrewer)
library(Rtsne)
library(ggplot2)
library(splitstackshape)

setwd("~/Desktop/GVHD analyses/McIver Mouse Flow/combined viable")
load(file="Fig3A_data.Rda")

Heat_Map_All<-Subsampled3a


CD4<-ggplot(Heat_Map_All, aes(x=V1, y=V2, color=`CD4`)) + geom_point(size=0.1)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD4 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD4")
CD4
CD4+scale_color_gradient(low="blue", high="red")
