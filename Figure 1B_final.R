# Heat Map Plot Creation
## Started by JG
## 07-24-18

rm(list=ls())

library(RColorBrewer)
library(Rtsne)
library(ggplot2)
library(splitstackshape)

setwd("~/Desktop/GVHD analyses/McIver Mouse Flow/combined viable")

load("~/Desktop/GVHD analyses/McIver Mouse Flow/combined viable/Figure1a_subsampled.Rda")
Heat_Map_All<-Subsampled

# Color by protein values for each marker
CD3<-ggplot(Heat_Map_All, aes(x=V1, y=V2, color=CD3)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD3 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD3")
CD3
CD3+scale_color_gradient(low="blue", high="red")

CD8<-ggplot(Heat_Map_All, aes(x=V1, y=V2, color=CD8)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD8 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD8")
CD8
CD8+scale_color_gradient(low="blue", high="red")

CD4<-ggplot(Heat_Map_All, aes(x=V1, y=V2, color=CD4)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD4 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD4")
CD4
CD4+scale_color_gradient(low="blue", high="red")

LAG_3<-ggplot(Heat_Map_All, aes(x=V1, y=V2, color=`LAG-3`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of LAG-3 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "LAG-3")
LAG_3
LAG_3+scale_color_gradient(low="blue", high="red")

PD_1<-ggplot(Heat_Map_All, aes(x=V1, y=V2, color=`PD-1`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of PD-1 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "PD-1")
PD_1
PD_1+scale_color_gradient(low="blue", high="red")

MLR_PD<-Subsampled[Treatment=="MLR+PD",]