## Figure 3A Grayson et al
## Jason M. Grayson
## Started on 08-20-2018

rm(list=ls())

library(RColorBrewer)
library(Rtsne)
library(ggplot2)
library(splitstackshape)

setwd("~Your working directory")

#clear the workspace
rm(list = ls())
load(file="Labels_All.Rda")
load(file = "tsne_surface_Viable.Rda")
tSNE_data<-as.data.frame(tsne$Y)
Flow_data<-cbind(tSNE_data,Labels_All)
rm(Labels_All,tsne,tSNE_data)
colnames(Flow_data)<-c("V1","V2","Mouse","Treatment","Outcome","GVHD_score","AML","Survival_time","Concentration")
load("Viable.Rda")
load("Viable_after_trans.Rda")

Flow_data<-cbind(Flow_data,Viable_after_trans)
set.seed(2)
Subsampled<-stratified(Flow_data, c("Treatment","Outcome","Mouse"),size=300, replace=TRUE)
Subsampled3a<-subset(Subsampled,Treatment!="B6")
Subsampled3a<-subset(Subsampled3a,Treatment!="MLR")
Subsampled3a<-subset(Subsampled3a,Treatment!="MLR+PD")
save(Subsampled3a,file="Fig3A_data.Rda")
Subsampled3a<-droplevels(Subsampled3a)
g2<-ggplot(Subsampled3a, aes(x=V1,y=V2,color=Outcome))+geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ guides(color = guide_legend(override.aes = list(size=2)))
g2

#Code for supplemental figures
# Color by protein values for each marker
CD3<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=CD3)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD3 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD3")
CD3
CD3+scale_color_gradient(low="blue", high="red")

CD8<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=CD8)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD8 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD8")
CD8
CD8+scale_color_gradient(low="blue", high="red")

CD4<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=CD4)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD4 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD4")
CD4
CD4+scale_color_gradient(low="blue", high="red")

LAG_3<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`LAG-3`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of LAG-3 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "LAG-3")
LAG_3
LAG_3+scale_color_gradient(low="blue", high="red")

PD_1<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`PD-1`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of PD-1 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "PD-1")
PD_1
PD_1+scale_color_gradient(low="blue", high="red")

CD44<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`CD44`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD44 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD44")
CD44
CD44+scale_color_gradient(low="blue", high="red")

CD62L<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`CD62L`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD62L in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD62L")
CD62L
CD62L+scale_color_gradient(low="blue", high="red")

IFNg<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`IFNg`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of IFNg in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "IFNg")
IFNg
IFNg+scale_color_gradient(low="blue", high="red")

TNFa<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`TNFa`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of TNFa in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "TNFa")
TNFa
TNFa+scale_color_gradient(low="blue", high="red")

IL_2<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`IL-2`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of IL-2 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "IL-2")
IL_2
IL_2+scale_color_gradient(low="blue", high="red")

CTLA_4<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`CTLA-4`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CTLA-4 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CTLA-4")
CTLA_4
CTLA_4+scale_color_gradient(low="blue", high="red")

CD25a<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`CD25a`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of CD25 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "CD25a")
CD25a
CD25a+scale_color_gradient(low="blue", high="red")
