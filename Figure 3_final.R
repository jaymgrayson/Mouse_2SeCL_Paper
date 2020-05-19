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

# Each condition is plotted, then image exported as pdf and imported into Canvas and condition order rearranged.
g2<-ggplot(Subsampled3a, aes(x=V1,y=V2,color=Outcome))+geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ guides(color = guide_legend(override.aes = list(size=2)))
g2

#Code for supplemental figures
# Color by protein values for each marker
# Example shown for LAG-3

LAG_3<-ggplot(Subsampled3a, aes(x=V1, y=V2, color=`LAG-3`)) + geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Heatmap of LAG-3 in Various Treatments of Mice") + theme(plot.title = element_text(hjust = 0.5))+ labs(colour = "LAG-3")
LAG_3
LAG_3+scale_color_gradient(low="blue", high="red")
