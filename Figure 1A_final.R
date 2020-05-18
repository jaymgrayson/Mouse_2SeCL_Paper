##Figure 1A script
##Coded by Millie Perez and Jason Grayson

rm(list=ls())

library(RColorBrewer)
library(Rtsne)
library(ggplot2)
library(splitstackshape)

setwd("~/Desktop/GVHD analyses/McIver Mouse Flow/combined viable")

load("Viable.Rda")
load("Viable_after_trans.Rda")

ALL<-as.data.frame(scale(Viable_after_trans))

ALL<-cbind(ALL,Viable_with_label$Mouse,Viable_with_label$Treatment,Viable_with_label$Outcome,Viable_with_label$GVHD_score,Viable_with_label$AML,Viable_with_label$Survival_time,Viable_with_label$Concentration) 


set.seed(1234)
ALL.train<-ALL

Labels_All<-ALL[,c(14:20)]
save(Labels_All,file="Labels_All.Rda") #Now you have a label Rda
ALL.train<-ALL.train[,-c(14:20)]
#Executing the algorithm on curated data, only use iter=1000, most error lost by 300, saves a lot of time
tsne<- Rtsne(ALL.train, dims = 2, perplexity=30, verbose=TRUE, max_iter = 1000)

#Save the tsne
save(tsne,file = "tsne_surface_Viable.Rda")

#clear the workspace
rm(list = ls())
load(file="Labels_All.Rda")
load(file = "tsne_surface_Viable.Rda")
tSNE_data<-as.data.frame(tsne$Y)
Flow_data<-cbind(tSNE_data,Labels_All)
rm(Labels_All,tsne,tSNE_data)
colnames(Flow_data)<-c("V1","V2","Mouse","Treatment","Outcome","GVHD_score","AML","Survival_time","Concentration")

#Celltype vs treatment
g1<-ggplot(Flow_data, aes(x=V1,y=V2,color=Outcome))+geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ guides(color = guide_legend(override.aes = list(size=2)))
g1

load(file="Scaled_wout_labels.Rda")
Flow_data<-cbind(Flow_data,ALL.train)
save(Flow_data,file="Data_Fig1B_HeatMaps.Rda")
#Flow_data<-Flow_data[,c(1:9)]
set.seed(2)
Subsampled<-stratified(Flow_data, c("Treatment","Outcome","Mouse"),size=300, replace=TRUE)
Subsampled1<-subset(Subsampled,Subsampled$Treatment=="B6")
Subsampled2<-subset(Subsampled,Subsampled$Treatment=="MLR")
Subsampled3<-subset(Subsampled,Subsampled$Treatment=="MLR+PD")
Subsampled<-rbind(Subsampled1,Subsampled2,Subsampled3)

save(Subsampled,file="Figure1A_subsampled.Rda")

g2<-ggplot(Subsampled, aes(x=V1,y=V2,color=Outcome))+geom_point(size=0.1)+facet_wrap(~Treatment)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ guides(color = guide_legend(override.aes = list(size=2)))
g2
## This then saved as pdf for import into canvas
