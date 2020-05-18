## Figure 4 Grayson et al
## Coded by Millie Perez and modded by Jason M. Grayson
## Revision start 8-21-2018

# This code is used to take scaled surface after FlowTrans and generate a Self Organizing Map(SOM)

## wipe the workspace
rm(list = ls())

setwd("~/Desktop/GVHD analyses/McIver Mouse Flow/combined viable")

library(flowCore)
library(flowTrans)
library(tidyverse)
library(RColorBrewer)
library(latticeExtra) ##3d barplot
library(FlowSOM) #FlowSOM

#read the written Rda file that is the data after transformation
load(file="Fig3A_data.Rda")
Labels<-Subsampled3a[,c(1:9)]
Subsampled3a<-Subsampled3a[,-c(1:9)]
#Extract the transformed data
ALL<-as.matrix(Subsampled3a)

##FCS conversion for FlowSOM
ALL_FCS <- flowFrame(ALL) # convert the pooled cells to FCS

#add treatment label back

ALL_with_label <- cbind(as.data.frame(ALL),treatment=as.data.frame(Labels)[,4])

AML <- ALL_with_label[ALL_with_label$treatment == "AML",][,-14] #extract healthy controls data
AML_GVHD <- ALL_with_label[ALL_with_label$treatment == "AML+GVHD",][,-14]  
AML_PD<-ALL_with_label[ALL_with_label$treatment == "AML+PD",][,-14]
AML_PD2<-ALL_with_label[ALL_with_label$treatment == "AML+PD 2",][,-14]
Third<-ALL_with_label[ALL_with_label$treatment == "Third",][,-14]
Cytoxan<-ALL_with_label[ALL_with_label$treatment == "GVHD+Cytoxan",][,-14]
#convert data to FCS
 
AML <- flowFrame(as.matrix(AML)) 
AML_GVHD<-flowFrame(as.matrix(AML_GVHD))
AML_PD<-flowFrame(as.matrix(AML_PD))
AML_PD2<-flowFrame(as.matrix(AML_PD2)) 
Third<-flowFrame(as.matrix(Third))
Cytoxan<-flowFrame(as.matrix(Cytoxan))

#FlowSOM
set.seed(2L)
fSOM_ALL <- ReadInput(ALL_FCS,compensate = FALSE, transform = FALSE) #Read the pooled FCS into FlowSom
fSOM_ALL <- BuildSOM(fSOM_ALL, xdim=6,ydim=5,rlen=100) #Build Self-Organizing Map(SOM)
fSOM_ALL <- BuildMST(fSOM_ALL) #Build Minimal Spanning Tree(MST)
PlotStars(fSOM_ALL, legend = TRUE) #Plot MST

fSOM_AML <- NewData(fSOM_ALL,AML) #Map data to the SOM grid (standard formed by total population)
PlotStars(fSOM_AML, legend = TRUE)

fSOM_AML_GVHD <- NewData(fSOM_ALL,AML_GVHD) #Map data to the SOM grid (standard formed by total population)
PlotStars(fSOM_AML_GVHD, legend = TRUE)


fSOM_AML_PD <- NewData(fSOM_ALL,AML_PD) #Map data to the SOM grid (standard formed by total population)
PlotStars(fSOM_AML_PD, legend = TRUE)

fSOM_AML_PD2 <- NewData(fSOM_ALL,AML_PD2) #Map data to the SOM grid (standard formed by total population)
PlotStars(fSOM_AML_PD2, legend = TRUE)
fSOM_Third <- NewData(fSOM_ALL,Third) #Map data to the SOM grid (standard formed by total population)
PlotStars(fSOM_Third, legend = TRUE)

fSOM_Cytoxan <- NewData(fSOM_ALL,Cytoxan) #Map data to the SOM grid (standard formed by total population)
PlotStars(fSOM_Cytoxan, legend = TRUE)
