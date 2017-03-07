#Which individual habitat cells improve, which cells deteriorate (based on impact scores)
#+emissions of all IED farms
#To be joined with vector layers in QGIS
library(gdxrrw)
library(reshape2)
library(rgdal)
library(maptools)
library(ggplot2)
library(RColorBrewer)
igdx("C:/GAMS/win64/24.7/")

#Clear environment
rm(list = ls())

setwd("C:/Users/ddpue/Documents/GPBV Flanders/R")

#Reading GDX data
#HabitatScores <- rgdx.param('IED.gdx', symName = 'rIS', squeeze = FALSE)
HabitatScores <- rgdx.param('combo.gdx', symName = 'zIS', squeeze = FALSE)
HabitatsScores2 <- rgdx.param('IED.gdx', symName='rIS', squeeze=FALSE)
#SourceEmissions <- rgdx.param('IED.gdx', symName='rAmmoniaEmissionFarm', squeeze=FALSE)
SourceEmissions <- rgdx.param('combo.gdx', symName='zAmmoniaEmissionFarm', squeeze=FALSE)
Habitatcoordinates <- rgdx.param('IED.gdx', symName='pReceptorCoord', squeeze=FALSE)
MaximumIS <- rgdx.param('test.gdx', symName='pISr', squeeze=FALSE)

#Cast to wide format
#HabitatScoresCast <- dcast(HabitatScores, sReceptor~scenario)
HabitatScoresCast <- dcast(HabitatScores, sReceptor~run)
HabitatsScores2Cast <- dcast(HabitatsScores2, sReceptor~scenario)
#SourceEmissionsCast <- dcast(SourceEmissions, sFarm~scenario)
SourceEmissionsCast <- dcast(SourceEmissions, sFarm~run)
Habitatcoordinates <- dcast(Habitatcoordinates, sReceptor~sCoordinates)

HabitatScoresCast <- data.frame(cbind(HabitatsScores2Cast[,1:5], HabitatScoresCast[,3:5]))
rm(HabitatsScores2Cast)
colnames(HabitatScoresCast) <- c("receptor", "scen1", "scen2", "scen3", "scen4", "scen5", "scen6", "scen7")

#Reading polygon with Flemish province borders
provinces <- readOGR(dsn="C:/Users/ddpue/Documents/GPBV Flanders/GIS/Referentiebestanden Vlaams Gewest", 
                     layer="Refprv")

#Relative improvement in deposition compared to full capacity case
Improvement <- HabitatScoresCast[,2:8]/MaximumIS$pISr
Improvement <- data.frame(cbind(Habitatcoordinates, Improvement))

#Assign classes (A to G, A: big improvement, F: no improvement) to relative change of all scenarios. 
ChangeClasses <- as.data.frame(apply(Improvement[,4:10], 2, function(x){
        ClassVector <- cut(x, c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.9999999, 1.1), 
                           c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"))
        print(ClassVector)
}))

ChangeClasses <- data.frame(cbind(Habitatcoordinates, ChangeClasses))
ChangeClasses[is.na(ChangeClasses)] <- 'F'

# #Colour palette for maps (Green-Yellow-Red)
# palette <- c("#008B00", "#B3EE3A", "#C0FF3E", "khaki1", "#FFA500", "#FF4500", "#CD0000") 
#         
#         
# #Map habitats using ggplot
# ggplot(data=ChangeClasses, aes(x=X, y=Y, group=run2, colour=run2))+
#         ggtitle("SS<4 TIS<12")+
#         geom_polygon(data=provinces, aes(x=long, y=lat, group=group), colour="Black", fill="LightGray")+
#         geom_point()+
#         guides(fill=FALSE)+
#         scale_color_manual(values="khaki1", name="Deposition\nrelative to\nreference", 
#                            labels=c("0.8-1.2"))
# 
# ggplot(data=ChangeClasses, aes(x=X, y=Y, group=run3, colour=run3))+
#         ggtitle("SS<7 TIS<7")+
#         geom_polygon(data=provinces, aes(x=long, y=lat, group=group), colour="Black", fill="LightGray")+
#         geom_point()+
#         guides(fill=FALSE)+
#         scale_color_manual(values=palette[3:5], name="Deposition\nrelative to\nreference", 
#                            labels=c("0.5-0.8", "0.8-1.2", "1.2-2"))
# 
# ggplot(data=ChangeClasses, aes(x=X, y=Y, group=run4, colour=run4))+
#         ggtitle("SS<13 TIS<6")+
#         geom_polygon(data=provinces, aes(x=long, y=lat, group=group), colour="Black", fill="LightGray")+
#         geom_point()+
#         guides(fill=FALSE)+
#         scale_color_manual(values=palette[2:6], name="Deposition\nrelative to\nreference", 
#                            labels=c("0.2-0.5", "0.5-0.8", "0.8-1.2", "1.2-2", "2-5"))
# 
# ggplot(data=ChangeClasses, aes(x=X, y=Y, group=run5, colour=run5))+
#         ggtitle("SS<19 & TIS<5.5")+
#         geom_polygon(data=provinces, aes(x=long, y=lat, group=group), colour="Black", fill="LightGray")+
#         geom_point()+
#         guides(fill=FALSE)+
#         scale_color_manual(values=palette[2:6], name="Deposition\nrelative to\nreference", 
#                            labels=c("0.2-0.5", "0.5-0.8", "0.8-1.2", "1.2-2", "2-5"))
# 
# ggplot(data=ChangeClasses, aes(x=X, y=Y, group=run6, colour=run6))+
#         ggtitle("TIS<5.1")+
#         geom_polygon(data=provinces, aes(x=long, y=lat, group=group), colour="Black", fill="LightGray")+
#         geom_point()+
#         guides(fill=FALSE)+
#         scale_color_manual(values=palette[2:7], name="Deposition\nrelative to\nreference", 
#                            labels=c("0.2-0.5", "0.5-0.8", "0.8-1.2", "1.2-2", "2-5", ">5"))

##Plot maximum deposition
#Get idea of distribution values
quantile(MaximumIS$pISr, probs=0.1*c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

#Assign classes (A to D, A: low IS, E: very high IS) to all habitats. 
DepositionClasses <- cut(MaximumIS$pISr, c(0, 0.001, 0.01, 0.1, 200), c("A", "B", "C", "D"))

DepositionClasses <- data.frame(cbind(Habitatcoordinates, DepositionClasses))

#Palettes
palette1 <- brewer.pal(11, 'Spectral') #Relative improvement habitats
palette2 <- c("#008B00","#B3EE3A","#FF4500", "#CD0000")  #IS per habitat
getPalette = colorRampPalette(c("khaki2", "darkolivegreen4", "cadetblue2", "dodgerblue4"))

#Map habitats using ggplot
#IS at full capacity
ggplot(data=DepositionClasses, aes(x=X, y=Y, group=DepositionClasses, colour=DepositionClasses))+
        ggtitle("Deposition at full capacity")+
        geom_polygon(data=provinces, aes(x=long, y=lat, group=group), colour="Black", fill="LightGray")+
        geom_point()+
        guides(fill=FALSE)+
        scale_color_manual(values=palette2, name="Deposition/CL",
                           labels=c("<0.001", "0.001-0.01", "0.01-0.1", ">0.1"))

scenarios <- c(1:7)
titles <- c("SS<3.75", "SO max. benefit", "SO min. impact", "TIS<5.1",
            "SS<4 TIS<12", "SS<7 TIS<7", "SS<13 TIS<6")
scenarios <- data.frame(cbind(scenarios, titles))

apply(scenarios, 1, function(x){
        index <- as.integer(x[1])+3 
        Classes <- data.frame(cbind(ChangeClasses[,2:3], ChangeClasses[,index]))
        colnames(Classes) <- c("X", "Y", "value")
        ggplot(data=Classes, aes(x=X, y=Y, group=value, colour=value))+
                ggtitle(x[2])+
                geom_polygon(data=provinces, aes(x=long, y=lat, group=group), colour="Black", fill="LightGray")+
                geom_point()+
                guides(fill=FALSE)+
                scale_color_manual(values=rev(getPalette(11)), name="Decrease deposition\nrelative to\nfull capacity",
                                   labels=c(">90%", "80-90%", "70-80%", "60-70%", "50-60%", "40-50%", "30-40%", "20-30",
                                            "10-20%", "<10", "no improvement"))
})


