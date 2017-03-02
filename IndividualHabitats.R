#Which individual habitat cells improve, which cells deteriorate (based on impact scores)
#+emissions of all IED farms
#To be joined with vector layers in QGIS
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(e1071)
library(RColorBrewer)
igdx("C:/GAMS/win64/24.7/")

#Clear environment
rm(list = ls())

setwd("C:/Users/ddpue/Documents/GPBV Flanders/R")

#Reading GDX data
HabitatScores <- rgdx.param('IED.gdx', symName = 'rIS', squeeze = FALSE)
SourceEmissions <- rgdx.param('IED.gdx', symName='rAmmoniaEmissionFarm', squeeze=FALSE)
Habitatcoordinates <- rgdx.param('IED.gdx', symName='pReceptorCoord', squeeze=FALSE)

#Cast to wide format
HabitatScoresCast <- dcast(HabitatScores, sReceptor~scenario)
SourceEmissionsCast <- dcast(SourceEmissions, sFarm~scenario)
Habitatcoordinates <- dcast(Habitatcoordinates, sReceptor~sCoordinates)

#Map habitats using ggplot
ggplot(data=Habitatcoordinates, aes(x=X, y=Y))+
        geom_point()

#DistributionHabitatScores
ggplot(HabitatScores, aes(x=rIS, fill=scenario))+
        geom_density(alpha=.3)
ggplot(HabitatScores, aes(x=log(rIS), fill=scenario))+
        geom_density(alpha=.3)
ggplot(SourceEmissions, aes(x=rAmmoniaEmissionFarm, fill=scenario))+
        geom_density(alpha=.3)

#Normalise habitatscores by dividing them with the reference value
RelativeChange <- data.frame(cbind(Habitatcoordinates$sReceptor, (HabitatScoresCast[,3:6]/HabitatScoresCast$scen1)))
RelativeChange <- melt(RelativeChange)

#Plot distribution of relative change
ggplot(RelativeChange, aes(x=value, fill=variable))+
        geom_density(alpha=.3)

ggplot(RelativeChange, aes(x=log(value), fill=variable))+
        geom_density(alpha=.3)+
        coord_cartesian(xlim=c(-0.5,0.5))

RelativeChange <- data.frame(cbind(Habitatcoordinates, RelativeChange))
RelativeChange$Habitatcoordinates.sReceptor <- NULL

RelativeChange$scen2bin <- "less"
RelativeChange$scen2bin[which(RelativeChange$scen2 > 1)] <- "more"

RelativeChange$scen3bin <- "less"
RelativeChange$scen3bin[which(RelativeChange$scen3 > 1)] <- "more"

RelativeChange$scen4bin <- "less"
RelativeChange$scen4bin[which(RelativeChange$scen4 > 1)] <- "more"

RelativeChange$scen5bin <- "less"
RelativeChange$scen5bin[which(RelativeChange$scen5 > 1)] <- "more"

#Map habitats using ggplot
ggplot(data=RelativeChange, aes(x=X, y=Y, group=scen2bin, colour=scen2bin))+
        geom_point()+
        scale_colour_manual(values = c("green", "red"))

ggplot(data=RelativeChange, aes(x=X, y=Y, group=scen3bin, colour=scen3bin))+
        geom_point()+
        scale_colour_manual(values = c("green", "red"))

ggplot(data=RelativeChange, aes(x=X, y=Y, group=scen4bin, colour=scen4bin))+
        geom_point()+
        scale_colour_manual(values = c("green", "red"))

ggplot(data=RelativeChange, aes(x=X, y=Y, group=scen5bin, colour=scen5bin))+
        geom_point()+
        scale_colour_manual(values = c("green", "red"))


##Rest: not used
#Look at skewness, variance and inequality
skewness(HabitatScoresCast$scen1)
skewness(HabitatScoresCast$scen2)
skewness(HabitatScoresCast$scen3)
skewness(HabitatScoresCast$scen4)
skewness(HabitatScoresCast$scen5)

var(HabitatScoresCast$scen1)
var(HabitatScoresCast$scen2)
var(HabitatScoresCast$scen3)
var(HabitatScoresCast$scen4)
var(HabitatScoresCast$scen5)



#Replace NA by zero
HabitatScoresCast[is.na(HabitatScoresCast)] <- 0
SourceEmissionsCast[is.na(SourceEmissionsCast)] <- 0

HabitatScoresCast <- data.frame(HabitatScoresCast, Habitatcoordinates[,2:3])

#Write .csv
write.csv(HabitatScoresCast, "HabitatScores.csv")
write.csv(SourceEmissionsCast, "SourceEmissions.csv")


