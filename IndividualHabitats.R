#Which individual habitat cells improve, which cells deteriorate (based on impact scores)
#+emissions of all IED farms
#To be joined with vector layers in QGIS
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(e1071)
library(ineq)
igdx("C:/GAMS/win64/24.7/")

setwd("C:/Users/ddpue/Documents/GPBV Flanders/R")

#Reading GDX data
HabitatScores <- rgdx.param('IED.gdx', symName = 'rIS', squeeze = FALSE)
SourceEmissions <- rgdx.param('IED.gdx', symName='rAmmoniaEmissionFarm', squeeze=FALSE)
Habitatcoordinates <- rgdx.param('IED.gdx', symName='pReceptorCoord', squeeze=FALSE)

#Cast to wide format
HabitatScoresCast <- dcast(HabitatScores, sReceptor~scenario)
SourceEmissionsCast <- dcast(SourceEmissions, sFarm~scenario)
Habitatcoordinates <- dcast(Habitatcoordinates, sReceptor~sCoordinates)

#DistributionHabitatScores
ggplot(HabitatScores, aes(x=rIS, fill=scenario))+
        geom_density(alpha=.3)

ggplot(HabitatScores, aes(x=log(rIS), fill=scenario))+
        geom_density(alpha=.3)


ggplot(SourceEmissions, aes(x=rAmmoniaEmissionFarm, fill=scenario))+
        geom_density(alpha=.3)

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

ineq(HabitatScoresCast$scen1, type='Gini')
ineq(HabitatScoresCast$scen2, type='Gini')
ineq(HabitatScoresCast$scen3, type='Gini')
ineq(HabitatScoresCast$scen5, type='Gini')



boxplot(log(HabitatScoresCast$scen1))
#Calculate change in HabitatScores and emissions for spatially optimized cases
HabitatScoresCast$Scen2Diff <- HabitatScoresCast$scen2 - HabitatScoresCast$scen1
HabitatScoresCast$Scen3Diff <- HabitatScoresCast$scen3 - HabitatScoresCast$scen1
SourceEmissionsCast$Scen2rel <- (SourceEmissionsCast$scen2/SourceEmissionsCast$scen1 -1) * 100
SourceEmissionsCast$Scen3rel <- (SourceEmissionsCast$scen3/SourceEmissionsCast$scen1-1) * 100

#Replace NA by zero
HabitatScoresCast[is.na(HabitatScoresCast)] <- 0
SourceEmissionsCast[is.na(SourceEmissionsCast)] <- 0

HabitatScoresCast <- data.frame(HabitatScoresCast, Habitatcoordinates[,2:3])

#Write .csv
write.csv(HabitatScoresCast, "HabitatScores.csv")
write.csv(SourceEmissionsCast, "SourceEmissions.csv")

