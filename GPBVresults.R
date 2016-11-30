#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
igdx("C:/GAMS/win64/24.7/")

#Clear environment
rm(list = ls())

#Setwd
setwd("C:/Users/ddpue/Documents/GPBV Flanders/R/")

#Scenarios
Scenarios <- c("Reference", "Scenario2", "Scenario3", "Scenario4", "Scenario5", "Scenario6", "Scenario7")

#Sources
Sources <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/Sources.txt")

#NumberofData
ND <- length(Scenarios) * nrow(Sources)

#Read in Data - Global results
GlobPars <- c('dTotalProfit', 'dClosedFarms', 'dTotalImpact', 'dPercentageOccupiedRegion')
GlobalData <- sapply(GlobPars, function(x) {
        tmp <- rgdx.param("merged", x, names = 'Scenario', squeeze = FALSE)
        tmp$value
})
GlobalData <- data.frame(matrix(unlist(GlobalData), nrow = length(Scenarios)))
colnames(GlobalData) <- GlobPars
rownames(GlobalData) <- Scenarios

GlobalData$dClosedFarms <- GlobalData$dClosedFarms - 1 #Substract turkey farm

#Read in Data - Farm results
FarmPars <- c('dProfitFarm', 'dTotalImpactScore', 'dSignificanceScore',  'pFarmColour') 
FarmData <- sapply(FarmPars, function(x){
        tmp <- rgdx.param("merged", x, names = 'Scenario', squeeze = FALSE)
        tmp$value
})

FarmData <- data.frame(FarmData)

FarmData <- apply(FarmData, 2, function(x){
        tmp <- data.frame(matrix(x, nrow = nrow(Sources)))
        rownames(tmp) <- Sources$ID
        colnames(tmp) <- Scenarios
        print(tmp)
})

#Unwrap List with farm-level parameters to different data.frames
list2env(FarmData,envir=.GlobalEnv)

#Barplots with global results
ggplot(data=GlobalData, aes(x=rownames(GlobalData), y=dTotalProfit, fill=rownames(GlobalData)))+
               geom_bar(colour = "black", width=.8, stat="identity")+
               xlab("Scenarios") + ylab("Total Profit (€)")+
                ggtitle("Total societal profit")+
                guides(fill=FALSE)

ggplot(data=GlobalData, aes(x=rownames(GlobalData), y=dClosedFarms, fill=rownames(GlobalData)))+
        geom_bar(colour = "black", width=.8, stat="identity")+
        xlab("Scenarios") + ylab("Number of closed farms")+
        ggtitle("Number of farms that are closed")+
        guides(fill=FALSE)

ggplot(data=GlobalData, aes(x=rownames(GlobalData), y=dTotalImpact, fill=rownames(GlobalData)))+
        geom_bar(colour = "black", width=.8, stat="identity")+
        xlab("Scenarios") + ylab("Total Impact Score")+
        ggtitle("Total Impact Score of all farms combined")+
        guides(fill=FALSE)

ggplot(data=GlobalData, aes(x=rownames(GlobalData), y=dPercentageOccupiedRegion, fill=rownames(GlobalData)))+
        geom_bar(colour = "black", width=.8, stat="identity")+
        xlab("Scenarios") + ylab("% occupancy of stables")+
        ggtitle("Occupancy of permitted stables")+
        guides(fill=FALSE)


