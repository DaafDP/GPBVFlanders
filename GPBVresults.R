#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
igdx("C:/GAMS/win64/24.7/")

#Setwd
setwd("C:/Users/ddpue/Documents/GPBV Flanders/R/")

#Scenarios
Scenarios <- c("Reference", "Scenario2", "Scenario3", "Scenario4", "Scenario5", "Scenario6", "Scenario7")

#Read in Data - Global results
GlobPars <- c('dTotalProfit', 'dClosedFarms', 'dTotalImpact', 'dPercentageOccupiedRegion')
GlobalData <- sapply(GlobPars, function(x) {
        tmp <- rgdx.param("merged", x, names = 'Scenario', squeeze = FALSE)
        tmp$value
})
GlobalData <- data.frame(matrix(unlist(GlobalData), nrow = 7))
colnames(GlobalData) <- GlobPars
rownames(GlobalData) <- Scenarios

#Read in Data - Farm results
FarmPars <- c('dProfitFarm', 'dTotalImpactScore', 'dSignificanceScore',  'pFarmColour') 
FarmData <- sapply(FarmPars, function(x){
        tmp <- rgdx.param("merged", x, names = 'Scenario', squeeze = FALSE)
        tmp$value
})


