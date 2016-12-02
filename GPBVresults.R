#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(scales)
library(ineq)
igdx("C:/GAMS/win64/24.7/")

#Source MultiplotFunction
source("C:/Users/ddpue/Documents/R/Multiplot.R")

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
GlobPars <- c('dTotalProfit', 'dClosedFarms', 'dTotalImpact', 'dPercentageOccupiedRegion', 'dAmmoniaEmissionRegion')
GlobalData <- sapply(GlobPars, function(x) {
        tmp <- rgdx.param("merged", x, names = 'Scenario', squeeze = FALSE)
        tmp$value
})
GlobalData <- data.frame(matrix(unlist(GlobalData), nrow = length(Scenarios)))
colnames(GlobalData) <- GlobPars
rownames(GlobalData) <- Scenarios

GlobalData$dClosedFarms <- GlobalData$dClosedFarms - 1 #Substract turkey farm

#Read in Data - Farm results
FarmPars <- c('dProfitFarm', 'dTotalImpactScore', 'dSignificanceScore',  'pFarmColour', 'pSS', 'pTIS') 
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

#Define some extra Global results
GlobalData$CostPM <- GlobalData$dAmmoniaEmissionRegion * 12 
GlobalData$PrivateProfit <- GlobalData$dTotalProfit + GlobalData$CostPM
GlobalData$Scenario <- rownames(GlobalData)
GlobalData$BenefitToImpactRatio <- GlobalData$dTotalProfit / (GlobalData$dTotalImpact )

#Long Data frame with  social costs/private benefits
SCPB <- GlobalData
SCPB[,2:5] <- NULL
SCPB$PrivateProfit <- NULL
SCPB$BenefitToImpactRatio <- NULL
SCPB <- melt(SCPB)

#Stacked plot with costPM and PrivateProfit
p1 <- ggplot(data=SCPB, aes(x=Scenario, y=value, fill=variable))+
        geom_bar(colour = "black", width=.8, stat="identity")+
        guides(fill=guide_legend(title=NULL)) +
        scale_fill_manual(labels=c("Total Societal Benefit (€)", "PM Health Cost (€)"),
                            values=c("#009E73", "#D55E00"))+
        ggtitle("Total private benefit as sum of total societal benefit and external cost")+
        theme(plot.title = element_text(size = 15, face = "bold")) +
        xlab(NULL) + ylab(NULL) +
        theme(text= element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1, face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.y = element_text(angle=0))

plot(p1)
ggsave("TotalBenefits.png", width=10, height=10, dpi=400)

#Plot with number of closed farms
p2 <- ggplot(data=GlobalData, aes(x=rownames(GlobalData), y=dClosedFarms))+
        geom_bar(colour = "black", width=.8, stat="identity", fill = "#DD8888")+
        xlab(NULL) + ylab(NULL)+
        ggtitle("Number of farms that are closed")+
        guides(fill=FALSE)+
        theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, face="bold"))

plot(p2)
ggsave("ClosedFarms.png", width=10, height=10, dpi=400)

#Plot total impact score
p3 <- ggplot(data=GlobalData, aes(x=rownames(GlobalData), y=dTotalImpact))+
        geom_bar(colour = "black", width=.8, stat="identity", fill = "#DD8888")+
        xlab(NULL) + ylab(NULL)+
        ggtitle("Total impact score of all farms combined")+
        guides(fill=FALSE)+
        theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, face="bold"))

plot(p3)
ggsave("TotalImpactScore.png", width=10, height=10, dpi=400)

#Plot Benefit to impact ratio
p4<- ggplot(data=GlobalData, aes(x=rownames(GlobalData), y=BenefitToImpactRatio))+
        geom_bar(colour = "black", width=.8, stat="identity", fill = "#DD8888")+
        xlab(NULL) + ylab("€/TIS")+
        ggtitle("Benefit to impact ratio")+
        guides(fill=FALSE)+
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=90, face="bold"))

plot(p4)
ggsave("BenefitToImpact.png", width=10, height=10, dpi=400)

#Scatterplot Total Impact Score vs SS (full capacity)
ImpactScores <- data.frame(cbind(pTIS$Reference, pSS$Reference))
colnames(ImpactScores) <- c("TIS", "SS")
ImpactScores$Colour <- as.factor(pFarmColour$Reference)

ggplot(dat=ImpactScores, aes(x=TIS, y=SS, colour=Colour))+
        geom_point(shape=18, size = 2)+
        scale_x_continuous(trans=log10_trans())+
        scale_y_continuous(trans=log10_trans())+
        scale_colour_manual(values = c("green", "orange", "red"), labels = c("<5%", "5-50%", ">50%"), 
                          guide_legend(title="Significance Class"))+
        ggtitle("Significance Score versus Total Impact Score")+
        theme(text = element_text(size=15))
ggsave("SSvsTIS.png", dpi=400)

#Scatterplots Profit versus Impact
for (i in (1:ncol(dProfitFarm))) {
        scen <- paste('Scenario', as.character(i),sep= "")
        df <- data.frame(cbind(dProfitFarm[,i], dTotalImpactScore[,i]))
        colnames(df) <- c("Profit", "TIS")
        ggplot(dat=df, aes(x=TIS, y=Profit))+
                geom_point(shape=1)+
        ggtitle(paste("Profit versus Impact - ", scen, sep=""))
        ggsave(paste(scen, "_ProfitvsTIS.png", sep=""), dpi=400)
}

#Ginicoefficient all scenarios
GlobalData$Gini <- apply(dProfitFarm, 2, function(x){
        subselect <- x[which(x > 0)]
        ineq(subselect, type="Gini")
})

