#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(scales)
igdx("C:/GAMS/win64/24.7/")

#Clear environment
rm(list = ls())

#Source MultiplotFunction
#source("C:/Users/ddpue/Documents/R/Multiplot.R")

#Setwd
setwd("C:/Users/ddpue/Documents/GPBV Flanders/R/")

#Scenarios
Scenarios <- c("Reference", "Scenario2", "Scenario3", "Scenario4", "Scenario5", "Scenario6", "Scenario7")

#Sources
Sources <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/Sources.txt")


#NumberofData
ND <- length(Scenarios) * nrow(Sources)

#Read in Data - Global results
GlobPars <- c('dTotalProfit', 'dClosedFarms', 'dTotalImpact', 'dPercentageMaxProfitRegion', 'dAmmoniaEmissionRegion'
              ,'dMaxProfitRegion', 'dMaxImpactRegion', 'dMaxAmmoniaEmissionRegion')
GlobalData <- sapply(GlobPars, function(x) {
        tmp <- rgdx.param("merged", x, names = 'Scenario', squeeze = FALSE)
        tmp$value
})
GlobalData <- data.frame(matrix(unlist(GlobalData), nrow = length(Scenarios)))
colnames(GlobalData) <- GlobPars
rownames(GlobalData) <- Scenarios

GlobalData$dClosedFarms <- GlobalData$dClosedFarms - 1 #Substract turkey farm

#Read in Data - Farm results
FarmPars <- c('dProfitFarm', 'dTotalImpactScore', 'dSignificanceScore',  'pFarmColour', 'pSS', 'pTIS', 
              'dPercentageofMaxProfit', 'dMaxProfitFarm') 
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

HealthCost <- rgdx.param("merged", 'pHealthCost')

#Define some extra Global results
GlobalData$CostPM <- GlobalData$dAmmoniaEmissionRegion * HealthCost$value[1]
GlobalData$PrivateProfit <- GlobalData$dTotalProfit + GlobalData$CostPM
GlobalData$Scenario <- rownames(GlobalData)
GlobalData$BenefitToImpactRatio <- GlobalData$dTotalProfit / (GlobalData$dTotalImpact )

#Long Data frame with  social costs/private benefits
SCPB <- GlobalData
SCPB[,2:5] <- NULL
SCPB$PrivateProfit <- NULL
SCPB$BenefitToImpactRatio <- NULL
SCPB[,2:4] <- NULL
SCPB <- melt(SCPB)

#Stacked plot with costPM and PrivateProfit
p1 <- ggplot(data=SCPB, aes(x=Scenario, y=value, fill=variable))+
        geom_bar(colour = "black", width=.8, stat="identity")+
        guides(fill=guide_legend(title=NULL)) +
        scale_fill_manual(labels=c("Total Societal Benefit (€)", "PM Health Cost (€)"),
                            values=c("#009E73", "#D55E00"))+
        geom_hline(yintercept = GlobalData$dMaxProfitRegion[1], colour = "red", lty = 2, lwd=1) +
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
        geom_hline(yintercept=GlobalData$dMaxImpactRegion[1], colour = "red", lty=2, lwd=1)+
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

#Plot ammonia emission
p5 <- ggplot(data=GlobalData, aes(x=rownames(GlobalData), y=dAmmoniaEmissionRegion))+
        geom_bar(colour = "black", width=.8, stat="identity", fill = "#DD8888")+
        geom_hline(yintercept=GlobalData$dMaxAmmoniaEmissionRegion[1], colour = "red", lty=2, lwd=1)+
        xlab(NULL) + ylab(NULL)+
        ggtitle("Total ammonia emission of all farms combined")+
        guides(fill=FALSE)+
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=90, face="bold"))

plot(p5)
ggsave("TotalEmission.png", width=10, height=10, dpi=400)

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

#Scatterplots Profit versus Impact (TIS and SS)
for (i in (1:(ncol(dProfitFarm)))) {
        scen <- paste('Scenario', as.character(i),sep= "")
        df <- data.frame(cbind(dProfitFarm[,i], dTotalImpactScore[,i]), as.factor(pFarmColour$Reference))
        colnames(df) <- c("Profit", "TIS", "SignificanceClass")
        ggplot(dat=df, aes(x=TIS, y=Profit, colour=SignificanceClass))+
        geom_point(shape=20)+
        scale_colour_manual(values = c("green", "orange", "red"), labels = c("<5%", "5-50%", ">50%"), 
        guide_legend(title="Significance Class"))+
        ggtitle(paste("Profit versus Total Impact - ", scen, sep=""))
        ggsave(paste(scen, "_ProfitvsTIS.png", sep=""), dpi=400)
        
        scen <- paste('Scenario', as.character(i),sep= "")
        df <- data.frame(cbind(dProfitFarm[,i], dSignificanceScore[,i]), as.factor(pFarmColour$Reference))
        colnames(df) <- c("Profit", "SS", "SignificanceClass")
        ggplot(dat=df, aes(x=SS, y=Profit, colour=SignificanceClass))+
        geom_point(shape=20)+
        scale_colour_manual(values = c("green", "orange", "red"), labels = c("<5%", "5-50%", ">50%"), 
        guide_legend(title="Significance Class"))+
        ggtitle(paste("Profit versus Significance Score - ", scen, sep=""))
        ggsave(paste(scen, "_ProfitvsSS.png", sep=""), dpi=400)
}

colnames(Sources) <- c("ID", "X", "Y")

#Scatterplots Max. Profit versus Max Impact (TIS and SS)
df <- data.frame(cbind(dMaxProfitFarm[,1], pTIS[,1]), as.factor(pFarmColour$Reference))
colnames(df) <- c("Profit", "TIS", "SignificanceClass")
ggplot(dat=df, aes(x=TIS, y=Profit, colour=SignificanceClass))+
        geom_point(shape=20)+
        scale_colour_manual(values = c("green", "orange", "red"), labels = c("<5%", "5-50%", ">50%"), 
                            guide_legend(title="Significance Class"))+
        ggtitle("max. Profit versus max. Total Impact")
ggsave('maxProfitvsTIS.png', dpi=400)

df <- data.frame(cbind(dMaxProfitFarm[,1], pSS[,1]), as.factor(pFarmColour$Reference))
colnames(df) <- c("Profit", "SS", "SignificanceClass")
ggplot(dat=df, aes(x=SS, y=Profit, colour=SignificanceClass))+
        geom_point(shape=20)+
        scale_colour_manual(values = c("green", "orange", "red"), labels = c("<5%", "5-50%", ">50%"), 
                            guide_legend(title="Significance Class"))+
        ggtitle("max. Profit versus max. Significance Score")
ggsave('maxProfitvsSS.png', dpi=400)

#Make table with binary variable 1:closed 0:open
NonOperatingFarms <- apply(dProfitFarm, c(1,2), function(x){
                        if (x == 0) {
                        return(0)
                        }
                        else {
                        return(1)
                        }
                        })
NonOperatingFarms <- as.data.frame(cbind(Sources, NonOperatingFarms))

#Save as .csv
write.csv(NonOperatingFarms, "NonOperatingFarms.csv")

#Make table with constrained farms 1: constrained by SS/TIS ceiling 0: not constrained
SSconstrained <- dSignificanceScore[,1:3]
SSconstrained$Scenario2 <- NULL
SSconstrained<- apply(SSconstrained, c(1,2), function(x){
        if (isTRUE(all.equal(x, 5, tolerance =1e-5)) == TRUE |
            isTRUE(all.equal(x, 10, tolerance =1e-5)) == TRUE){
                return(1)
        }
        else {
                return(0)
        }
})
SSconstrained <- as.data.frame(cbind(Sources, SSconstrained))

TISconstrained <- dTotalImpactScore[,4:6]
TISconstrained <- apply(TISconstrained, c(1,2), function(x){
        if (isTRUE(all.equal(x, 10, tolerance =1e-5)) == TRUE |
            isTRUE(all.equal(x, 5, tolerance =1e-5)) == TRUE |
            isTRUE(all.equal(x, 2, tolerance =1e-5))){
                return(1)
        }
        else {
                return(0)
        }
})

TISconstrained <- as.data.frame(cbind(Sources, TISconstrained))



#Save as .csv
write.csv(SSconstrained, "SSconstrained.csv")
write.csv(TISconstrained, "TISconstrained.csv")

#Significance Class
SC <- as.data.frame(cbind(Sources, pFarmColour$Reference))
write.csv(SC, 'SignificanceClass.csv')

#Table with farms with suboptimal capacity (<95% of total) 1:suboptimal 0:optimal
Suboptimal<- apply(dPercentageofMaxProfit, c(1,2), function(x){
        print(x)
        if (isTRUE(all.equal(x, 100, tolerance=2)) == TRUE ){
                return(0)
        }
        else {
                return(1)
        }
})
Suboptimal <- as.data.frame(cbind(Sources, Suboptimal))

write.csv(Suboptimal, "Suboptimal.csv")
dPercentageofMaxProfit <- as.data.frame(cbind(Sources, dPercentageofMaxProfit))
write.csv(dPercentageofMaxProfit, "PercentageofMaxProfit.csv")

