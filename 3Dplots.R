#3D visualisation trade-off TIS and SS
library(gdxrrw)
library(reshape2)
library(scatterplot3d)
library(RColorBrewer)
library(plot3D)
library(lattice)
library(rgl)
library(ggplot2)
igdx("C:/GAMS/win64/24.7/")

setwd("C:/Users/ddpue/Documents/GPBV Flanders/R")

#Reading GDX data
ImpactScores <- rgdx.param('TIS_SS_combo.gdx', symName = 'pImpactscore', squeeze = FALSE)
TotalImpact <- rgdx.param('TIS_SS_combo.gdx', symName = 'zTotalImpact', squeeze = FALSE)
TotalSignificanceScore <- rgdx.param('TIS_SS_combo.gdx', symName = 'zTotalSignificanceScore', squeeze = FALSE)
PrivateProfit <- rgdx.param('TIS_SS_combo.gdx', symName = 'zPrivateProfit', squeeze = FALSE)
Ammonia <- rgdx.param('TIS_SS_combo.gdx', symName = 'zAmmoniaEmission', squeeze = FALSE)

#Cast ImpactScores
ImpactScores <- dcast(ImpactScores, run~impactscore)

#3Dplot
cols <- rainbow(150)

scatterplot3d(x=ImpactScores$TIS, y=ImpactScores$SS, z=PrivateProfit$zPrivateProfit)
plot<-scatterplot3d(x=ImpactScores$TIS, y=ImpactScores$SS, z=TotalImpact$zTotalImpact)
plot$plane3d(c(0,0,1444))

SS <- c(4, 7, 13)
TIS <- c(12, 7, 6)
impact <- rep(1444, 3)
profit <- PrivateProfit$zPrivateProfit[which(PrivateProfit$run %in% c("r143", "r253", "r491"))]

png('TotalImpact3D.png')
plot<-plot3d(x=ImpactScores$SS, y=ImpactScores$TIS, z=TotalImpact$zTotalImpact, size=3,
             xlab="SS", ylab="TIS", zlab= "")
planes3d(a=0, b=0, c=1, d=-1444, col="grey", alpha=0.4)
points3d(x=SS, y=TIS, z=impact, col="red", size=15)

cloud(TotalImpact$zTotalImpact ~ ImpactScores$SS *  ImpactScores$TIS, drape=TRUE, col.regions=cols,
          xlab = "SS", ylab="TIS", zlab="Total Impact", scales=list(arrows=FALSE))
dev.off()

png('PrivateProfit3D.png', width=800, height = 616)
plot3d(x=ImpactScores$SS, y=ImpactScores$TIS, z=PrivateProfit$zPrivateProfit)
points3d(x=SS, y=TIS, z=profit, col="red", size=15)
wireframe(PrivateProfit$zPrivateProfit ~ ImpactScores$SS *  ImpactScores$TIS, drape=TRUE, col.regions=cols,
          xlab = "SS", ylab="TIS", zlab="Private Profit")
dev.off()

png('Emission3D.png', width=800, height = 616)
plot3d(x=ImpactScores$SS, y=ImpactScores$TIS, z=Ammonia$zAmmoniaEmission)
wireframe(Ammonia$zAmmoniaEmission ~ ImpactScores$SS *  ImpactScores$TIS, drape=TRUE, col.regions=cols,
          xlab = "SS", ylab="TIS", zlab="NH3 emission")
dev.off()

png('TotalSignificanceScore.png', width=800, height = 616)
plot3d(x=ImpactScores$SS, y=ImpactScores$TIS, z=TotalSignificanceScore$zTotalSignificanceScore)
wireframe(TotalSignificanceScore$zTotalSignificanceScore ~ ImpactScores$SS *  ImpactScores$TIS, drape=TRUE, col.regions=cols,
          xlab = "SS", ylab="TIS", zlab="Total Significance Score")
dev.off()

png('ImpactRatio3D.png', width=800, height = 616)
RatioSS_TIS <- TotalSignificanceScore$zTotalSignificanceScore/TotalImpact$zTotalImpact
plot3d(x=ImpactScores$SS, y=ImpactScores$TIS, z=RatioSS_TIS)
wireframe(RatioSS_TIS ~ ImpactScores$SS *  ImpactScores$TIS, drape=TRUE, col.regions=cols,
          xlab = "SS", ylab="TIS", zlab="Impact Ratio")
dev.off()


#One big dataframe
AllData <- as.data.frame(cbind(ImpactScores, TotalImpact, TotalSignificanceScore, PrivateProfit, Ammonia, RatioSS_TIS))
AllData$run <- NULL #Repeat until all columns 'run' are gone
AllData$run <- NULL
AllData$run <- NULL
AllData$run <- NULL
AllData$run <- NULL
colnames(AllData) <- c("SS", "TIS", "TotalImpact", "TotalSignificance", "PrivateProfit", "Ammonia", "ImpactRatio")



TISvector <- c(2,8,14,20)
SSvector <- c(2,10,18,26,34, 40)

ggplot(data=AllData[which(AllData$TIS %in% TISvector),], aes(x=SS, y=ImpactRatio, group=TIS, colour=TIS))+
        geom_line()

ggplot(data=AllData[which(AllData$SS %in% SSvector),], aes(x=TIS, y=ImpactRatio, group=SS, colour=SS))+
        geom_line()

#Subsetting based on policy goals
#1.Global Impact should decrease with at least 25% (max impact: 1926)
SubData <- subset(AllData, isTRUE(all.equal(TotalImpact, 1444.5, tolerance=10000)))
#2. Private profit should at least be 80% of maximum (max profit:228742849)
SubData <- subset(SubData, PrivateProfit>182994279)
#3. Ammonia Emission (and health cost) should decrease with 20% (max emission: 2735622)
SubData <- subset(SubData, Ammonia<2188498)
