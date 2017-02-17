#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(gridExtra)
igdx("C:/GAMS/win64/24.7/")

#Setwd
setwd("C:/Users/ddpue/Documents/GPBV Flanders/R/")

################################################################################
#Individual TIS constraint 0-70
#What TIS for 10% reduction in Total Impact?
#What TIS for Total Impact equivalent to 'Effectiviness' scenario?
#What TIS for Total Societal profit equivalent to 'Efficiency' scenario?
################################################################################
#Reading data from .gdx
ClosedFarms <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'zClosedFarms', squeeze = FALSE)
ExternalHealthCost <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'zExternalHealthCost', squeeze = FALSE)
PercentageMaxProfit <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'zPercentageMaxProfit', squeeze = FALSE)
PrivateProfit <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'zPrivateProfit', squeeze = FALSE)
TotalImpact <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'zTotalImpact', squeeze = FALSE)
TotalProfit <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'zTotalProfit', squeeze = FALSE)

ClosedFarms2 <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zClosedFarms', squeeze = FALSE)
ExternalHealthCost2 <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zExternalHealthCost', squeeze = FALSE)
PercentageMaxProfit2 <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zPercentageMaxProfit', squeeze = FALSE)
PrivateProfit2 <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zPrivateProfit', squeeze = FALSE)
TotalImpact2 <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zTotalImpact', squeeze = FALSE)
TotalProfit2 <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zTotalProfit', squeeze = FALSE)

OptimalProfit <- rgdx.param('GPBVOptimalProfitvsImpact.gdx', symName = 'zTotalProfit', squeeze = FALSE)
OptimalImpact <- rgdx.param('GPBVOptimalProfitvsImpact.gdx', symName = 'zTotalImpact', squeeze = FALSE)
OptimalImpact$zTotalProfit <- OptimalProfit$zTotalProfit

OptimalImpact <- OptimalImpact[which(OptimalImpact$zTotalImpact<1926),]

ReferenceAmmonia <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'rAmmoniaEmission', squeeze = FALSE)
ReferenceClosedFarms <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'rClosedFarms', squeeze = FALSE)
ReferenceTotalImpact <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'rTotalImpact', squeeze = FALSE)
ReferenceTotalProfit <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'rTotalProfit', squeeze = FALSE)

PrivateProfit$TIStreshold <- as.integer(sub("r",replacement = "", x = ClosedFarms$run)) /10

ReferenceTotalProfit$PrivateProfit <- ReferenceAmmonia$rAmmoniaEmission * 36 + ReferenceTotalProfit$rTotalProfit
ReferenceTotalProfit$ExternalHealth <- ReferenceTotalProfit$PrivateProfit - ReferenceTotalProfit$rTotalProfit

#
#Plot Total Impact
TotalImpact$TIStreshold <- PrivateProfit$TIStreshold

p1 <- ggplot(data=TotalImpact, aes(x=TIStreshold, y=zTotalImpact))+
        geom_line() +
        #ggtitle("Total Impact on Natura 2000 - TIS treshold 0.1-70.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              axis.text = element_text(size = 20)) +
        geom_hline(aes(yintercept=1925.81), colour = "red", lty = "dashed", size=1) +
        annotate("text", 60, 2000, label="Max. Impact", colour="red", size = 7)+
        geom_hline(aes(yintercept=1444.36), colour = "darkgreen", lty = "dashed", size=1) +
        annotate("text", 60, 1530, label="Impact -25%", colour="darkgreen", size=7)+
        geom_vline(aes(xintercept=5.1), colour = "darkgreen", lty="dotted", size=1)+
        annotate("text", 19, 500, label = "TIS constraint 5.1", colour = "darkgreen", size=7)+
        geom_point(x=5.1, y=1444.36, shape = 16, colour = "black", size=7)+
        coord_cartesian(ylim=c(500, 2000))+
        xlab("TIS treshold") + ylab("Total Impact Score") 

ggsave("TIS_TotalImpact.png", dpi = 400)

TotalImpact2$CLtreshold <- as.integer(sub("r",replacement = "", x = TotalImpact2$run)) *0.25 

p2 <- ggplot(data=TotalImpact2, aes(x=CLtreshold, y=zTotalImpact))+
        geom_line() +
        #ggtitle("Total Impact on Natura 2000 - SS treshold 0.25-175")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              axis.text = element_text(size = 20)) +
        geom_hline(aes(yintercept=1925.81), colour = "red", lty = "dashed", size=1) +
        annotate("text", 145, 2000, label="Max. Impact", colour="red", size = 7)+
        geom_hline(aes(yintercept=1444.36), colour = "darkgreen", lty = "dashed", size=1) +
        annotate("text", 145, 1530, label="Impact -25%", colour="darkgreen", size=7)+
        geom_vline(aes(xintercept=3.75), colour = "darkgreen", lty="dotted", size=1)+
        annotate("text", 42, 600, label = "SS constraint 3.75", colour = "darkgreen", size=7)+
        geom_point(x=3.75, y=1444.36, shape = 18, colour = "black", size=7)+
        coord_cartesian(ylim=c(500, 2000))+
        xlab("SS treshold") + ylab("Total Impact Score") 

ggsave("SS_TotalImpact.png", dpi = 400)

grid.arrange(p1, p2, ncol=2)


#Scatterplot_ImpactvsProfit 
#Put all data in one data frame (AllData)
df <- as.data.frame(cbind(TotalProfit$zTotalProfit, TotalImpact$zTotalImpact))
colnames(df) <- c("profit", "impact")
df$scenario <- 'TIS constraint'
df2 <- as.data.frame(cbind(TotalProfit2$zTotalProfit, TotalImpact2$zTotalImpact))
colnames(df2) <- c("profit", "impact")
df2$scenario <- 'SS constraint'
AllData <- as.data.frame(cbind(OptimalImpact[,4], OptimalImpact[,3]))
colnames(AllData) <- c("profit", "impact")
AllData$scenario <- 'Optimal'
AllData <- rbind(AllData, df)
AllData <- rbind(AllData, df2)
colnames(AllData) <- c("profit", "impact", "scenario")

ReferencePoints <- cbind(ReferenceTotalImpact[,2], ReferenceTotalProfit[,2])
colnames(ReferencePoints) <- c("impact", "profit")
rownames(ReferencePoints) <- c("<5% SS", "Max. Profit", "Min. Impact")
ReferencePoints <- as.data.frame(ReferencePoints)

ggplot(dat=AllData, aes(x=impact, y=profit, group=scenario, colour=scenario))+
        scale_fill_manual(breaks=c("Optimal", "TIS constraint", "SS constraint")) +
        #geom_smooth(se=FALSE) +
        geom_line(size=1.0) +
        geom_point(x=1444, y=128691934, shape = 16, colour = "black", size=4)+
        geom_point(x=1444, y=125881648, shape = 17,  colour = "black", size=4)+
        geom_point(x=1444, y=124334589, shape = 18, colour = "black", size=4)+
        #geom_point(dat=ReferencePoints, aes(x=impact, y=profit))+
        ggtitle("Total Societal Benefit versus Total Impact")+
        geom_hline(aes(yintercept=130405978), colour = "red", lty="dotted", size=1)+
        geom_vline(aes(xintercept=1926), colour = "red", lty="dotted", size=1)+
        geom_vline(aes(xintercept=1444), colour="darkgreen", lty="dotted", size =1)+
        annotate("text", 1585, 77000000, label="Impact -25%", colour="darkgreen", size=5)+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size=12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        coord_cartesian(ylim=c(110000000, 131000000), xlim=c(1000, 1926))+
        xlab("Aggregated Impact on Natura 2000 (Total Impact Score)") + ylab("Total Societal Benefit (€)")

        ggsave("ProfitvsTIS_Individualconstraint.png", dpi = 400)
