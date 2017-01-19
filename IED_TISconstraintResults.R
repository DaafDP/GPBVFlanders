#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
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

ReferenceAmmonia <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'rAmmoniaEmission', squeeze = FALSE)
ReferenceClosedFarms <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'rClosedFarms', squeeze = FALSE)
ReferenceTotalImpact <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'rTotalImpact', squeeze = FALSE)
ReferenceTotalProfit <- rgdx.param('SensitivityTIStreshold.gdx', symName = 'rTotalProfit', squeeze = FALSE)

PrivateProfit$TIStreshold <- as.integer(sub("r",replacement = "", x = ClosedFarms$run)) /10

ReferenceTotalProfit$PrivateProfit <- ReferenceAmmonia$rAmmoniaEmission * 36 + ReferenceTotalProfit$rTotalProfit
ReferenceTotalProfit$ExternalHealth <- ReferenceTotalProfit$PrivateProfit - ReferenceTotalProfit$rTotalProfit

#Plot ExternalHealthCost
ExternalHealthCost$TIStreshold <- PrivateProfit$TIStreshold

ggplot(data=ExternalHealthCost, aes(x=TIStreshold, y=zExternalHealthCost))+
        geom_line() +
        ggtitle("Total External Cost - TIS treshold 0.1-70.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        geom_hline(aes(yintercept=ReferenceTotalProfit$ExternalHealth[1]), colour = "red", lty = "dashed") +
        xlab("TIS treshold") + ylab("Total External Cost (€)") 

#Plot Private Profit
ggplot(data=PrivateProfit, aes(x=TIStreshold, y=zPrivateProfit))+
        geom_line() +
        ggtitle("Total Private Profit - TIS treshold 0.1-70.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        geom_hline(aes(yintercept=ReferenceTotalProfit$PrivateProfit[1]), colour = "red", lty = "dashed") +
        xlab("TIS treshold") + ylab("Total Private Profit (€)") 

#Plot Total Profit
TotalProfit$TIStreshold <- PrivateProfit$TIStreshold

ggplot(data=TotalProfit, aes(x=TIStreshold, y=zTotalProfit))+
        geom_line() +
        ggtitle("Total Societal Profit - TIS treshold 0.1-70.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        geom_hline(aes(yintercept=ReferenceTotalProfit$rTotalProfit[1]), colour = "red", lty = "dashed")+
        geom_hline(aes(yintercept=ReferenceTotalProfit$rTotalProfit[2]), colour = "darkgreen", lty = "dashed")+
        xlab("TIS treshold") + ylab("Total Societal Profit (€)") 

ggsave("TIS_TotalPrivateProfit.png", dpi = 400)

#Plot Total Impact
TotalImpact$TIStreshold <- PrivateProfit$TIStreshold

ggplot(data=TotalImpact, aes(x=TIStreshold, y=zTotalImpact))+
        geom_line(size=1.0) +
        ggtitle("Total Impact on Natura 2000 - TIS treshold 0.1-70.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        geom_hline(aes(yintercept=ReferenceTotalImpact$rTotalImpact[1]), colour = "red", lty = "dashed") +
        geom_hline(aes(yintercept=ReferenceTotalImpact$rTotalImpact[3]), colour = "darkgreen", lty = "dashed") +
        xlab("TIS treshold") + ylab("Total Impact Score") 

ggsave("TIS_TotalImpact.png", dpi = 400)

#Scatterplot_ImpactvsProfit 
df <- as.data.frame(cbind(TotalProfit$zTotalProfit, TotalImpact$zTotalImpact, TotalProfit$TIStreshold))
df2 <- as.data.frame(cbind(TotalProfit2$zTotalProfit, TotalImpact2$zTotalImpact))
colnames(df) <- c("profit", "impact", "TIS")
colnames(df2) <- c("profit", "impact")
ggplot(dat=df, aes(x=impact, y=profit))+
        geom_line()+
        geom_line(dat=df2, aes(x=impact, y=profit), col = "red")+
        geom_point(x=ReferenceTotalImpact$rTotalImpact[1], y=ReferenceTotalProfit$rTotalProfit[1], colour = "red")+
        geom_point(x=ReferenceTotalImpact$rTotalImpact[2], y=ReferenceTotalProfit$rTotalProfit[2], colour = "blue")+
        geom_point(x=ReferenceTotalImpact$rTotalImpact[3], y=ReferenceTotalProfit$rTotalProfit[3], colour = "darkgreen")+
        ggtitle("Profit versus Total Impact - Individual TIS constraint 0-70")+
        geom_hline(aes(yintercept=93589597))+
        geom_vline(aes(xintercept=1926))+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        coord_cartesian(ylim=c(66000000, 94000000), xlim=c(500, 2000))+
        xlab("Impact on Natura 2000 (Total Impact Score)") + ylab("Total Societal Profit (€)")

        ggsave("ProfitvsTIS_IndividualTISconstraint.png", dpi = 400)
