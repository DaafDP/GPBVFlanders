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

#Plot number of closed farms
ClosedFarms$TIStreshold <- as.integer(sub("r",replacement = "", x = ClosedFarms$run)) /10

ggplot(data=ClosedFarms, aes(x=TIStreshold, y=zClosedFarms, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line(size=1.0) +
        ggtitle("Number of closed farms - TIS treshold 0.1-40.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("TIS treshold") + ylab("# Closed Farms") 

ggsave("SensitivityTIStreshold_ClosedFarms.png", dpi=400)

#Plot ExternalHealthCost
ExternalHealthCost$TIStreshold <- ClosedFarms$TIStreshold

ggplot(data=ExternalHealthCost, aes(x=TIStreshold, y=zExternalHealthCost, group = scenario, colour = scenario, 
                                    shape = scenario))+
        geom_line(size=1.0) +
        ggtitle("Total External Cost - TIS treshold 0.1-40.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("TIS treshold") + ylab("Total External Cost (€)") 

ggsave("SensitivityTIStreshold_TotalExternalCost.png", dpi=400)

#Plot Percentage of Max. Profit
PercentageMaxProfit$TIStreshold <- ClosedFarms$TIStreshold

ggplot(data=PercentageMaxProfit, aes(x=TIStreshold, y=zPercentageMaxProfit, group = scenario, colour = scenario, 
                                     shape = scenario))+
        geom_line(size=1.0) +
        ggtitle("Percentage of Max. Profit - TIS treshold 0.1-40.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("TIS treshold") + ylab("% of max Profit") 

ggsave("SensitivityTIStreshold_PercentageOfMaxProfit.png", dpi=400)

#Plot Private Profit
PrivateProfit$TIStreshold <- ClosedFarms$TIStreshold 

ggplot(data=PrivateProfit, aes(x=TIStreshold, y=zPrivateProfit, group = scenario, colour = scenario, 
                               shape = scenario))+
        geom_line(size=1.0) +
        ggtitle("Total Private Profit - TIS treshold 0.1-40.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("TIS treshold") + ylab("Total Private Profit (€)") 

ggsave("SensitivityTIStreshold_TotalPrivateProfit.png", dpi = 400)

#Plot Total Impact
TotalImpact$TIStreshold <- ClosedFarms$TIStreshold

ggplot(data=TotalImpact, aes(x=TIStreshold, y=zTotalImpact, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line(size=1.0) +
        ggtitle("Total Impact on Natura 2000 - TIS treshold 0.1-40.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("TIS treshold") + ylab("Total Impact Score") 

ggsave("SensitivityTIStreshold_TotalImpact.png", dpi = 400)

#Plot Total Profit
TotalProfit$TIStreshold <- ClosedFarms$TIStreshold

ggplot(data=TotalProfit, aes(x=TIStreshold, y=zTotalProfit, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line(size=1.0) +
        ggtitle("Total Societal Profit - TIS treshold 0.1-40.0")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("TIS treshold") + ylab("Total Societal Profit (€)") 

ggsave("SensitivityTIStreshold_TotalProfit.png", dpi = 400)