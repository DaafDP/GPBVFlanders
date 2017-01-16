#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
igdx("C:/GAMS/win64/24.7/")

#Setwd
setwd("C:/Users/ddpue/Documents/GPBV Flanders/R/")

#Reading data from .gdx
ClosedFarms <- rgdx.param('SensitivityHealthCost.gdx', symName = 'zClosedFarms', squeeze = FALSE)
ExternalHealthCost <- rgdx.param('SensitivityHealthCost.gdx', symName = 'zExternalHealthCost', squeeze = FALSE)
PercentageMaxProfit <- rgdx.param('SensitivityHealthCost.gdx', symName = 'zPercentageMaxProfit', squeeze = FALSE)
PrivateProfit <- rgdx.param('SensitivityHealthCost.gdx', symName = 'zPrivateProfit', squeeze = FALSE)
TotalImpact <- rgdx.param('SensitivityHealthCost.gdx', symName = 'zTotalImpact', squeeze = FALSE)
TotalProfit <- rgdx.param('SensitivityHealthCost.gdx', symName = 'zTotalProfit', squeeze = FALSE)

#Plot number of closed farms
ClosedFarms$HealthCost <- (as.integer(sub("r",replacement = "", x = ClosedFarms$run)) - 1) * 2

ggplot(data=ClosedFarms, aes(x=HealthCost, y=zClosedFarms, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Number of closed farms - Health Cost 0-100€/kg NH3")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("Health Cost (€/kgNH3)") + ylab("# Closed Farms") 

ggsave("SensitivityHealthCost_ClosedFarms.png", dpi=400)

#Plot ExternalHealthCost
ExternalHealthCost$HealthCost <- ClosedFarms$HealthCost[8:357]

ggplot(data=ExternalHealthCost, aes(x=HealthCost, y=zExternalHealthCost, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Total External Cost - Health Cost 0-100€/kg NH3")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("Health Cost (€/kgNH3)") + ylab("Total External Cost (€)") 

ggsave("SensitivityHealthCost_TotalExternalCost.png", dpi=400)

#Plot Percentage of Max. Profit
PercentageMaxProfit$HealthCost <- ClosedFarms$HealthCost

ggplot(data=PercentageMaxProfit, aes(x=HealthCost, y=zPercentageMaxProfit, group = scenario, colour = scenario, 
                                    shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Percentage of Max. Profit - Health Cost 0-100€/kg NH3")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("Health Cost (€/kgNH3)") + ylab("% of max Profit") 

ggsave("SensitivityHealthCost_PercentageOfMaxProfit.png", dpi=400)

#Plot Private Profit
PrivateProfit$HealthCost <- ClosedFarms$HealthCost 

ggplot(data=PrivateProfit, aes(x=HealthCost, y=zPrivateProfit, group = scenario, colour = scenario, 
                                     shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Total Private Profit - Health Cost 0-100€/kg NH3")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("Health Cost (€/kgNH3)") + ylab("Total Private Profit (€)") 

ggsave("SensitivityHealthCost_TotalPrivateProfit.png", dpi = 400)

#Plot Total Impact
TotalImpact$HealthCost <- ClosedFarms$HealthCost 

ggplot(data=TotalImpact, aes(x=HealthCost, y=zTotalImpact, group = scenario, colour = scenario, 
                               shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Total Impact on Natura 2000 - Health Cost 0-100€/kg NH3")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("Health Cost (€/kgNH3)") + ylab("Total Impact Score") 

ggsave("SensitivityHealthCost_TotalImpact.png", dpi = 400)

#Plot Total Profit
TotalProfit$HealthCost <- ClosedFarms$HealthCost 

ggplot(data=TotalProfit, aes(x=HealthCost, y=zTotalProfit, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Total Societal Profit - Health Cost 0-100€/kg NH3")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("Health Cost (€/kgNH3)") + ylab("Total Societal Profit (€)") 

ggsave("SensitivityHealthCost_TotalProfit.png", dpi = 400)
