#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
igdx("C:/GAMS/win64/24.7/")

#Setwd
setwd("C:/Users/ddpue/Documents/GPBV Flanders/R/")

################################################################################
#Sensitivity Health Cost (0-100€/kg)
################################################################################

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

################################################################################
#Sensitivity CL treshold reference (1-10 % CL in CHC)
################################################################################

#Reading data from .gdx
ClosedFarms <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zClosedFarms', squeeze = FALSE)
ExternalHealthCost <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zExternalHealthCost', squeeze = FALSE)
PercentageMaxProfit <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zPercentageMaxProfit', squeeze = FALSE)
PrivateProfit <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zPrivateProfit', squeeze = FALSE)
TotalImpact <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zTotalImpact', squeeze = FALSE)
TotalProfit <- rgdx.param('SensitivityCLtreshold.gdx', symName = 'zTotalProfit', squeeze = FALSE)

#Plot number of closed farms
ClosedFarms$CLtreshold <- as.integer(sub("r",replacement = "", x = ClosedFarms$run))

ggplot(data=ClosedFarms, aes(x=CLtreshold, y=zClosedFarms, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Number of closed farms - CL treshold 1-10%")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("CL treshold (%)") + ylab("# Closed Farms") 

ggsave("SensitivityCLtreshold_ClosedFarms.png", dpi=400)

#Plot ExternalHealthCost
ExternalHealthCost$CLtreshold <- ClosedFarms$CLtreshold

ggplot(data=ExternalHealthCost, aes(x=CLtreshold, y=zExternalHealthCost, group = scenario, colour = scenario, 
                                    shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Total External Cost - CL treshold 1-10%")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("CL treshold (%)") + ylab("Total External Cost (€)") 

ggsave("SensitivityCLtreshold_TotalExternalCost.png", dpi=400)

#Plot Percentage of Max. Profit
PercentageMaxProfit$CLtreshold <- ClosedFarms$CLtreshold

ggplot(data=PercentageMaxProfit, aes(x=CLtreshold, y=zPercentageMaxProfit, group = scenario, colour = scenario, 
                                     shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Percentage of Max. Profit - CL treshold 1-10%")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("CL treshold (%)") + ylab("% of max Profit") 

ggsave("SensitivityCLtreshold_PercentageOfMaxProfit.png", dpi=400)

#Plot Private Profit
PrivateProfit$CLtreshold <- ClosedFarms$CLtreshold 

ggplot(data=PrivateProfit, aes(x=CLtreshold, y=zPrivateProfit, group = scenario, colour = scenario, 
                               shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Total Private Profit - CL treshold 1-10%")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("CL treshold (%)") + ylab("Total Private Profit (€)") 

ggsave("SensitivityCLtreshold_TotalPrivateProfit.png", dpi = 400)

#Plot Total Impact
TotalImpact$CLtreshold <- ClosedFarms$CLtreshold

ggplot(data=TotalImpact, aes(x=CLtreshold, y=zTotalImpact, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Total Impact on Natura 2000 - CL treshold 1-10%")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("CL treshold (%)") + ylab("Total Impact Score") 

ggsave("SensitivityCLtreshold_TotalImpact.png", dpi = 400)

#Plot Total Profit
TotalProfit$CLtreshold <- ClosedFarms$CLtreshold

ggplot(data=TotalProfit, aes(x=CLtreshold, y=zTotalProfit, group = scenario, colour = scenario, 
                             shape = scenario))+
        geom_line() +
        geom_point()+
        ggtitle("Total Societal Profit - CL treshold 1-10%")+
        theme(plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 10)) +
        xlab("CL treshold (%)") + ylab("Total Societal Profit (€)") 

ggsave("SensitivityCLtreshold_TotalProfit.png", dpi = 400)

################################################################################
#Sensitivity TIS treshold reference (TIS 0.1-400 (step 0.1))
#Looking for intersection with Scenario 2 ('Efficiency')
#and Scenario 7 ('Effectiveness')
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