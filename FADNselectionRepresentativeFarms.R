#Reading in data
BEL2012 <- read.csv("S:/team_jb/staff/ddpue/FADN/BEL2012.csv")
BEL2011 <- read.csv("S:/team_jb/staff/ddpue/FADN/BEL2011.csv")
Permits <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/EnvironmentalPermits.txt")
FarmAnimals <- Permits[c("ID", "Braadkippen", "Leghennen", "Kalkoenen", "Runderen", "Mestvarkens", "Zeugen",
                         "Biggen", "Paarden", "Mestkalveren")]
colnames(FarmAnimals) <- c("ID", "Broilers", "LayingHens", "Turkeys",  "Cows", "FatteningPigs", "Sows",
                           "Piglets", "Horses", "FatteningCalves")
FarmAnimals$AdultCows <- round(0.60 * FarmAnimals$Cows)
FarmAnimals$Cows0to1 <- round(0.20 * FarmAnimals$Cows)
FarmAnimals$Cows1to2 <- round(0.20 * FarmAnimals$Cows)
FarmAnimals$Cows <- NULL

#Subselect FADN dataset
BEL2012 <- subset(BEL2012, BEL2012$A1 == 341) #Only Flanders
table(BEL2012$A34) #Frequency table according to Farm Type                   
table(BEL2012$A36) #Frequency table according to economic size

#Data frame with number of animals, farm type, total output, total input costs, total specific costs 
FADNselection <- BEL2012$D47AV
FADNselection <- as.data.frame(FADNselection)
colnames(FADNselection) <- "broilers"
FADNselection$layinghens <- BEL2012$D48AV
FADNselection$turkeys <- BEL2012$D49AV
FADNselection$fatteningpigs <- BEL2012$D45AV
FADNselection$sows <- BEL2012$D44AV
FADNselection$piglets <- BEL2012$D43AV
FADNselection$horses <- BEL2012$D22AV
FADNselection$fatteningcalves <- BEL2012$D23AV
FADNselection$adultcows <- BEL2012$D30AV + BEL2012$D31AV + BEL2012$D32AV
FADNselection$cows0to1 <- BEL2012$D24AV
FADNselection$cows1to2 <- BEL2012$D26AV + BEL2012$D25AV 
FADNselection$TOLivestock <- BEL2012$SE206 
FADNselection$TOtotal <- BEL2012$SE131
FADNselection$ValueLivestock <- BEL2012$SE211
FADNselection$OtherLivestock <- BEL2012$SE251
FADNselection$TF <- BEL2012$A25
FADNselection$Sizeclass <- BEL2012$A26
FADNselection$TotalInputCosts <- BEL2012$SE270
FADNselection$TotalSpecificCosts <- BEL2012$SE281
FADNselection$BeefandVeal <- BEL2012$SE220
FADNselection$Poultrymeat <- BEL2012$SE235
FADNselection$Eggs <- BEL2012$SE240
FADNselection$Pigs <- BEL2012$SE225
FADNselection$SpecificLivestockCosts <- BEL2012$SE309 * BEL2012$SE080
FADNselection$GrazingCosts <- BEL2012$SE310 + BEL2012$SE330
FADNselection$PigsPoultryCosts <- BEL2012$SE320 + BEL2012$SE330
FADNselection$TotalCosts <- BEL2012$F64 + BEL2012$F65 + BEL2012$F66 + BEL2012$F67 + BEL2012$F68 
                                + BEL2012$F69 + BEL2012$F70 + BEL2012$F71

##Make data frame with output and costs for all animal categories
EconomicParameters <- colnames(FarmAnimals)
EconomicParameters <- EconomicParameters[2:12]
EconomicParameters <- as.data.frame(EconomicParameters)
EconomicParameters$Output <- 0
EconomicParameters$Costs <- 0

##Regression Total Output Livestock
TOmodel <- lm(TOLivestock ~ broilers + layinghens + turkeys + fatteningpigs + sows + 
                      piglets + horses +fatteningcalves + adultcows +cows0to1 + cows1to2 
              + TF + Sizeclass, data=FADNselection)
capture.output(summary(TOmodel), file="RegressionTOLivestock_All.txt")

#leave out non-significant variables (exception: turkeys), horses and cows0to1 (other valuation methods)
TOmodel <- lm(TOLivestock ~broilers + layinghens + turkeys + fatteningpigs + sows + 
                        piglets + fatteningcalves + adultcows + cows1to2 
                , data=FADNselection)
capture.output(summary(TOmodel), file="RegressionTOLivestock_Subselection.txt")

#Take values from regression model
EconomicParameters$Output[1] <- coef(summary(TOmodel))["broilers", "Estimate"]
EconomicParameters$Output[2] <- coef(summary(TOmodel))["layinghens", "Estimate"]
EconomicParameters$Output[3] <- coef(summary(TOmodel))["turkeys", "Estimate"]
EconomicParameters$Output[4] <- coef(summary(TOmodel))["fatteningpigs", "Estimate"]
EconomicParameters$Output[5] <- coef(summary(TOmodel))["sows", "Estimate"]
EconomicParameters$Output[6] <- coef(summary(TOmodel))["piglets", "Estimate"]
EconomicParameters$Output[8] <- coef(summary(TOmodel))["fatteningcalves", "Estimate"]
EconomicParameters$Output[9] <- coef(summary(TOmodel))["adultcows", "Estimate"]


##Regression value livestock (valuation horses)
TOOthermodel <- lm(OtherLivestock ~ broilers + layinghens + turkeys + fatteningpigs + sows + 
                      piglets + horses +fatteningcalves + adultcows +cows0to1 + cows1to2 
              + TF + Sizeclass, data=FADNselection)
capture.output(summary(TOOthermodel), file = "RegressionOtherLivestock_all.txt")

#Retain significant variables
TOOthermodel <- lm(OtherLivestock ~layinghens + fatteningpigs + 
                           piglets + horses +fatteningcalves, data=FADNselection)
summary(TOOthermodel)
capture.output(summary(TOOthermodel),file= "RegressionOtherLivestock_Subselection.txt")

#Take values from regression model
EconomicParameters$Output[7] <- coef(summary(TOOthermodel))["horses", "Estimate"]

##Regression value cows0to1 and cows1to2
BeefModel <- lm(BeefandVeal ~ broilers + layinghens + turkeys + fatteningpigs + sows + 
                          piglets + horses +fatteningcalves + adultcows +cows0to1 + cows1to2 
                  + TF + Sizeclass, data=FADNselection)
capture.output(summary(BeefModel), file = "RegressionBeefAndVeal_all.txt")

#Retain significant variables
BeefModel <- lm(BeefandVeal ~ adultcows +cows0to1 + cows1to2 +
                 Sizeclass, data=FADNselection)
capture.output(summary(BeefModel), file = "RegressionBeefAndVeal_subselection.txt")

#Take values from regression model
EconomicParameters$Output[10] <- coef(summary(BeefModel))["cows0to1", "Estimate"]
EconomicParameters$Output[11] <- coef(summary(BeefModel))["cows1to2", "Estimate"]


#Regression total inputs costs
#Regression total output livestock and livestock products according to livestock categories
TImodel <- lm(TotalInputCosts ~ broilers + layinghens + turkeys + fatteningpigs + sows + 
                      piglets + horses +fatteningcalves + adultcows +cows0to1 + cows1to2 
              + TF + Sizeclass, data=FADNselection)
summary(TImodel)

##Regression total specific costs
TSCmodel <- lm(SpecificLivestockCosts ~ broilers + layinghens + turkeys + fatteningpigs + sows + 
                                   piglets + horses +fatteningcalves + adultcows +cows0to1 + cows1to2 
                           + TF + Sizeclass, data=FADNselection)
summary(TSCmodel)
capture.output(summary(TSCmodel), file = "RegressionTotalSpecificCosts_all.txt")

#Retain significant variables (+turkeys)
TSCmodel <- lm(SpecificLivestockCosts ~ broilers + layinghens + turkeys + fatteningpigs + sows + 
                       piglets + horses  + adultcows  + cows1to2 
               , data=FADNselection)
summary(TSCmodel)
capture.output(summary(TSCmodel), file = "RegressionTotalSpecificCosts_subselection.txt")

#Take values from regression model
EconomicParameters$Costs[1] <- coef(summary(TSCmodel))["broilers", "Estimate"]
EconomicParameters$Costs[2] <- coef(summary(TSCmodel))["layinghens", "Estimate"]
EconomicParameters$Costs[3] <- coef(summary(TSCmodel))["turkeys", "Estimate"]
EconomicParameters$Costs[4] <- coef(summary(TSCmodel))["fatteningpigs", "Estimate"]
EconomicParameters$Costs[5] <- coef(summary(TSCmodel))["sows", "Estimate"]
EconomicParameters$Costs[6] <- coef(summary(TSCmodel))["piglets", "Estimate"]
EconomicParameters$Costs[8] <- coef(summary(TSCmodel))["fatteningcalves", "Estimate"]
EconomicParameters$Costs[9] <- coef(summary(TSCmodel))["adultcows", "Estimate"]
EconomicParameters$Costs[11] <- coef(summary(TSCmodel))["cows1to2", "Estimate"]

##For remaining animal categories, a subselection is made with all farms containing  that type of animal
#Sows, horses, fatteningcalves, cows0to1
FADNhorsesSelection <- FADNselection[which(FADNselection$horses > 0),]
horsescosts <- lm(TotalCosts ~ horses , data=FADNhorsesSelection)
summary(horsescosts)
capture.output(summary(horsescosts), file="RegressionTotalCosts_horses.txt")

FADNSowsSelection <- FADNselection[which(FADNselection$sows > 0),]
sowscosts <- lm(TotalSpecificCosts ~ sows, data=FADNSowsSelection)
summary(sowscosts)
