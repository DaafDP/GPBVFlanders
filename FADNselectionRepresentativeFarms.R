#Reading in data
BEL2012 <- read.csv("S:/team_jb/staff/ddpue/FADN/BEL2012.csv")
Permits <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/EnvironmentalPermits.txt")
FarmAnimals <- Permits[c("ID", "Braadkippen", "Leghennen", "Kalkoenen", "Runderen", "Mestvarkens", "Zeugen",
                         "Biggen", "Paarden", "Mestkalveren")]
colnames(FarmAnimals) <- c("ID", "Broilers", "LayingHens", "Turkeys",  "Cows", "FatteningPigs", "Sows",
                           "Piglets", "Horses", "FatteningCalves")
FarmAnimals$AdultCows <- round(0.60 * FarmAnimals$Cows)
FarmAnimals$Cows0to1 <- round(0.20 * FarmAnimals$Cows)
FarmAnimals$Cows1to2 <- round(0.20 * FarmAnimals$Cows)
FarmAnimals$Cows <- NULL

#StandardOutputperAnimalType
#Numbers for Flanders, 2010
SOAnimals <-  as.data.frame(cbind(c("Broilers", "LayingHens", "Turkeys", "FatteningPigs", "Sows", "Piglets", "Horses", "FatteningCalves",
                                    "AdultCows", "Cows0to1", "Cows1to2"), rep(0,11))) 
colnames(SOAnimals) <- c("AnimalCategory", "SO")
SOAnimals$AnimalCategory <- as.character(SOAnimals$AnimalCategory)
SOAnimals$SO <- as.integer(SOAnimals$SO)
SOAnimals[1,2] <- 1087/100 #Poultry - broilers
SOAnimals[2,2] <- 1496/100 #Laying Hens
SOAnimals[3,2] <- 1903/100 #Turkeys
SOAnimals[4,2] <- 212 #Pigs - others
SOAnimals[5,2] <- 916 #Pigs - breeding sows over 50 kg
SOAnimals[6,2] <- 91 #Piglets under 20 kg
SOAnimals[7,2] <- 45 #Equidae 
SOAnimals[8,2] <- 463 #Bovine under one year old - total
SOAnimals[9,2] <- 2411 #Dairy Cows
SOAnimals[10,2] <- 463  #Bovine under one year old - total
SOAnimals[11,2] <- 432  #Bovine 1-2 yrs female


#Calculate SO and assign economic size to GPBV farms
FarmTypes <- apply(FarmAnimals, 1, function(x){
        SO <- (as.integer(x[2:12]) * SOAnimals[,2])
        #Assign Total Standard Output
        TotalSO <- sum(SO)
        #Assign EconomicSize
        EconomicSize <- 0
        if (TotalSO >= 3000000) {
                EconomicSize <- 14
        }
        else if (TotalSO > 1500000 ) {
                EconomicSize <- 13
        }
        else if (TotalSO > 1000000) {
                EconomicSize <- 12
        }
        else if (TotalSO > 750000) {
                EconomicSize <- 11
        }
        else if (TotalSO > 500000) {
                EconomicSize <- 10
        }
        else if (TotalSO > 250000) {
                EconomicSize <- 9
        }
        #Assign Farm Type (TF)
        if ((sum(SO[9:11])/TotalSO) > (2/3)) { #Specialist Dairying
                TF <- 450
        }
        else if ((SO[5]/TotalSO) > (2/3)) { #Specialist pig rearing
                TF <- 511
        }
        else if (((SO[4]+SO[6])/TotalSO) > (2/3)) { #Specialist pig fattening
                TF <- 512
        }
        else if ((sum(SO[4:5])/TotalSO) > (2/3)) { #Pig rearing and fattening combined
                TF <- 513
        }
        else if ((sum(SO[4:5])/TotalSO) > (2/3)) { #Pig rearing and fattening combined
                TF <- 513
        }
        else if ((SO[2]/TotalSO) > (2/3)) { #Specialist layers
                TF <- 521
        }
        else if (((SO[1]+SO[3])/TotalSO) > (2/3)) { #Specialist poultry-meat
                TF <- 522
        }
        else if ((sum(SO[1:3])/TotalSO) > (2/3)) { #Layers  and poultry-meat combined
                TF <- 523
        }
        else if ((sum(SO[1:6])/TotalSO) > (2/3)) { #Various granivores combined
                TF <- 530
        }
        else if (sum(SO[1:6]) > (sum(SO[7:11]))) { 
                if (sum(SO[7:8]) > sum(SO[9:11])) {
                        TF <- 742 #Mixed livestock: granivores and dairying
                }
                else {
                        TF <- 741 #Mixed livestock: granivores and non-dairying grazing livestock
                }
        }
        else if (sum(SO[1:6]) < (sum(SO[7:11]))) { 
                if (sum(SO[7:8]) > sum(SO[9:11])) {
                        TF <- 732 #Mixed livestock, mainly non-dairying grazing livestock
                }
                else {
                        TF <- 731 #Mixed livestock, mainly dairying
                }
        }
        else {
                TF <- 0
        }
        c(SO, TotalSO, EconomicSize, TF) 
})
FarmTypes <- as.data.frame(t(FarmTypes))
colnames(FarmTypes) <- c(paste("SO",SOAnimals$AnimalCategory), "TotalSO", "EconomicSize", "TF")
row.names(FarmTypes) <- paste("s", c(1:nrow(FarmTypes)), sep="")

#Subselect FADN dataset
TFs <- c(450, 511, 512, 513, 521, 522, 523, 530, 731, 741, 742)
Subselection <- subset(BEL2012, BEL2012$A34 %in% TFs)
Subselection <- subset(Subselection, Subselection$A1 == 341) #Only Flanders
table(Subselection$A34) #Frequency table according to Farm Type                   
table(Subselection$A36) #Frequency table according to economic size

#Data frame with number of animals in subselected FADN farms
FADNanimals <- Subselection$D47AV
FADNanimals <- as.data.frame(FADNanimals)
colnames(FADNanimals) <- "broilers"
FADNanimals$layinghens <- Subselection$D48AV
FADNanimals$turkeys <- Subselection$D49AV
FADNanimals$fatteningpigs <- Subselection$D45AV
FADNanimals$sows <- Subselection$D44AV
FADNanimals$piglets <- Subselection$D43AV
FADNanimals$horses <- Subselection$D22AV
FADNanimals$fatteningcalves <- Subselection$D23AV
FADNanimals$adultcows <- Subselection$D28AV
FADNanimals$cows0to1 <- Subselection$D24AV
FADNanimals$cows1to2 <- Subselection$D25AV + Subselection$D26AV


#Identify representable farms from FADN dataset, based on TF (firstly), Occurring animals (secondly) 
#and economic size (thirdly)
RepresentableFarms <- apply(FarmTypes, 1, function(x){
        SelectedFarms <- subset(Subselection, Subselection$A25 == x[14]) #TF
        if (x[1] ! 0) {
                SelectedFarms <- subset(SelectedFarms, )
        }
        
        SelectedFarms <- subset(SelectedFarms, SelectedFarms$A36 == x[13]) #EconomicSize
        return(nrow(SelectedFarms))
})



