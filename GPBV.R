#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
igdx("C:/GAMS/win64/24.5/")

#Clear environment
rm(list = ls())

#Setwd
setwd("C:/Users/ddpue/Documents/GPBV Flanders/R/")

#Inlezen data
Receptor <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/Receptor.txt")
Sources <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/Sources.txt")
Hoedje <- read.csv("~/Regional Model/SpatialOptimization  GPBV Flanders/dataHoedje.csv")
Permits <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/EnvironmentalPermits.txt")

#Vectoren met coordinaten hoedje
X <- c(-199:200) * 100
Y <- X
#hoedje met tabel voor DD en ND afzonderlijk
Hoedje <- Hoedje[,1:5]
colnames(Hoedje) <- c("ID", "X", "Y", "DD", "ND")
hoedjeDD <- as.data.frame(matrix(Hoedje$DD[1:160000], nrow=length(X)))
rownames(hoedjeDD) <- Y
colnames(hoedjeDD) <- X
hoedjeND <- as.data.frame(matrix(Hoedje$ND[1:160000], nrow=length(X)))
rownames(hoedjeND) <- Y
colnames(hoedjeND) <- X

Scores <- apply(Sources, 1, function(x)
{
        print(x[1])
        impacttable <- as.data.frame(matrix(ncol=9, nrow=nrow(Receptor)))
        colnames(impacttable) <- c("ID", "dep", "CL", "Vd", "IS", "X", "Y", "DD", "ND")
        impacttable$ID <- Receptor$ID
        impacttable$CL <- Receptor$CL
        impacttable$Vd <- Receptor$Vd
        impacttable$TND <- Receptor$TND
        #Convert to 'hoedje' coordinates
        impacttable$X <- Receptor$X - as.numeric(x[2])
        impacttable$Y <- Receptor$Y - as.numeric(x[3])
        #Round to 100m
        impacttable$X <- round(impacttable$X/100)*100
        impacttable$Y <- round(impacttable$Y/100)*100
        #Create lookuptable 
        impacttable <- impacttable[which(impacttable$X <= 20000 & impacttable$X >= -19900),]
        impacttable <- impacttable[which(impacttable$Y <= 20000 & impacttable$Y >= -19900),]
        print(nrow(impacttable))
        mat <- apply(impacttable, 1, function(y){
                ycor <- gsub("\\s", "", as.character(y[7]))
                xcor <- gsub("\\s", "", as.character(y[6]))
                DD <- hoedjeDD[ycor, xcor]
                ND <- hoedjeND[ycor, xcor]
                cbind(DD, ND)
                })
        mat <- data.frame(matrix(unlist(mat), nrow=nrow(impacttable),byrow=T))
        head(mat)
        impacttable$DD <- mat$X1
        impacttable$ND <- mat$X2
        impacttable$dep <- (5000/8784)*(((impacttable$Vd/0.88)*impacttable$DD)+impacttable$ND)
        impacttable$IS <- impacttable$dep/impacttable$CL
        TIS <- sum(impacttable$IS)
        SC <- 100 * max(subset(impacttable, TND > CL, select = IS)) #CHC: TDN > CL!!
        return(cbind(TIS, SC))
        #return(((cbind(sum(impacttable$IS), (100*max((impacttable$IS)))))))
        
})

Scores<- as.data.frame(Scores)
Scores <- t(Scores)

Sources$TIS <- Scores[,1]
Sources$SS <- Scores[,2]
colnames(Sources) <- c("ID", "X", "Y", "TIS", "SS")

#Bundle all data in GDX format
FarmCoordinates <- Sources[,1:3]
FarmCoordinates <- melt(FarmCoordinates)
colnames(FarmCoordinates) <- c("i", "j", "value")
FarmCoordinates$i <- as.factor(FarmCoordinates$i)
FarmCoordinates$j <- as.factor(FarmCoordinates$j)
attr(FarmCoordinates, "symName") <- "pFarmCoord"
attr(FarmCoordinates, "ts") <- "X and Y Lambert Coordinates Sources"
attr(FarmCoordinates, "domains") <- c("sFarm", "sCoordinates")

ImpactScores <- Sources[c("ID", "TIS", "SS")]
ImpactScores <- melt(ImpactScores)
colnames(ImpactScores) <- c("i", "j", "value")
ImpactScores$i <- as.factor(ImpactScores$i)
ImpactScores$j <- as.factor(ImpactScores$j)
attr(ImpactScores, "symName") <- "pImpactScores"
attr(ImpactScores, "ts") <- "TIS and SS for emission strength 5000 kg NH3 per year"
attr(ImpactScores, "domains") <- c("sFarm", "sImpactscores")

AnimalCategory <- data.frame(as.factor(c("Broilers", "LayingHens", "Turkeys", "Cows0to1", "Cows1to2", 
                    "AdultCows", "FatteningPigs", "Sows", "Piglets", "Horses", "FatteningCalves")))
colnames(AnimalCategory) <- "i"
attr(AnimalCategory, "symName") <- "sAnimalCategory"
attr(AnimalCategory, "ts") <- "max permitted animals"

FarmAnimals <- Permits[c("ID", "Braadkippen", "Leghennen", "Kalkoenen", "Runderen", "Mestvarkens", "Zeugen",
                         "Biggen", "Paarden", "Mestkalveren")]
colnames(FarmAnimals) <- c("ID", "Broilers", "LayingHens", "Turkeys",  "Cows", "FatteningPigs", "Sows",
                           "Piglets", "Horses", "FatteningCalves")
FarmAnimals$AdultCows <- round(0.60 * FarmAnimals$Cows)
FarmAnimals$Cows0to1 <- round(0.20 * FarmAnimals$Cows)
FarmAnimals$Cows1to2 <- round(0.20 * FarmAnimals$Cows)
FarmAnimals$Cows <- NULL
FarmAnimals <- melt(FarmAnimals)
colnames(FarmAnimals) <- c("i", "j", "value")
FarmAnimals$i <- as.factor(FarmAnimals$i)
FarmAnimals$j <- as.factor(FarmAnimals$j)
attr(FarmAnimals, "symName") <- "pFarmAnimals"
attr(FarmAnimals, "ts") <- "max permitted animals per farm"
attr(FarmAnimals, "domains") <- c("sFarm", "sAnimalCategory")

PermitYear <- Permits[c("ID", "Jaar")]
colnames(PermitYear) <- c("i", "value")
PermitYear$i <- as.factor(PermitYear$i)
attr(PermitYear, "symName") <- "pPermitYear"
attr(PermitYear, "ts") <- "Year in which permit was granted (start of 20 years permit)"
attr(PermitYear, "domains") <- c("sFarm")

EmissionFactors <- data.frame(AnimalCategory)
EmissionFactors$value <- c(0.02, 0.015, 0.33, 3.705, 4.18, 9.12, 1.2, 2.2, 0.2, 5, 2.38)
EmissionFactors$i <- as.factor(EmissionFactors$i)
attr(EmissionFactors, "symName") <- "pEmissionFactor"
attr(EmissionFactors, "ts") <- "EMAV emission factors for BBT AEAS (kg NH3 per year per place)"
attr(EmissionFactors, "domains") <- "sAnimalCategory"
 
wgdx.lst("C:/Users/ddpue/Documents/GPBV Flanders/GAMS/GPBV.gdx", FarmCoordinates, ImpactScores, 
         AnimalCategory, FarmAnimals, PermitYear, EmissionFactors)

#Quit R
q("yes)

