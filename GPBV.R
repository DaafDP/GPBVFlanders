#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(data.table)
igdx("C:/GAMS/win64/24.7/")

#Clear environment
rm(list = ls())

#Setwd
setwd("C:/Users/ddpue/Documents/GPBV Flanders/R/")

#Inlezen data
Receptor <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/Receptor.txt")
Sources <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/Sources.txt")
Hoedje <- read.csv("~/Regional Model/SpatialOptimization  GPBV Flanders/dataHoedje.csv")
Permits <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/EnvironmentalPermits.txt")
ImpactScores <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/ReceptorSourceEndogenous/ImpactScores.txt", header=FALSE)
colnames(ImpactScores) <- c("source", "type", "score")

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

Receptor$ID <- paste("r", c(1:nrow(Receptor)), sep="")

#Calculate individual dep and IS per pair of source & receptor 
Scores <- apply(Sources, 1, function(x)
{
        print(x[1])
        impacttable <- as.data.frame(matrix(ncol=9, nrow=nrow(Receptor)))
        colnames(impacttable) <- c("receptor", "dep", "CL", "Vd", "IS", "X", "Y", "DD", "ND")
        impacttable$receptor <- Receptor$ID
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
        temp <- impacttable[,1:5]
        temp <- cbind("source" = as.character(rep(x[1],nrow(impacttable))), temp)
        temp[,4:5] <- NULL
        return(temp)
        #return(((cbind(sum(impacttable$IS), (100*max((impacttable$IS)))))))
        
})
DepScores <- rbindlist(Scores, use.names=TRUE)

colnames(Sources) <- c("ID", "X", "Y")


#Bundle all data in GDX format
Deps <- DepScores[,1:3]
colnames(Deps) <- c("i", "j", "value")
Deps$i <- as.factor(Deps$i)
Deps$j <- as.factor(Deps$j)
attr(Deps, "symName") <- "pDep"
attr(Deps, "ts") <- "Indivudal  deposition source x to receptor y"
attr(Deps, "domains") <- c("sFarm", "sReceptor")

IS <- DepScores
IS$dep <- NULL
colnames(IS) <- c("i", "j", "value")
IS$i <- as.factor(IS$i)
IS$j <- as.factor(IS$j)
attr(IS, "symName") <- "pIS"
attr(IS, "ts") <- "Individual  deposition source x to receptor y"
attr(IS, "domains") <- c("sFarm", "sReceptor")


FarmCoordinates <- Sources[,1:3]
FarmCoordinates <- melt(FarmCoordinates)
colnames(FarmCoordinates) <- c("i", "j", "value")
FarmCoordinates$i <- as.factor(FarmCoordinates$i)
FarmCoordinates$j <- as.factor(FarmCoordinates$j)
attr(FarmCoordinates, "symName") <- "pFarmCoord"
attr(FarmCoordinates, "ts") <- "X and Y Lambert Coordinates Sources"
attr(FarmCoordinates, "domains") <- c("sFarm", "sCoordinates")

ReceptorCoordinates <- Receptor[,1:3]
ReceptorCoordinates <- melt(ReceptorCoordinates)
colnames(ReceptorCoordinates) <- c("i", "j", "value")
ReceptorCoordinates$i <- as.factor(ReceptorCoordinates$i)
ReceptorCoordinates$j <- as.factor(ReceptorCoordinates$j)
attr(ReceptorCoordinates, "symName") <- "pReceptorCoord"
attr(ReceptorCoordinates, "ts") <- "X and Y Lambert Coordinates Sources"
attr(ReceptorCoordinates, "domains") <- c("sFarm", "sCoordinates")

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
EmissionFactors$value <- c(0.02, 0.015, 0.33, 3.705, 4.18, 9.12, 1.2, 4, 0.2, 5, 2.38)
EmissionFactors$i <- as.factor(EmissionFactors$i)
attr(EmissionFactors, "symName") <- "pEmissionFactor"
attr(EmissionFactors, "ts") <- "EMAV emission factors for BBT AEAS (kg NH3 per year per place)"
attr(EmissionFactors, "domains") <- "sAnimalCategory"

#Data AMS 2012
BrutoSaldo <- data.frame(c("Broilers", "LayingHens", "AdultCows", "FatteningPigs", "Sows"))
BrutoSaldo$value <- c(2.0017, 7.9723, 1310.21, 54.43, 246.82)
colnames(BrutoSaldo) <- c("i", "value")
BrutoSaldo$i <- as.factor(BrutoSaldo$i)
attr(BrutoSaldo, "symName") <- "pBrutoSaldo"
attr(BrutoSaldo, "ts") <- "Bruto Saldo per animal category, AML 2012"
attr(BrutoSaldo, "domains") <- "ssAnimalCategory"
 
wgdx.lst("C:/Users/ddpue/Documents/GPBV Flanders/GAMS/GPBV.gdx", Deps, IS, FarmCoordinates, 
         ReceptorCoordinates, ImpactScores, 
         AnimalCategory, FarmAnimals, PermitYear, EmissionFactors, BrutoSaldo)

ssAnimalCategory <- data.frame(c("Broilers", "LayingHens", "AdultCows", "FatteningPigs", "Sows", "Piglets" ))
colnames(ssAnimalCategory) <- "i"
attr(ssAnimalCategory, "symName") <- "ssAnimalCategory"
attr(ssAnimalCategory, "ts") <- "animals for which we have bruto saldo"

wgdx.lst("C:/Users/ddpue/Documents/GPBV Flanders/GAMS/BrutoSaldo.gdx", BrutoSaldo, ssAnimalCategory)

#Quit 
q("yes)

