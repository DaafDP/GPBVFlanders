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

Deposition <- 
        apply(Sources, 1, function(x){
                print(x[1])
                DepositionTable <- as.data.frame(matrix(ncol=5, nrow=nrow(Receptor)))
                colnames(DepositionTable) <- c("ID", "X", "Y", "DD", "ND")
                DepositionTable$ID <- Receptor[,1]
                DepositionTable$X <- Receptor$X - as.numeric(x[2])
                DepositionTable$Y <- Receptor$Y - as.numeric(x[3])
                #coordinaten afronden op 100 (aligneren met hoedje)
                DepositionTable$X <- round((DepositionTable$X/100), digits=0) * 100
                DepositionTable$Y <- round((DepositionTable$Y/100), digits=0) * 100
                test <- apply(DepositionTable[,2:3], 1, function(y){
                        #print(y[2])
                        #print(y[3])
                        if (y[2] > 20000 | y[2] <= -19900) {
                                DD <- 0
                                ND <- 0
                        } else if (y[1] > 20000 | y[1] <= -19900) {
                                DD <- 0
                                ND <- 0
                        } else {
                                
                                DD <- hoedjeDD[as.character(y[2]), as.character(y[1])]
                                ND <- hoedjeND[as.character(y[2]), as.character(y[1])]
                        }
                        c(DD,ND)
                })
                #test <- data.frame(matrix(unlist(test), nrow=nrow(DepositionTable),byrow=T))
        })

Deposition <- as.data.frame(Deposition)
colnames(Deposition) <- Sources[,1]
Deposition$j <- rep(paste("r", c(1:nrow(Receptor)), sep=""), each=2)
Deposition$k <- rep(c("DD", "ND"))

#Reshape data frame for export to gdx/GAMS
Deposition <- melt(Deposition)
Deposition <- Deposition[c("variable", "j", "k", "value")]
colnames(Deposition) <- c("i", "j", "k", "value")
Deposition$j <- as.factor(Deposition$j)
Deposition$k <- as.factor(Deposition$k)
attr(Deposition, "symName") <- "pDeposition"
attr(Deposition, "ts") <- "Deposition for 5000 kg NH3/yr hoedje"
attr(Deposition, "domains") <- c("sFarm", "sReceptor", "sDep")
str(Deposition)

#Write Deposition into GDX-file
wgdx.lst("Deposition.gdx", Deposition)

#Quit R
#q("yes)

