#Reading in data
BEL2012 <- read.csv("S:/team_jb/staff/ddpue/FADN/BEL2012.csv")
Permits <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/EnvironmentalPermits.txt")

#Selection based on Type of Farm (TF). Specialist pigs, specialist poultry and mixed livestock
TFs <- c(511, 512, 513, 521, 522, 523, 530, 732, 741)
Subselection <- subset(BEL2012, BEL2012$A34 %in% TFs)

#Only Flanders
Subselection <- subset(Subselection, Subselection$A1 == 341)
table(Subselection$A34) #Frequency table according to Farm Type                   
table(Subselection$A36) #Frequency table according to economic size
