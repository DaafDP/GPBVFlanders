*Regional Model version 12
*EMEP ammonia emissions
*Hoedje dataset (IFDM, VITO, 20*20 km², resolutie 100 m)
*Meteo 2011
*Deposition velocities VLOPS


$call GDXXRW dataGPBVFlanders.xlsx   index=index

********************************************************************************
********************************SET INPUT***************************************
********************************************************************************

Sets
*sAnimalCategory
*sEmissionStage
sFarm
*sManureType
sCoordinates
sReceptor
sHabitats
;

$gdxin dataGPBVHouthulst.gdx
$load sFarm, sCoordinates, sReceptor, sHabitats
$gdxin
;

Set
*sSubsetStage(sEmissionStage) /Yard, Building, Grazing/
sDep /DD, ND/
;

********************************************************************************
**************************PARAMETERS INPUT**************************************
********************************************************************************

Parameters
*pManureType(sAnimalCategory, sManureType, sFarm) Proportion of manure handled as liquid slurry or solid
*pNex(sAnimalCategory) Annual N excretion of animal (table 3.7)
*pTimeSpent(sSubsetStage, sAnimalCategory) Proportion of time spent on certain stage (possibly variable)
*pTAN(sAnimalCategory) Proportion of TAN in total N excretion  (table 3.7)
*pAmmoniaEF(sAnimalCategory, sManureType, sEmissionStage) Default emission factors ammonia (table 3.7)
*pStrawAnimal(sAnimalCategory) (table 3.5)
*pNinStraw(sAnimalCategory) (table 3.5)
*pStored(sManureType) Proportion of manure stored before application (possibly variable)
*pLaughingGasEF(sAnimalCategory, sManureType) Default emission factor laughing gas (table 3.6)
*pNitrogenOxideEF(sManureType) Default emission factor nitrogen oxide (table 3.8)
*pNitrogenGasEF(sManureType) Default emission factor nitrogen gas (table 3.8)
*pAnimalCategory(sAnimalCategory, sFarm) Type of animals per farm
*pEmissionType(sAnimalCategory, sManureType, sEmissionStage) Allowed emission stage depending on animal type and manure type
*pYard(sAnimalCategory) Excretion on Yard
*pStrawMass(sAnimalCategory) Straw added per animal
pFarmCoord(sFarm, sCoordinates) X and Y Lambert Coordinates Sources
pReceptorCoord(sReceptor, sCoordinates) X and Y Lambert Coordinates Receptor
pHabitats(sReceptor, sHabitats) Habitat characteristics (CL - habitat type - Vd )
;

$gdxin dataGPBVFlanders.gdx
$load pFarmCoord, pReceptorCoord, pHabitats
$gdxin

$ontext
Scalars
pFimm Fraction of TAN immobilized in organic matter when manure is managed as solid /0.0067/
pFmin Fraction of organic N mineralised to TAN before gaseous emissions /0.1/
;

$offtext

********************************************************************************
********************************R hoedje****************************************
********************************************************************************
*Linking source/receptor with right data from 'hoedje'

*Running R code to get calibration set
*$setglobal R "C:\Users\ddpue\Documents\R\R-3.2.2\bin\i386\R.exe"
*$setglobal Rfile RegionalModelDeposition.R

*$call "=%R% --rhome=%system.fp% CMD BATCH %rfile% %rfile%.log"

*Load in parameter pDeposition in GAMS
Parameter pDeposition(sFarm, sReceptor, sDep) ;

$gdxin Deposition.gdx
$load pDeposition
$gdxin


$ontext
********************************************************************************
********************************************************************************
********************************EMEP *******************************************
********************************************************************************
********************************************************************************

*Ref: 1. Webb J, Hutchings N, Amon B. Manure Management. EMEP/EEA Emiss Invent Guideb 2013. 2013;(July):1-14.

********************************************************************************
**************************Variables*********************************************
********************************************************************************

Variable
vAmmoniaEmissionRegion See Step 15
vAmmoniaEmissionFarm(sFarm) See Step 15
;

********************************************************************************
**************************Equations EMEP****************************************
********************************************************************************

Equations
eqTotalExcretion(sAnimalCategory, sFarm) See Step 2
eqAnnualNdepositedBuilding(sAnimalCategory, sSubsetStage, sFarm) See Step 3
eqAnnualNdepositedGrazing(sAnimalCategory, sSubsetStage, sFarm) See Step 3
eqAnnualNdepositedYard(sAnimalCategory, sManureType, sFarm) See Step 3
eqAnnualTANdeposited(sAnimalCategory, sSubsetStage, sFarm) See Step 4
eqAnnualNBuildingManureType(sAnimalCategory, sFarm, sManureType) See Step 5
eqAnnualTANBuildingManureType(sAnimalCategory, sFarm, sManureType) See Step 5
eqAmmoniaEmissionBuilding(sAnimalCategory, sFarm, sManureType, sEmissionStage) Step 6
eqAmmoniaEmissionYard(sAnimalCategory, sFarm, sManureType, sEmissionStage) Step 6
eqOutBuildingManureTAN(sAnimalCategory, sFarm, sManureType) See Step 7
eqOutBuildingManureN(sAnimalCategory, sFarm, sManureType) See Step 7
eqStorageTANslurry(sAnimalCategory, sFarm, sManureType) See Step 8
eqStorageNslurry(sAnimalCategory, sFarm, sManureType) See Step 8
eqSpreadingTANslurry(sAnimalCategory, sFarm, sManureType) See Step 8
eqSpreadingNslurry(sAnimalCategory, sFarm, sManureType) See Step 8
eqStorageTANsolid(sAnimalCategory, sFarm, sManureType) See Step 8
eqStorageNsolid(sAnimalCategory, sFarm, sManureType) See Step 8
eqSpreadingTANsolid(sAnimalCategory, sFarm, sManureType) See Step 8
eqSpreadingNsolid(sAnimalCategory, sFarm, sManureType) See Step 8
eqModifiedMassStorageTANslurry(sAnimalCategory, sFarm) See Step 9
eqAmmonniaEmissionSlurryStorage(sAnimalCategory, sFarm, sManureType, sEmissionStage) See Step 10
eqAmmonniaEmissionSolidStorage(sAnimalCategory, sFarm, sManureType, sEmissionStage) See Step 10
eqEmissionsNStorageSlurry(sAnimalCategory, sFarm, sManureType) See Step 10
eqEmissionsNStorageSolid(sAnimalCategory, sFarm, sManureType) See Step 10
eqApplicationTANslurry(sAnimalCategory, sFarm, sManureType) See Step 11
eqApplicationNslurry(sAnimalCategory, sFarm, sManureType) See Step 11
eqApplicationTANsolid(sAnimalCategory, sFarm, sManureType) See Step 11
eqApplicationNsolid(sAnimalCategory, sFarm, sManureType) See Step 11
eqAmmoniaEmissionSlurryApplication(sAnimalCategory, sFarm, sManureType, sEmissionStage) See Step 12
eqAmmoniaEmissionSolidApplication(sAnimalCategory, sFarm, sManureType, sEmissionStage) See Step 12
eqReturnedTANslurry(sAnimalCategory, sFarm, sManureType) See Step 13
eqReturnedNslurry(sAnimalCategory, sFarm, sManureType) See Step 13
eqReturnedTANsolid(sAnimalCategory, sFarm, sManureType) See Step 13
eqReturnedNsolid(sAnimalCategory, sFarm, sManureType) See Step 13
eqAmmoniaEmissionGrazing(sAnimalCategory, sFarm, sManureType, sEmissionStage) See Step 14
eqReturnedNGrazing(sAnimalCategory, sFarm) See Step 14
eqReturnedTANGrazing(sAnimalCategory, sFarm) See Step 14
eqAmmoniaEmissionFarm(sFarm) See Step 15
eqAmmoniaEmissionRegion See Step 15
eqInput
eqOutput
eqMassBalance
;

Positive Variables
vTotalExcretion(sAnimalCategory, sFarm) KgN per yr
vAnnualNdeposited(sAnimalCategory, sSubsetStage, sFarm) KgN per yr
vAnnualTANdeposited(sAnimalCategory, sSubsetStage, sFarm) KgN per yr
vAnnualNBuildingManureType(sAnimalCategory, sFarm, sManureType) KgN per yr
vAnnualTANBuildingManureType(sAnimalCategory, sFarm, sManureType) KgN per yr
vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage) KgNH3-N per yr
vOutBuildingManureTAN(sAnimalCategory, sFarm, sManureType) KgN per yr
vOutBuildingManureN(sAnimalCategory, sFarm, sManureType) KgN per yr
vStorageTAN(sAnimalCategory, sFarm, sManureType) KgN per yr
vStorageN(sAnimalCategory, sFarm, sManureType) KgN per yr
vSpreadingTAN(sAnimalCategory, sFarm, sManureType) KgN per yr
vSpreadingN(sAnimalCategory, sFarm, sManureType) KgN per yr
vModifiedMassStorageTANslurry(sAnimalCategory, sFarm) KgN per yr
vEmissionsNStorage(sAnimalCategory, sFarm, sManureType) KgN per yr
vApplicationTAN(sAnimalCategory, sFarm, sManureType) KgN per yr
vApplicationN(sAnimalCategory, sFarm, sManureType) KgN per yr
vReturnedTAN(sAnimalCategory, sFarm, sManureType) KgN per yr
vReturnedN(sAnimalCategory, sFarm, sManureType) KgN per yr
vMassBalance should be zero
;


********************************************************************************
****************************Type of Animals*************************************
********************************************************************************
Positive Variable vNumberOfAnimals(sAnimalCategory, sFarm) ;

Equation
eqTypeOfAnimals(sAnimalCategory, sFarm)
eqTypeofAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage)
;

*Set types of animals per farms
eqTypeOfAnimals(sAnimalCategory, sFarm)..
vNumberOfAnimals(sAnimalCategory, sFarm) =e= vNumberOfAnimals(sAnimalCategory, sFarm) * pAnimalCategory(sAnimalCategory, sFarm)
;

eqTypeOfAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage) =e= vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage) * pAnimalCategory(sAnimalCategory, sFarm)
;

Equation
EqEmissionType(sAnimalCategory, sFarm, sManureType, sEmissionStage)
;

EqEmissionType(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage) =e= vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage) * pEmissionType(sAnimalCategory, sManureType, sEmissionStage)
;

********************************************************************************
****************************STEP2***********************************************
********************************************************************************
**Calculation of total excretion of N by animals

eqTotalExcretion(sAnimalCategory, sFarm)..
vTotalExcretion(sAnimalCategory, sFarm) =e= vNumberOfAnimals(sAnimalCategory, sFarm) * pNex(sAnimalCategory)
;

********************************************************************************
****************************STEP3***********************************************
********************************************************************************
**Calculation amount of annual N deposited in Buildings, Yards and Grazing

eqAnnualNdepositedBuilding(sAnimalCategory, sSubsetStage, sFarm)..
vAnnualNdeposited(sAnimalCategory, 'Building', sFarm) =e=  pTimeSpent('Building', sAnimalCategory) * vTotalExcretion(sAnimalCategory, sFarm)*(1-pYard(sAnimalCategory))
;

eqAnnualNdepositedGrazing(sAnimalCategory, sSubsetStage, sFarm)..
vAnnualNdeposited(sAnimalCategory, 'Grazing', sFarm) =e=  pTimeSpent('Grazing', sAnimalCategory) * vTotalExcretion(sAnimalCategory, sFarm)*(1-pYard(sAnimalCategory))
;

eqAnnualNdepositedYard(sAnimalCategory, sManureType, sFarm)..
vAnnualNdeposited(sAnimalCategory, 'Yard', sFarm) =e= vTotalExcretion(sAnimalCategory, sFarm) * pYard(sAnimalCategory)
;


********************************************************************************
****************************STEP4***********************************************
********************************************************************************
**Calculation amount of annual TAN deposited in Buildings, Yards and Grazing

eqAnnualTANdeposited(sAnimalCategory, sSubsetStage, sFarm)..
vAnnualTANdeposited(sAnimalCategory, sSubsetStage, sFarm) =e= vAnnualNdeposited(sAnimalCategory, sSubsetStage, sFarm) * pTAN(sAnimalCategory)
;

********************************************************************************
****************************STEP5***********************************************
********************************************************************************
**Amounts of TAN and total N deposited in buildings handled as liquid or solid

eqAnnualNBuildingManureType(sAnimalCategory, sFarm, sManureType)..
vAnnualNBuildingManureType(sAnimalCategory, sFarm, sManureType) =e= vAnnualNdeposited(sAnimalCategory, 'Building', sFarm) * pManureType(sAnimalCategory, sManureType, sFarm)
;

eqAnnualTANBuildingManureType(sAnimalCategory, sFarm, sManureType)..
vAnnualTANBuildingManureType(sAnimalCategory, sFarm, sManureType) =e= vAnnualTANdeposited(sAnimalCategory, 'Building', sFarm) * pManureType(sAnimalCategory, sManureType, sFarm)
;

********************************************************************************
****************************STEP6***********************************************
********************************************************************************
**Ammonia Emissions from building and yard

eqAmmoniaEmissionBuilding(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, 'Building') =e= vAnnualTANBuildingManureType(sAnimalCategory, sFarm, sManureType) * pAmmoniaEF(sAnimalCategory, sManureType, 'Building')
;

eqAmmoniaEmissionYard(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Yard') =e= vAnnualTANdeposited(sAnimalCategory, 'Yard', sFarm) * pAmmoniaEF(sAnimalCategory, 'Solid', 'Yard')
;

********************************************************************************
****************************STEP7***********************************************
********************************************************************************
**Allow for the addition of N in bedding of the animals

Variables
vStraw(sAnimalCategory, sFarm)
vStrawTotal(sAnimalCategory, sFarm)
;

Equations
eqStraw(sAnimalCategory, sFarm)
eqStrawTotal(sAnimalCategory, sFarm)
;

eqStraw(sAnimalCategory, sFarm)..
vStraw(sAnimalCategory, sFarm) =e= vNumberOfAnimals(sAnimalCategory, sFarm) * pNinStraw(sAnimalCategory) * pManureType(sAnimalCategory, 'Solid', sFarm)
;

eqStrawTotal(sAnimalCategory, sFarm)..
vStrawTotal(sAnimalCategory, sFarm) =e= vNumberOfAnimals(sAnimalCategory, sFarm) * pStrawMass(sAnimalCategory) * pManureType(sAnimalCategory, 'Solid', sFarm)
;

eqOutBuildingManureTAN(sAnimalCategory, sFarm, sManureType)..
vOutBuildingManureTAN(sAnimalCategory, sFarm, 'Solid') =e= vAnnualTANBuildingManureType(sAnimalCategory, sFarm, 'Solid') - (vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Building')+(vStrawTotal(sAnimalCategory,sFarm)*pFimm))
;

eqOutBuildingManureN(sAnimalCategory, sFarm, sManureType)..
vOutBuildingManureN(sAnimalCategory, sFarm, 'Solid') =e= vAnnualNBuildingManureType(sAnimalCategory, sFarm, 'Solid') + vStraw(sAnimalCategory, sFarm)- vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Building')
;

********************************************************************************
****************************STEP8***********************************************
********************************************************************************
**Calculate N and TAN stored before application on land

eqStorageTANslurry(sAnimalCategory, sFarm, sManureType)..
vStorageTAN(sAnimalCategory, sFarm, 'Slurry') =e= ((vAnnualTANBuildingManureType(sAnimalCategory, sFarm, 'Slurry') + vAnnualTANdeposited(sAnimalCategory, 'Yard', sFarm))- (vAmmoniaEmission(sAnimalCategory, sFarm, 'Slurry', 'Building') + vAmmoniaEmission(sAnimalCategory, sFarm, 'solid', 'Yard')))*pStored('Slurry')
;

eqStorageNslurry(sAnimalCategory, sFarm, sManureType)..
vStorageN(SAnimalCategory, sFarm, 'Slurry') =e= (vAnnualNBuildingManureType(sAnimalCategory, sFarm, 'Slurry') - vAmmoniaEmission(sAnimalCategory, sFarm, 'Slurry', 'Building') + vAnnualNdeposited(sAnimalCategory, 'Yard', sFarm) - vAmmoniaEmission(sAnimalCategory, sFarm, 'solid', 'Yard'))* pStored('Slurry')
;

eqSpreadingTANslurry(sAnimalCategory, sFarm, sManureType)..
vSpreadingTAN(sAnimalCategory, sFarm, 'Slurry') =e= (vAnnualTANBuildingManureType(sAnimalCategory, sFarm, 'Slurry') - vAmmoniaEmission(sAnimalCategory, sFarm, 'Slurry', 'Building') + vAnnualTANdeposited(sAnimalCategory, 'Yard', sFarm) - vAmmoniaEmission(sAnimalCategory, sFarm, 'solid', 'Yard'))* (1- pStored('Slurry'))
;

eqSpreadingNslurry(sAnimalCategory, sFarm, sManureType)..
vSpreadingN(sAnimalCategory, sFarm, 'Slurry') =e= (vAnnualNBuildingManureType(sAnimalCategory, sFarm, 'Slurry') - vAmmoniaEmission(sAnimalCategory, sFarm, 'Slurry', 'Building') + vAnnualNdeposited(sAnimalCategory, 'Yard', sFarm) - vAmmoniaEmission(sAnimalCategory, sFarm, 'solid', 'Yard'))* (1- pStored('Slurry'))
;

eqStorageTANsolid(sAnimalCategory, sFarm, sManureType)..
vStorageTAN(sAnimalCategory, sFarm, 'Solid') =e= vOutBuildingManureTAN(sAnimalCategory, sFarm, 'Solid') * pStored('Solid')
;

eqStorageNsolid(sAnimalCategory, sFarm, sManureType)..
vStorageN(sAnimalCategory, sFarm, 'Solid') =e= vOutBuildingManureN(sAnimalCategory, sFarm, 'Solid') * pStored('Solid')
;

eqSpreadingTANsolid(sAnimalCategory, sFarm, sManureType)..
vSpreadingTAN(sAnimalCategory, sFarm, 'Solid') =e= vOutBuildingManureTAN(sAnimalCategory, sFarm, 'Solid') * (1 - pStored('Solid'))
;

eqSpreadingNsolid(sAnimalCategory, sFarm, sManureType)..
vSpreadingN(sAnimalCategory, sFarm, 'Solid') =e= vOutBuildingManureN(sAnimalCategory, sFarm, 'Solid') * (1 - pStored('Solid'))
;


********************************************************************************
****************************STEP9***********************************************
********************************************************************************
**Calculate amount of TAN from which emissions will occur from slurry stores

eqModifiedMassStorageTANslurry(sAnimalCategory, sFarm)..
vModifiedMassStorageTANslurry(sAnimalCategory, sFarm) =e= vStorageTAN(sAnimalCategory, sFarm, 'Slurry') + (((vStorageN(SAnimalCategory, sFarm, 'Slurry') - vStorageTAN(sAnimalCategory, sFarm, 'Slurry'))*pFmin))
;

********************************************************************************
****************************STEP10**********************************************
********************************************************************************
**Calculate emissions of ammonia, laughing gas, nitrogen oxide and nitrogen gas from storage

eqAmmonniaEmissionSlurryStorage(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, 'Slurry', 'Storage') =e= vModifiedMassStorageTANslurry(sAnimalCategory, sFarm) * pAmmoniaEF(sAnimalCategory, 'Slurry', 'Storage')
;

eqAmmonniaEmissionSolidStorage(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Storage') =e= vStorageTAN(sAnimalCategory, sFarm, 'Solid') * pAmmoniaEF(sAnimalCategory, 'Solid', 'Storage')
;

eqEmissionsNStorageSlurry(sAnimalCategory, sFarm, sManureType)..
vEmissionsNStorage(sAnimalCategory, sFarm, 'Slurry') =e=  vModifiedMassStorageTANslurry(sAnimalCategory, sFarm) * (pAmmoniaEF(sAnimalCategory, 'Slurry', 'Storage') + pLaughingGasEF(sAnimalCategory, 'Slurry') + pNitrogenOxideEF('Slurry') + pNitrogenGasEF('Slurry'))
;

eqEmissionsNStorageSolid(sAnimalCategory, sFarm, sManureType)..
vEmissionsNStorage(sAnimalCategory, sFarm, 'Solid') =e= vStorageTAN(sAnimalCategory, sFarm, 'Solid') * (pAmmoniaEF(sAnimalCategory, 'Solid', 'Storage') + pLaughingGasEF(sAnimalCategory, 'Solid') + pNitrogenOxideEF('Solid') + pNitrogenGasEF('Solid'))
;

********************************************************************************
****************************STEP11**********************************************
********************************************************************************
**Calculate N and TAN that is applied to the field

eqApplicationTANslurry(sAnimalCategory, sFarm, sManureType)..
vApplicationTAN(sAnimalCategory, sFarm, 'Slurry') =e= vSpreadingTAN(sAnimalCategory, sFarm, 'Slurry') + vModifiedMassStorageTANslurry(sAnimalCategory, sFarm) - vEmissionsNStorage(sAnimalCategory, sFarm, 'Slurry')
;

eqApplicationNslurry(sAnimalCategory, sFarm, sManureType)..
vApplicationN(sAnimalCategory, sFarm, 'Slurry') =e= vSpreadingN(sAnimalCategory, sFarm, 'Slurry') + vStorageN(SAnimalCategory, sFarm, 'Slurry') - vEmissionsNStorage(sAnimalCategory, sFarm, 'Slurry')
;

eqApplicationTANsolid(sAnimalCategory, sFarm, sManureType)..
vApplicationTAN(sAnimalCategory, sFarm, 'Solid') =e= vSpreadingTAN(sAnimalCategory, sFarm, 'Solid') + vStorageTAN(sAnimalCategory, sFarm,'Solid') - vEmissionsNStorage(sAnimalCategory, sFarm, 'Solid')
;

eqApplicationNsolid(sAnimalCategory, sFarm, sManureType)..
vApplicationN(sAnimalCategory, sFarm, 'Solid') =e= vSpreadingN(sAnimalCategory, sFarm, 'Solid') + vStorageN(sAnimalCategory, sFarm, 'Solid') - vEmissionsNStorage(sAnimalCategory, sFarm, 'Solid')
;

********************************************************************************
****************************STEP12**********************************************
********************************************************************************
**Calculate emission following application to field

eqAmmoniaEmissionSlurryApplication(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, 'Slurry', 'Application') =e= vApplicationTAN(sAnimalCategory, sFarm, 'Slurry') * pAmmoniaEF(sAnimalCategory, 'Slurry', 'Application')
;

eqAmmoniaEmissionSolidApplication(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Application') =e= vApplicationTAN(sAnimalCategory, sFarm, 'Solid') * pAmmoniaEF(sAnimalCategory, 'Solid', 'Application')
;

********************************************************************************
****************************STEP13**********************************************
********************************************************************************
**Calculate total-N and TAN returned to soil

eqReturnedTANslurry(sAnimalCategory, sFarm, sManureType)..
VReturnedTAN(sAnimalCategory, sFarm, 'Slurry') =e= vApplicationTAN(sAnimalCategory, sFarm, 'Slurry') - vAmmoniaEmission(sAnimalCategory, sFarm, 'Slurry', 'Application')
;

eqReturnedNslurry(sAnimalCategory, sFarm, sManureType)..
vReturnedN(sAnimalCategory, sFarm, 'Slurry') =e= vApplicationN(sAnimalCategory, sFarm, 'Slurry') - vAmmoniaEmission(sAnimalCategory, sFarm, 'Slurry', 'Application')
;

eqReturnedTANsolid(sAnimalCategory, sFarm, sManureType)..
vReturnedTAN(sAnimalCategory, sFarm, 'Solid') =e= vApplicationTAN(sAnimalCategory, sFarm, 'Solid') - vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Application')
;

eqReturnedNsolid(sAnimalCategory, sFarm, sManureType)..
vReturnedN(sAnimalCategory, sFarm, 'Solid') =e= vApplicationN(sAnimalCategory, sFarm, 'Solid') - vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Application')
;

********************************************************************************
****************************STEP14**********************************************
********************************************************************************
**Calculate emissions from grazing

eqAmmoniaEmissionGrazing(sAnimalCategory, sFarm, sManureType, sEmissionStage)..
vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Grazing') =e= vAnnualTANdeposited(sAnimalCategory, 'Grazing', sFarm) * pAmmoniaEF(sAnimalCategory, 'Solid', 'Grazing')
;

Positive Variables
vReturnedGrazingTAN(sAnimalCategory, sFarm)
vReturnedGrazingN(sAnimalCategory, sFarm);

eqReturnedTANGrazing(sAnimalCategory, sFarm)..
vReturnedGrazingTAN(sAnimalCategory, sFarm) =e= vAnnualTANdeposited(sAnimalCategory, 'Grazing', sFarm) - vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Grazing')
;

eqReturnedNGrazing(sAnimalCategory, sFarm)..
vReturnedGrazingN(sAnimalCategory, sFarm) =e= vAnnualNdeposited(sAnimalCategory, 'Grazing', sFarm) - vAmmoniaEmission(sAnimalCategory, sFarm, 'Solid', 'Grazing')
;


********************************************************************************
****************************STEP15**********************************************
********************************************************************************
**Calculate total ammonia emission (kg NH3-N/yr)

eqAmmoniaEmissionFarm(sFarm)..
vAmmoniaEmissionFarm(sFarm) =e= SUM((sAnimalCategory, sManureType, sEmissionStage), vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage))
;

$offtext
Variable
vAmmoniaEmissionRegion
vAmmoniaEmissionFarm(sFarm)
;


Equation
eqAmmoniaEmissionRegion ;

*Objective
eqAmmoniaEmissionRegion..
vAmmoniaEmissionRegion =e= SUM(sFarm,vAmmoniaEmissionFarm(sFarm))
*Alternative equation for scenario 3:
*SUM(sFarm,vAmmoniaEmissionFarm(sFarm)) =g= 24670
;

$ontext
********************************************************************************
****************************MassBalanceCheck************************************
********************************************************************************

Variables
vMassBalanceN
vNinput
vNoutput
;

eqInput..
vNinput =e= SUM((sAnimalCategory, sSubsetStage, sFarm), vAnnualNdeposited(sAnimalCategory, sSubsetStage, sFarm))
         + SUM((sAnimalCategory, sFarm),vStraw(sAnimalCategory, sFarm))
;

eqOutput..
vNoutput =e= SUM((sAnimalCategory, sFarm, sManuretype), vReturnedN(sAnimalCategory, sFarm, sManureType))
          + SUM((sAnimalCategory, sFarm, sManureType, sEmissionStage), vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, sEmissionStage))
          + SUM((sAnimalCategory, sFarm, sManureType), vEmissionsNStorage(sAnimalCategory, sFarm, sManureType))
          - SUM((sAnimalCategory, sFarm, sManureType), vAmmoniaEmission(sAnimalCategory, sFarm, sManureType, 'Storage'))
          + SUM((sAnimalCategory, sFarm), vReturnedGrazingN(sAnimalCategory, sFarm))
;

eqMassBalance..
vMassBalanceN =e= vNinput - vNOutput
;

$offtext

********************************************************************************
********************************************************************************
********************************Atmospheric Dispersion**************************
********************************************************************************
********************************************************************************
*Hoedje IFDM

Equation
eqDeposition(sFarm, sReceptor) depostion of ammonia coming from farm i on nature j
eqAmmoniaCeiling(sFarm)
;

*vAmmoniaEmissionFarm.scale(sFarm) = 10000;

Positive Variable
         vDep(sFarm, sReceptor) kgN per ha per year

;

*Maximally permitted ammonia emission per farm: 15 000 kg NH3 per year
eqAmmoniaCeiling(sFarm).. vAmmoniaEmissionFarm(sFarm) =l= 25000           ;

*Omzetting emissie kg N-NH3/jr naar Kg NH3/jr --> * (17/14)

eqDeposition(sFarm, sReceptor)$(pDeposition(sFarm, sReceptor, "DD") ne 0)..
vDep(sFarm, sReceptor)   =e= (((vAmmoniaEmissionFarm(sFarm)*(17/14))/8784) * ((pHabitats(sReceptor, "Vd")/0.88)*pDeposition(sFarm, sReceptor, "DD")+pDeposition(sFarm, sReceptor, "ND")))
;

********************************************************************************
********************************************************************************
********************************Scenario Analysis*******************************
********************************************************************************
********************************************************************************

**Scenario 1: <3% CL in  KHC (Reference)
$ontext

Equations
eqContribution(sFarm, sReceptor)      deposition of farm i on nature j
*eqTotalDeposition(sReceptor)     deposition nature j
;


eqContribution(sFarm,sReceptor)$(pDeposition(sFarm, sReceptor, "DD") ne 0) ..     vDep(sFarm, sReceptor)  =l=  0.03 *pHabitats(sReceptor, 'KDW') ;

*eqTotalDeposition(sReceptor) ..    SUM(sFarm, vDep(sFarm, sReceptor)) + 20 =l= 2*pHabitats(sReceptor, 'KDW') ;





**Scenario 2: Efficiency check: Total Impact max. 2847, max. vAmmoniaEmisison, no individual farm constraints
Equations
*eqTotalDeposition(sReceptor)     deposition nature j
eqTotalImpact
;

*eqTotalDeposition(sReceptor) ..    SUM(sFarm, vDep(sFarm, sReceptor)) + 20 =l= 2*pHabitats(sReceptor, 'KDW') ;
eqTotalImpact.. sum(sFarm, SUM(sReceptor$(pDeposition(sFarm, sReceptor, "DD") ne 0), vDep(sFarm, sReceptor)/pHabitats(sReceptor, 'KDW'))) =l= 3348.77 ;



**Scenario 3: Same as scenario 2 (ceiling average deposition), but wich individual ceiling of 10%CL
Equations
*eqTotalDeposition(sReceptor)     deposition nature j
eqContribution(sFarm, sReceptor)      deposition of farm i on nature j
eqTotalImpact
;


*eqTotalDeposition(sReceptor) ..    SUM(sFarm, vDep(sFarm, sReceptor)) + 20 =l= 2*pHabitats(sReceptor, 'KDW') ;
eqContribution(sFarm,sReceptor)$(pDeposition(sFarm, sReceptor, "DD") ne 0) ..     vDep(sFarm, sReceptor)  =l=  0.10 *pHabitats(sReceptor, 'KDW') ;
eqTotalImpact.. sum(sFarm, SUM(sReceptor$(pDeposition(sFarm, sReceptor, "DD") ne 0), vDep(sFarm, sReceptor)/pHabitats(sReceptor, 'KDW'))) =l= 308.74 ;



$offtext

**Scenario '4-5-6: Using impact score, based on sum deposition/Cl ratio, respectively 10-5-2


Equations
*eqTotalDeposition(sReceptor)     deposition nature j
eqvDepPercCL(sFarm)
eqTotalImpact
;

*eqTotalDeposition(sReceptor) ..    SUM(sFarm, vDep(sFarm, sReceptor)) + 20 =l= 2*pHabitats(sReceptor, 'KDW') ;

eqvDepPercCL(sFarm)..         sum(sReceptor$(pDeposition(sFarm, sReceptor, "DD") ne 0), vDep(sFarm, sReceptor)/pHabitats(sReceptor, 'KDW')) =l= 10 ;
eqTotalImpact.. sum(sFarm, SUM(sReceptor$(pDeposition(sFarm, sReceptor, "DD") ne 0), vDep(sFarm, sReceptor)/pHabitats(sReceptor, 'KDW'))) =l= 139.58 ;



Model RegionalModel /All/ ;

*RegionalModel.scaleopt=1;

Option Reslim = 200000 ;

*Option LP = GAMSCHK ;
Solve RegionalModel using lp maximizing vAmmoniaEmissionRegion ;



*Display vNumberOfAnimals.l, vNumberOfAnimals.m ;

Display RegionalModel.MODELSTAT, RegionalModel.SOLVESTAT ;

parameter

TD(sReceptor)  Total deposition in nature area j in kgN per hectares per year
AverageDeposition Average deposition
AverageFarm(sFarm) Average deposition of farm x in kgN per hectares per year
ImpactScore(sFarm)
TotalImpact
;



TD(sReceptor) =  sum(sFarm, vDep.L(sFarm, sReceptor)) + 20 ;
AverageDeposition= sum(sReceptor, TD(sReceptor))/ card(sReceptor) ;
AverageFarm(sFarm) = (SUM(sReceptor, vDep.L(sFarm, sReceptor)))/(card(sReceptor)) ;
ImpactScore(sFarm) = SUM(sReceptor, vDep.L(sFarm, sReceptor)/pHabitats(sReceptor, 'KDW'))   ;
TotalImpact = SUM(sFarm, ImpactScore(sFarm)) ;

display TD,AverageDeposition, AverageFarm ;


