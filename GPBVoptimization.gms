*Dataset GPBV farms Flanders (coordinates, permitted animals)
*EMAV ammonia stable emissions
*Hoedje dataset (IFDM, VITO, 20*20 km², resolutie 100 m, meteo 2012 Luchtbal)
*Deposition velocities VLOPS
*Data preprocessing and making of gdx file in R


********************************************************************************
*****************************Data preprocessing in R and  Data Input************
********************************************************************************
*Linking source/receptor with right data from 'hoedje'
*Making GDX file with all the data

*Running R code to get calibration set (only when processing new source data)
*$setglobal R "C:\Program Files\R\R-3.3.1\bin\i386\R.exe"
*$setglobal Rfile GPBV.R

*$call "=%R% --rhome=%system.fp% CMD BATCH %rfile% %rfile%.log"

Sets
    sFarm /s1*s826/
    sImpactscores /TIS, SS/
    sCoordinates /X, Y/
    sAnimalCategory ;

$gdxin GPBV.gdx
$load sAnimalCategory
$gdxin

Parameters
pFarmCoord(sFarm, sCoordinates)
pImpactScores(sFarm, sImpactscores)
pFarmAnimals(sFarm, sAnimalCategory)
pPermitYear(sFarm)
pEmissionFactor(sAnimalCategory)
;

$gdxin GPBV.gdx
$load pFarmCoord, pImpactScores, pFarmAnimals, pPermitYear, pEmissionFactor
$gdxin

********************************************************************************
********************************Model*******************************************
********************************************************************************

Variables
vAmmoniaEmissionRegion
vAmmoniaEmissionFarm(sFarm)  ;

Positive variable
vAnimals(sFarm, sAnimalCategory) ;

Equations
eqAnimals(sFarm, sAnimalCategory) Permit constraint
eqAmmoniaEmissionFarm(sFarm) ammonia emission per farm
eqTotalImpact(sFarm) Total Impact Score constraint


eqAmmoniaEmissionRegion objective function
;




eqAmmoniaEmissionFarm(sFarm)..
vAmmoniaEmissionFarm(sFarm) =e= sum(sAnimalCategory, (pEmissionFactor(sAnimalCategory) * vAnimals(sFarm, sAnimalCategory))) ;

**Constraints
eqTotalImpact(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'TIS') =l= 10000 ;

eqAnimals(sFarm, sAnimalCategory)..
vAnimals(sFarm, sAnimalCategory) =l= pFarmAnimals(sFarm, sAnimalCategory) ;

**Objective
eqAmmoniaEmissionRegion..
vAmmoniaEmissionRegion =e= SUM(sFarm,vAmmoniaEmissionFarm(sFarm))    ;


********************************************************************************
********************************************************************************
********************************Scenario Analysis*******************************
********************************************************************************
********************************************************************************

**Scenario 1: <3% CL in  KHC (Reference)


Equations
eqSignificanceScore(sFarm) Significance Score constraint
;


eqSignificanceScore(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'SS') =l= 3 ;



$ontext

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





**Scenario '4-5-6: Using impact score, based on sum deposition/Cl ratio, respectively 10-5-2


Equations
*eqTotalDeposition(sReceptor)     deposition nature j
eqvDepPercCL(sFarm)
eqTotalImpact
;

*eqTotalDeposition(sReceptor) ..    SUM(sFarm, vDep(sFarm, sReceptor)) + 20 =l= 2*pHabitats(sReceptor, 'KDW') ;

eqvDepPercCL(sFarm)..         sum(sReceptor$(pDeposition(sFarm, sReceptor, "DD") ne 0), vDep(sFarm, sReceptor)/pHabitats(sReceptor, 'KDW')) =l= 10 ;
eqTotalImpact.. sum(sFarm, SUM(sReceptor$(pDeposition(sFarm, sReceptor, "DD") ne 0), vDep(sFarm, sReceptor)/pHabitats(sReceptor, 'KDW'))) =l= 139.58 ;

$offtext

Model RegionalModel /All/ ;

Option Reslim = 200000 ;

Solve RegionalModel using lp maximizing vAmmoniaEmissionRegion ;

Display vAnimals.l, vAnimals.m ;

Display RegionalModel.MODELSTAT, RegionalModel.SOLVESTAT ;

parameter
dSignificanceScore(sFarm)
dTotalImpactScore(sFarm)
dTotalImpact
dPercentageOccupied(sFarm, sAnimalCategory) percentage of permitted capacitiy occupied by animals
dPercentageOccupiedFarm(sFarm)
dPercentageOccupiedRegion
dPermittedAnimals(sFarm)
;

dSignificanceScore(sFarm) = (vAmmoniaEmissionFarm.l(sFarm)/5000)* pImpactScores(sFarm, 'SS')   ;
dTotalImpactScore(sFarm) = (vAmmoniaEmissionFarm.l(sFarm)/5000)* pImpactScores(sFarm, 'TIS')     ;
dTotalImpact = sum(sFarm, dTotalImpactScore(sFarm)) ;
dPercentageOccupied(sFarm, sAnimalCategory)$(pFarmAnimals(sFarm, sAnimalCategory) ne 0) = (vAnimals.l(sFarm, sAnimalCategory)/pFarmAnimals(sFarm, sAnimalCategory)) * 100  ;
dPermittedAnimals(sFarm) =  sum(sAnimalCategory, rel_ne(pFarmAnimals(sFarm, sAnimalCategory),0)) ;
dPercentageOccupiedFarm(sFarm)$(dPermittedAnimals(sFarm) ne 0) = (sum(sAnimalCategory, dPercentageOccupied(sFarm, sAnimalCategory)) / dPermittedAnimals(sFarm));
dPercentageOccupiedRegion = (sum(sFarm,(dPercentageOccupiedFarm(sFarm)))/card(sFarm))             ;

display dSignificancescore, dTotalImpactScore, dTotalImpact, dPercentageOccupied,  dPercentageOccupiedFarm, dPercentageOccupiedRegion ;


