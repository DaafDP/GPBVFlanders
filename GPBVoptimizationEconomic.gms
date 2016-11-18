*Dataset GPBV farms Flanders (coordinates, permitted animals)
*EMAV ammonia stable emissions
*Hoedje dataset (IFDM, VITO, 20*20 km�, resolutie 100 m, meteo 2012 Luchtbal)
*Deposition velocities VLOPS
*Bruto Saldo AML 2012
*HealthCost ammonia 12�/kg
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
    sAnimalCategory
    ssAnimalCategory(sAnimalCategory) ;

$gdxin GPBV.gdx
$load sAnimalCategory
$gdxin

$gdxin BrutoSaldo.gdx
$load ssAnimalCategory
$gdxin

Parameters
pFarmCoord(sFarm, sCoordinates)
pImpactScores(sFarm, sImpactscores)
pFarmAnimals(sFarm, sAnimalCategory)
pPermitYear(sFarm)
pEmissionFactor(sAnimalCategory)
pBrutoSaldo(ssAnimalCategory)
;

$gdxin GPBV.gdx
$load pFarmCoord, pImpactScores, pFarmAnimals, pPermitYear, pEmissionFactor
$gdxin

$gdxin BrutoSaldo.gdx
$load pBrutoSaldo
$gdxin

Scalar pHealthCost health cost (euro) per kg ammonia emitted /12/ ;

Set sAnimalsIncluded(sAnimalCategory) all animals considered in economic model (dynamic set)
;

sAnimalsIncluded(sAnimalCategory) = yes ;
sAnimalsIncluded('Turkeys') = no ;
sAnimalsIncluded('Horses') = no ;
sAnimalsIncluded('FatteningCalves') = no ;


********************************************************************************
********************************Model*******************************************
********************************************************************************

Variables
vAmmoniaEmissionRegion
vAmmoniaEmissionFarm(sFarm)
vProfitSociety
vProfitFarm(sFarm) ;

Positive  variable
vAnimals(sFarm, sAnimalCategory) ;

Equations
eqAnimals(sFarm, sAnimalCategory) Permit constraint
eqAmmoniaEmissionFarm(sFarm) ammonia emission per farm
eqAmmoniaEmissionRegion total ammonia emission region
eqProfitFarm(sFarm) profit per farm
eqProfitSociety objective fucntion
eqYoungCows(sFarm) For every adult cow we assume there's one cow younger than 1 and 1 cow 1 to 2
eqCows(sFarm)  For every adult cow we assume there's one cow younger than 1 and 1 cow 1 to 2
;

eqAmmoniaEmissionFarm(sFarm)..
vAmmoniaEmissionFarm(sFarm) =e= sum(sAnimalsIncluded, (pEmissionFactor(sAnimalsIncluded) * vAnimals(sFarm, sAnimalsIncluded))) ;

eqAnimals(sFarm, sAnimalCategory)..
vAnimals(sFarm, sAnimalCategory) =l= pFarmAnimals(sFarm, sAnimalCategory) ;

eqAmmoniaEmissionRegion..
vAmmoniaEmissionRegion =e= SUM(sFarm,vAmmoniaEmissionFarm(sFarm))    ;

eqProfitFarm(sFarm)..
vProfitFarm(sFarm) =e= sum(ssAnimalCategory, (vAnimals(sFarm, ssAnimalCategory) * pBrutoSaldo(ssAnimalCategory)))   ;

eqProfitSociety..
vProfitSociety =e= sum(sFarm, vProfitFarm(sFarm)) - (pHealthCost * vAmmoniaEmissionRegion) ;

eqYoungCows(sFarm)..
vAnimals(sFarm, 'Cows0to1') - vAnimals(sFarm, 'Cows1to2') =e= 0 ;

eqCows(sFarm)..
vAnimals(sFarm, 'AdultCows') - 3*vAnimals(sFarm, 'Cows0to1')  =e= 0 ;

*===============================================================================
*===============================Scenario Analysis===============================
*===============================================================================

*-------------------------------------------------------------------------------
*Scenario 1: <3% CL in  KHC (Reference)-----------------------------------------
*-------------------------------------------------------------------------------


Equations
eqSignificanceScore(sFarm) Significance Score constraint
;


eqSignificanceScore(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'SS') =l= 3 ;

Model Scenario1 /eqAnimals, eqAmmoniaEmissionFarm, eqAmmoniaEmissionRegion, eqProfitFarm, eqProfitSociety, eqYoungCows, eqCows, eqSignificanceScore/          ;

Option lp = CPLEX ;

Solve Scenario1 maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms


parameter
dTotalImpactReference, dTotalProfitReference, dAmmoniaEmissionReference, pModelStat, pSolveStat       ;

dAmmoniaEmissionReference = dAmmoniaEmissionRegion ;
dTotalImpactReference = dTotalImpact ;
dTotalProfitReference = dTotalProfit ;
pModelStat = Scenario1.MODELSTAT         ;
pSolveSTat = Scenario1.SOLVESTAT         ;

execute_unload 'Reference.gdx'

*-------------------------------------------------------------------------------
*Scenario 2: Efficiency check: Total Impact max. 1349 (sc1), max. vAmmoniaEmisison, no individual farm constraints
*-------------------------------------------------------------------------------

Equations
eqTotalImpactRegion
;

*lower than total impact from previous model
eqTotalImpactRegion..
sum(sFarm, (vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'TIS')) =l= dTotalImpactReference ;

Model Scenario2 /Scenario1 - eqSignificanceScore + eqTotalImpactRegion/          ;

Option lp = CPLEX ;

Solve Scenario2 maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario2.MODELSTAT         ;
pSolveSTat = Scenario2.SOLVESTAT         ;


execute_unload 'Alternative.gdx'

execute 'gdxdiff Reference.gdx Alternative.gdx scenario2.gdx' ;

*-------------------------------------------------------------------------------
**Scenario 3: Same as scenario 2 (ceiling total impact), but wich individual ceiling of 10%CL
*-------------------------------------------------------------------------------
Equations
eqSignificanceScoreSc3(sFarm) Significance Score constraint
;

eqSignificanceScoreSc3(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'SS') =l= 10 ;
;

Model Scenario3 /Scenario2 + eqSignificanceScoreSc3/          ;

Option lp = CPLEX ;

Solve Scenario3 maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario3.MODELSTAT         ;
pSolveSTat = Scenario3.SOLVESTAT         ;


execute_unload 'Alternative.gdx'

execute 'gdxdiff Reference.gdx Alternative.gdx scenario3.gdx' ;

*-------------------------------------------------------------------------------
**Scenario 4: Using impact score, based on sum deposition/Cl ratio, max 10------
*-------------------------------------------------------------------------------

Equations
eqTotalImpactSc4(sFarm) Total Impact Score constraint
;

eqTotalImpactSc4(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'TIS') =l= 10 ;


Model Scenario4 /Scenario2 + eqTotalImpactSc4/          ;

Option lp = CPLEX ;

Solve Scenario4 maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario4.MODELSTAT         ;
pSolveSTat = Scenario4.SOLVESTAT         ;

execute_unload 'Alternative.gdx'

execute 'gdxdiff Reference.gdx Alternative.gdx scenario4.gdx' ;

*-------------------------------------------------------------------------------
**Scenario 5: Using impact score, based on sum deposition/Cl ratio, max 5-------
*-------------------------------------------------------------------------------

Equations
eqTotalImpactSc5(sFarm) Total Impact Score constraint
;

eqTotalImpactSc5(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'TIS') =l= 5 ;


Model Scenario5 /Scenario2 + eqTotalImpactSc5/          ;

Option lp = CPLEX ;

Solve Scenario5 maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario5.MODELSTAT         ;
pSolveSTat = Scenario5.SOLVESTAT         ;

execute_unload 'Alternative.gdx'

execute 'gdxdiff Reference.gdx Alternative.gdx scenario5.gdx' ;

*-------------------------------------------------------------------------------
**Scenario 6: Using impact score, based on sum deposition/Cl ratio, max 2-------
*-------------------------------------------------------------------------------

Equations
eqTotalImpactSc6(sFarm) Total Impact Score constraint
;

eqTotalImpactSc6(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'TIS') =l= 2 ;


Model Scenario6 /Scenario2 + eqTotalImpactSc6/          ;

Option lp = CPLEX ;

Solve Scenario6 maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario6.MODELSTAT         ;
pSolveSTat = Scenario6.SOLVESTAT         ;

execute_unload 'Alternative.gdx'

execute 'gdxdiff Reference.gdx Alternative.gdx scenario6.gdx' ;

*-------------------------------------------------------------------------------
**Scenario 7: Effectivity check, minimize TIS,  societal profit bigger  than sc1
*-------------------------------------------------------------------------------

Variable
vTotalImpact
;

Equation
eqTotalImpactRegionSc7
eqSocietalProfit ;

eqTotalImpactRegionSc7..
vTotalImpact =e= sum(sFarm, (vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'TIS')) ;

eqSocietalProfit..
vProfitSociety =g= dTotalProfitReference ;

Model Scenario7 /Scenario1 - eqSignificanceScore + eqTotalImpactRegionSc7 + eqSocietalProfit/ ;

Option lp = CPLEX ;

Solve Scenario7 using lp minimizing vTotalImpact ;

$batinclude GPBVreportingeconomic.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario7.MODELSTAT         ;
pSolveSTat = Scenario7.SOLVESTAT         ;

execute_unload 'Alternative.gdx'

execute 'gdxdiff Reference.gdx Alternative.gdx scenario7.gdx' ;



