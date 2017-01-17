*=============================================================================
* File      : GPBVsensitivityTIStreshold.gms
* Author    : David De Pue
* Version   :
* Date      : 17-January-2017
* Changed   :
* Changed
* Remarks:
**TIS treshold (total impact score) 0-10 (sensitivity analysis)
*Textify parameter declarations in file 'GPBVreportingeconomic.gms'
*(no declarations allowed within loop).
*4 scenarios: Reference (scen1), Efficiency (scen2),
*TISconstraint(scen3) and Effectiveness (scen4)

*===============================================================================
*================Define number of iterations and range of CL treshold ==========
*===============================================================================

set run /r1*r400/     ;

set scenario /scen1*scen4/ ;

Parameter
pTIStresholdSens(run) ;
pTIStresholdSens(run) = (ord(run)) *0.1  ;

*===============================================================================
*=======================Data preprocessing and data input=======================
*===============================================================================
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

Scalar pHealthCost health cost (euro) per kg ammonia emitted /36/ ;

Scalar pTIStreshold Critical load treshold /1/ ;

Set sAnimalsIncluded(sAnimalCategory) all animals considered in economic model (dynamic set)
;

sAnimalsIncluded(sAnimalCategory) = yes ;
sAnimalsIncluded('Turkeys') = no ;
sAnimalsIncluded('Horses') = no ;
sAnimalsIncluded('FatteningCalves') = no ;

Set class /Green, Orange, Red/

Parameter
pFarmColour(sFarm) Classify farm according to ANB colour - assuming maximum capacity 1:green 2:orange 3:red
pSS(sFarm) Significance score if full capacity
pTIS(sFarm) Total Impact Score if full capacity
pTotalClassNumbers(class) ;

pSS(sFarm) = (sum(sAnimalsIncluded,(pEmissionFactor(sAnimalsIncluded) * pFarmAnimals(sFarm, sAnimalsIncluded)))/5000) * pImpactScores(sFarm, 'SS') ;
pSS(sFarm)$(pSS(sFarm) = 0) = EPS ;
pTIS(sFarm) = (sum(sAnimalsIncluded,(pEmissionFactor(sAnimalsIncluded) * pFarmAnimals(sFarm, sAnimalsIncluded)))/5000) * pImpactScores(sFarm, 'TIS') ;
pTIS(sFarm)$(pTIS(sFarm) = 0) = EPS ;

Loop(class,
pTotalClassNumbers(class) = 0
) ;

Loop(sFarm,
If (pSS(sFarm) > 50,
   pFarmColour(sFarm) = 3 ;
   pTotalClassNumbers('Red') = pTotalClassNumbers('Red')+1 ;
Elseif pSS(sFarm) <5,
   pFarmColour(sFarm) = 1 ;
   pTotalClassNumbers('Green') = pTotalClassNumbers('Green')+1 ;
Else
   pFarmColour(sFarm) = 2 ;
   pTotalClassNumbers('Orange') = pTotalClassNumbers('Orange')+1 ;  )
   ;
) ;

*===============================================================================
*========Declaration Intermediate parameters + reported parameters==============
*===============================================================================
parameter
dSignificanceScore(sFarm)
dTotalImpactScore(sFarm)
dTotalImpact
dTotalProfit
dClosedFarms
dAmmoniaEmissionFarm(sFarm)
dAmmoniaEmissionRegion
dProfitFarm(sFarm)
dMaxProfitFarm(sFarm)
dMaxProfitRegion
dPercentageofMaxProfit(sFarm)
dPercentageMaxProfitRegion
dMaxImpactRegion
dMaxAmmoniaEmissionRegion
dPrivateProfit
dExternalCostHealth
;

parameter
dTotalImpactReference, dTotalProfitReference, dAmmoniaEmissionReference       ;

parameters
zClosedFarms(run, scenario)
zPercentageMaxProfit(run, scenario)
zTotalImpact(run, scenario)
zTotalProfit(run, scenario)
zPrivateProfit(run, scenario)
zExternalHealthCost(run, scenario)
zModelStat(run, scenario)
zSolveStat(run, scenario)
;

*===============================================================================
*========================Model (all equations-all scenarios)====================
*===============================================================================

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

*-------------------------------------------------------------------------------
*Scen1: <5% CL in  KHC (Reference)-----------------------------------------
*-------------------------------------------------------------------------------

Equations
eqSignificanceScore(sFarm) Significance Score constraint 5% (reference scenario)
;


eqSignificanceScore(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'SS') =l= 5 ;

Model Reference /eqAnimals, eqAmmoniaEmissionFarm, eqAmmoniaEmissionRegion, eqProfitFarm, eqProfitSociety, eqYoungCows, eqCows, eqSignificanceScore/          ;
*Model Scenario1 /eqAmmoniaEmissionFarm, eqAmmoniaEmissionRegion, eqProfitFarm, eqProfitSociety, eqYoungCows, eqCows, eqSignificanceScore/          ;

*-------------------------------------------------------------------------------
*Scen2: Efficiency check: Total Impact max. sc1, max. profit society, no individual farm constraints
*-------------------------------------------------------------------------------

Equations
eqTotalImpactRegion
;

*lower than total impact from previous model
eqTotalImpactRegion..
sum(sFarm, (vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'TIS')) =l= dTotalImpactReference ;

Model Efficiency /Reference - eqSignificanceScore + eqTotalImpactRegion/          ;

*-------------------------------------------------------------------------------
**Scen3: Using impact score, based on sum deposition/Cl ratio (sensitivity)-----
*-------------------------------------------------------------------------------

Equations
eqTotalImpactSc4(sFarm) Total Impact Score constraint 10
;

eqTotalImpactSc4(sFarm)..
(vAmmoniaEmissionFarm(sFarm)/5000)* pImpactScores(sFarm, 'TIS') =l= pTIStreshold ;


Model TISconstraint /Efficiency + eqTotalImpactSc4/          ;

*-------------------------------------------------------------------------------
**Scen4: Effectivity check, minimize TIS,  societal profit bigger  than sc1
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

Model Effectiveness /Reference - eqSignificanceScore + eqTotalImpactRegionSc7 + eqSocietalProfit/ ;

*===============================================================================
*=======================Different model runs====================================
*===============================================================================
loop(run,

pTIStreshold = pTIStresholdSens(run)          ;

Option lp = CPLEX ;

Solve Reference maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms

dAmmoniaEmissionReference = dAmmoniaEmissionRegion ;
dTotalImpactReference = dTotalImpact ;
dTotalProfitReference = dTotalProfit ;

zClosedFarms(run, 'scen1') = dClosedFarms ;
zPercentageMaxProfit(run, 'scen1') =  dPercentageMaxProfitRegion ;
zTotalImpact(run, 'scen1') = dTotalImpact ;
zTotalProfit(run, 'scen1') = dTotalProfit ;
zPrivateProfit(run, 'scen1') = dPrivateProfit  ;
zExternalHealthCost(run, 'scen1') = dExternalCostHealth ;
zModelStat(run, 'scen1') = Reference.MODELSTAT  ;
zSolveStat(run, 'scen1') = Reference.SOLVESTAT  ;

Solve Efficiency maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms

zClosedFarms(run, 'scen2') = dClosedFarms ;
zPercentageMaxProfit(run, 'scen2') =  dPercentageMaxProfitRegion ;
zTotalImpact(run, 'scen2') = dTotalImpact ;
zTotalProfit(run, 'scen2') = dTotalProfit ;
zPrivateProfit(run, 'scen2') = dPrivateProfit  ;
zExternalHealthCost(run, 'scen2') = dExternalCostHealth ;
zModelStat(run, 'scen2') = Efficiency.MODELSTAT  ;
zSolveStat(run, 'scen2') = Efficiency.SOLVESTAT  ;

Solve TISconstraint maximizing vProfitSociety using lp ;

$batinclude GPBVreportingeconomic.gms

zClosedFarms(run, 'scen3') = dClosedFarms ;
zPercentageMaxProfit(run, 'scen3') =  dPercentageMaxProfitRegion ;
zTotalImpact(run, 'scen3') = dTotalImpact ;
zTotalProfit(run, 'scen3') = dTotalProfit ;
zPrivateProfit(run, 'scen3') = dPrivateProfit  ;
zExternalHealthCost(run, 'scen3') = dExternalCostHealth ;
zModelStat(run, 'scen3') = TISconstraint.MODELSTAT  ;
zSolveStat(run, 'scen3') = TISconstraint.SOLVESTAT  ;

Solve Effectiveness using lp minimizing vTotalImpact ;

$batinclude GPBVreportingeconomic.gms

zClosedFarms(run, 'scen4') = dClosedFarms ;
zPercentageMaxProfit(run, 'scen4') =  dPercentageMaxProfitRegion ;
zTotalImpact(run, 'scen4') = dTotalImpact ;
zTotalProfit(run, 'scen4') = dTotalProfit ;
zPrivateProfit(run, 'scen4') = dPrivateProfit  ;
zExternalHealthCost(run, 'scen4') = dExternalCostHealth ;
zModelStat(run, 'scen4') = Effectiveness.MODELSTAT  ;
zSolveStat(run, 'scen4') = Effectiveness.SOLVESTAT  ;

);

*===============================================================================
*=======================Write away results======================================
*===============================================================================
execute_unloaddi 'SensitivityTIStreshold.gdx' zClosedFarms zPercentageMaxProfit zTotalImpact zTotalProfit zPrivateProfit zExternalHealthCost zModelStat zSolvestat
