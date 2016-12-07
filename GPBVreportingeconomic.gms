*=============================================================================
* File      : GPBVreportingeconomic.gms
* Author    : David De Pue
* Version   :
* Date      :
* Changed   : 06-December-2016
* Changed
* Remarks: All parameters/results that are of interest
*Zeros replaced by EPS, for consequent analysis in R/QGIS

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
;

dSignificanceScore(sFarm) = (vAmmoniaEmissionFarm.l(sFarm)/5000)* pImpactScores(sFarm, 'SS')   ;
dSignificanceScore(sFarm)$(dSignificanceScore(sFarm) = 0) = Eps ;
dTotalImpactScore(sFarm) = (vAmmoniaEmissionFarm.l(sFarm)/5000)* pImpactScores(sFarm, 'TIS')     ;
dTotalImpactScore(sFarm)$(dTotalImpactScore(sFarm) = 0) = Eps ;
dTotalImpact = sum(sFarm, dTotalImpactScore(sFarm)) ;
dTotalProfit = vProfitSociety.l ;
dClosedFarms  = card(sFarm) - sum(sFarm, rel_ne(vAmmoniaEmissionFarm.l(sFarm), 0))                ;
dAmmoniaEmissionFarm(sFarm) = vAmmoniaEmissionFarm.l(sFarm) ;
dAmmoniaEmissionRegion = vAmmoniaEmissionRegion.l ;
dProfitFarm(sFarm) = vProfitFarm.l(sFarm) ;
dProfitFarm(sFarm)$(dProfitFarm(sFarm) = 0) = Eps ;
dMaxProfitFarm(sFarm) = sum(ssAnimalcategory, (pFarmAnimals(sFarm, ssAnimalCategory) * pBrutoSaldo(ssAnimalCategory))) ;
dMaxProfitRegion = sum(sFarm, dMaxProfitFarm(sFarm)) ;
dPercentageofMaxProfit(sFarm)$(dMaxProfitFarm(sFarm) ne 0)  = (dProfitFarm(sFarm)/dMaxProfitFarm(sFarm))*100 ;
dPercentageofMaxProfit(sFarm)$(dPercentageofMaxProfit(sFarm) = 0) = Eps ;
dPercentageMaxProfitRegion = (sum(sFarm, dProfitFarm(sFarm)) / sum(sFarm, dMaxProfitFarm(sFarm))) * 100 ;
dMaxImpactRegion = sum(sFarm, pTIS(sFarm)) ;
dMaxAmmoniaEmissionRegion = sum((sFarm, sAnimalsIncluded), (pFarmAnimals(sFarm, sAnimalsIncluded) * pEmissionFactor(sAnimalsIncluded))) ;
