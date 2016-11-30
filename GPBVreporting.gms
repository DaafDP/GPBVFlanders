*All parameters/results that are of interest
*Zeros replaced by EPS, for consequent

parameter
dSignificanceScore(sFarm)
dTotalImpactScore(sFarm)
dTotalImpact
dTotalProfit
dPercentageOccupied(sFarm, sAnimalCategory) percentage of permitted capacitiy occupied by animals
dPercentageOccupiedFarm(sFarm)
dPercentageOccupiedRegion
dPermittedAnimals(sFarm)
dClosedFarms
dAmmoniaEmissionFarm(sFarm)
dAmmoniaEmissionRegion
dProfitFarm(sFarm)
;

dSignificanceScore(sFarm) = (vAmmoniaEmissionFarm.l(sFarm)/5000)* pImpactScores(sFarm, 'SS')   ;
dSignificanceScore(sFarm)$(dSignificanceScore(sFarm) = 0) = Eps ;
dTotalImpactScore(sFarm) = (vAmmoniaEmissionFarm.l(sFarm)/5000)* pImpactScores(sFarm, 'TIS')     ;
dTotalImpactScore(sFarm)$(dTotalImpactScore(sFarm) = 0) = Eps ;
dTotalImpact = sum(sFarm, dTotalImpactScore(sFarm)) ;
dTotalProfit = vProfitSociety.l ;
dPercentageOccupied(sFarm, sAnimalCategory)$(pFarmAnimals(sFarm, sAnimalCategory) ne 0) = (vAnimals.l(sFarm, sAnimalCategory)/pFarmAnimals(sFarm, sAnimalCategory)) * 100  ;
dPermittedAnimals(sFarm) =  sum(sAnimalCategory, rel_ne(pFarmAnimals(sFarm, sAnimalCategory),0)) ;
dPercentageOccupiedFarm(sFarm)$(dPermittedAnimals(sFarm) ne 0) = (sum(sAnimalCategory, dPercentageOccupied(sFarm, sAnimalCategory)) / dPermittedAnimals(sFarm));
dPercentageOccupiedFarm(sFarm)$(dPercentageOccupiedFarm(sFarm) = 0) = Eps ;
dPercentageOccupiedRegion = (sum(sFarm,(dPercentageOccupiedFarm(sFarm)))/card(sFarm))             ;
dClosedFarms  = card(sFarm) - sum(sFarm, rel_ne(vAmmoniaEmissionFarm.l(sFarm), 0))                ;
dAmmoniaEmissionFarm(sFarm) = vAmmoniaEmissionFarm.l(sFarm) ;
dAmmoniaEmissionRegion = vAmmoniaEmissionRegion.l ;
dProfitFarm(sFarm) = vProfitFarm.l(sFarm) ;
dProfitFarm(sFarm)$(dProfitFarm(sFarm) = 0) = Eps ;

