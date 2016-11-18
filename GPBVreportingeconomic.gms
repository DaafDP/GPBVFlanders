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
dAmmoniaEmissionRegion
;

dSignificanceScore(sFarm) = (vAmmoniaEmissionFarm.l(sFarm)/5000)* pImpactScores(sFarm, 'SS')   ;
dTotalImpactScore(sFarm) = (vAmmoniaEmissionFarm.l(sFarm)/5000)* pImpactScores(sFarm, 'TIS')     ;
dTotalImpact = sum(sFarm, dTotalImpactScore(sFarm)) ;
dTotalProfit = vProfitSociety.l ;
dPercentageOccupied(sFarm, sAnimalCategory)$(pFarmAnimals(sFarm, sAnimalCategory) ne 0) = (vAnimals.l(sFarm, sAnimalCategory)/pFarmAnimals(sFarm, sAnimalCategory)) * 100  ;
dPermittedAnimals(sFarm) =  sum(sAnimalCategory, rel_ne(pFarmAnimals(sFarm, sAnimalCategory),0)) ;
dPercentageOccupiedFarm(sFarm)$(dPermittedAnimals(sFarm) ne 0) = (sum(sAnimalCategory, dPercentageOccupied(sFarm, sAnimalCategory)) / dPermittedAnimals(sFarm));
dPercentageOccupiedRegion = (sum(sFarm,(dPercentageOccupiedFarm(sFarm)))/card(sFarm))             ;
dClosedFarms  = card(sFarm) - sum(sFarm, rel_ne(vAmmoniaEmissionFarm.l(sFarm), 0))                ;
dAmmoniaEmissionRegion = vAmmoniaEmissionRegion.l ;

