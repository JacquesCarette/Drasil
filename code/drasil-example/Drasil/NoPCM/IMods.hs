module Drasil.NoPCM.IMods (eBalanceOnWtr, iMods, instModIntro) where

import Language.Drasil
import Theory.Drasil (DataDefinition, InstanceModel, im)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (goal)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.PhysicalProperties (liquid)
import Data.Drasil.Concepts.Thermodynamics (melting, boilPt)

import Data.Drasil.Quantities.Physics (energy, time)

import Drasil.SWHS.Concepts (water)
import Drasil.SWHS.DataDefs (ddHtFluxC, balanceDecayRate)
import Drasil.SWHS.IMods (eBalanceOnWtrDerivDesc1, eBalanceOnWtrDerivDesc3,
 heatEInWtr)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.Unitals (tempW, tempC, tauW, wMass, htCapW, coilHTC, 
  coilSA, tempInit, timeFinal, htFluxC)

import Drasil.NoPCM.Assumptions (assumpNIHGBW, assumpWAL)
import Drasil.NoPCM.Goals (waterTempGS, waterEnergyGS)

iMods :: [InstanceModel]
iMods = [eBalanceOnWtr, heatEInWtr]

---------
-- IM1 --
---------
-- FIXME: comment on reference?
eBalanceOnWtr :: InstanceModel
eBalanceOnWtr = im eBalanceOnWtrRC [qw tempC, qw tempInit, qw timeFinal, 
  qw coilSA, qw coilHTC, qw htCapW, qw wMass] 
  [sy tempInit $<= sy tempC] (qw tempW) 
  --Tw(0) cannot be presented, there is one more constraint Tw(0) = Tinit
  [0 $< sy time $< sy timeFinal] [makeCiteInfo koothoor2013 $ RefNote "with PCM removed"] 
  (Just eBalanceOnWtrDeriv) "eBalanceOnWtr" [balWtrDesc]

eBalanceOnWtrRC :: RelationConcept
eBalanceOnWtrRC = makeRC "eBalanceOnWtrRC" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtrRel
  -- (mkLabelSame "eBalnaceOnWtr" (Def Instance))

balWtrRel :: Relation
balWtrRel = deriv (sy tempW) time $= 1 / sy tauW *
  (sy tempC - apply1 tempW time)

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ sy tempW) `isThe` phrase tempW +:+.
  sParen (unwrap $ getUnit tempW), 
  (E $ sy tempC) `isThe` phrase tempC +:+. sParen (unwrap $ getUnit tempC),
  ch tauW, S "is from" +:+. makeRef2S balanceDecayRate,
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ 0 $< sy tempW $< 100),
  sParen (unwrap $ getUnit tempW), S "where", E 0,
  sParen (unwrap $ getUnit tempW) `sAnd` E 100,
  sParen (unwrap $ getUnit tempW), S "are the", phrase melting `sAnd`
  plural boilPt, S "of", phrase water `sC` S "respectively"
  +:+ sParen (makeRef2S assumpWAL)]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv = mkDerivName (S "the" +:+ phrase energy +:+ S "balance on water")
  (weave [eBalanceOnWtrDerivSentences, map E eBalanceOnWtrDerivEqns])

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = map foldlSentCol [
  eBalanceOnWtrDerivDesc1 EmptyS (S "over area" +:+ (E $ sy coilSA)) EmptyS assumpNIHGBW,
  eBalanceOnWtrDerivDesc2 ddHtFluxC,
  eBalanceOnWtrDerivDesc3, eBalanceOnWtrDerivDesc4]

eBalanceOnWtrDerivDesc2 :: DataDefinition -> [Sentence]
eBalanceOnWtrDerivDesc2 dd = [S "Using", makeRef2S dd, S "for", ch dd `sC`
  S "this can be written as"]

eBalanceOnWtrDerivDesc4 :: [Sentence]
eBalanceOnWtrDerivDesc4 = [substitute [balanceDecayRate]]

eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4 :: Expr

eBalanceOnWtrDerivEqn1 = sy wMass * sy htCapW * deriv (sy tempW) time $= 
  sy htFluxC * sy coilSA

eBalanceOnWtrDerivEqn2 = sy wMass * sy htCapW * deriv (sy tempW) time $= 
  sy coilHTC * sy coilSA *  (sy tempC - sy tempW)

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $= 
  (sy coilHTC * sy coilSA / 
  (sy wMass * sy htCapW)) *  (sy tempC - sy tempW)

eBalanceOnWtrDerivEqn4 =  
  deriv (sy tempW) time $= 1 / sy tauW * (sy tempC - sy tempW)

eBalanceOnWtrDerivEqns :: [Expr]
eBalanceOnWtrDerivEqns = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4]

-----------
-- Intro --
-----------

instModIntro :: Sentence
instModIntro = foldlSent [S "The", phrase goal, makeRef2S waterTempGS,
  S "is met by", makeRef2S eBalanceOnWtr `andThe` phrase goal,
  makeRef2S waterEnergyGS, S "is met by", makeRef2S heatEInWtr]
  