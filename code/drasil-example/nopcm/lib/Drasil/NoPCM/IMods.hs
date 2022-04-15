module Drasil.NoPCM.IMods (eBalanceOnWtr, iMods, instModIntro) where

import Language.Drasil
import Theory.Drasil (InstanceModel, im, qwC, qwUC, newDEModel')
import Utils.Drasil (weave)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Control.Lens ((^.))

import Data.Drasil.Concepts.Documentation (goal)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.PhysicalProperties (liquid)
import Data.Drasil.Concepts.Thermodynamics (melting, boilPt)

import Data.Drasil.Quantities.Physics (energy, time)

import Drasil.SWHS.Concepts (water)
import Drasil.SWHS.DataDefs (balanceDecayRate)
import Drasil.SWHS.GenDefs (htFluxWaterFromCoil)
import Drasil.SWHS.IMods (eBalanceOnWtrDerivDesc1, eBalanceOnWtrDerivDesc3, heatEInWtr)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.Unitals (coilHTC, coilSA, htCapW, htFluxC, tauW, tempC,
  tempInit, tempW, timeFinal, wMass)

import Drasil.NoPCM.Assumptions (assumpNIHGBW, assumpWAL)
import Drasil.NoPCM.Goals (waterTempGS, waterEnergyGS)
import Drasil.NoPCM.Derivations (eBalanceOnWtrDerivEqns)

iMods :: [InstanceModel]
iMods = [eBalanceOnWtr, heatEInWtr]

---------
-- IM1 --
---------
-- FIXME: comment on reference?
eBalanceOnWtr :: InstanceModel
eBalanceOnWtr = im (newDEModel' eBalanceOnWtrRC)
  [qwC tempC $ UpFrom (Inc, sy tempInit)
  , qwUC tempInit, qwUC timeFinal, qwUC coilSA, qwUC coilHTC, qwUC htCapW, qwUC wMass]
  (qw tempW) []
  --Tw(0) cannot be presented, there is one more constraint Tw(0) = Tinit
  [dRefInfo koothoor2013 $ RefNote "with PCM removed"]
  (Just eBalanceOnWtrDeriv) "eBalanceOnWtr" balWtrNotes

eBalanceOnWtrRC :: DifferentialModel 
eBalanceOnWtrRC = 
  makeLinear
    time 
    tempW
    [exactDbl 1 $* 1]
    balWtrExpr
    "eBalanceOnWtrRC" 
    (nounPhraseSP $ "Energy balance on " ++ "water to find the temperature of the water") 
    (tempW ^. defn)

balWtrExpr :: PExpr
balWtrExpr = neg (recip_ (sy tauW) `mulRe` (sy tempC $- apply1 tempW time))

balWtrNotes :: [Sentence]
balWtrNotes = map foldlSent [
  [ch tauW `S.is` S "calculated from", refS balanceDecayRate],
  [S "The above", phrase equation, S "applies as long as the", phrase water,
   S "is in", phrase liquid, S "form" `sC` eS (realInterval tempW $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 100)),
   sParen (unwrap $ getUnit tempW), S "where", eS (exactDbl 0),
   sParen (unwrap $ getUnit tempW) `S.and_` eS (exactDbl 100),
   sParen (unwrap $ getUnit tempW), S "are the", pluralNP ((melting `and_`
   boilPt) `of_PSNPNI` water) `sC` S "respectively", sParen (refS assumpWAL)]]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv = mkDerivName (phraseNP (the energy) +:+ S "balance on water")
  (weave [eBalanceOnWtrDerivSentences, map eS eBalanceOnWtrDerivEqns])

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = [eBalanceOnWtrDerivDesc1 EmptyS (S "over area" +:+ ch coilSA) EmptyS assumpNIHGBW,
  eBalanceOnWtrDerivDesc2, eBalanceOnWtrDerivDesc3, eBalanceOnWtrDerivDesc4]

eBalanceOnWtrDerivDesc2 :: Sentence
eBalanceOnWtrDerivDesc2 = foldlSentCol [S "Using", refS htFluxWaterFromCoil `S.for`
  ch htFluxC `sC` S "this can be written as"]

eBalanceOnWtrDerivDesc4 :: Sentence
eBalanceOnWtrDerivDesc4 = substitute [balanceDecayRate]

-----------
-- Intro --
-----------

instModIntro :: Sentence
instModIntro = foldlSent [atStartNP (the goal), refS waterTempGS,
  S "is met by", refS eBalanceOnWtr `S.andThe` phrase goal,
  refS waterEnergyGS, S "is met by", refS heatEInWtr]
