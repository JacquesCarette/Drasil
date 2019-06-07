module Drasil.NoPCM.IMods (eBalanceOnWtr, iMods, instModIntro) where

import Language.Drasil
import Theory.Drasil (DataDefinition, InstanceModel, im)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (goal)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.PhysicalProperties (liquid)
import Data.Drasil.Concepts.Thermodynamics (melting, boilPt)

import Data.Drasil.Quantities.Physics (energy, time)

import Data.Drasil.Utils (unwrap, weave)

import Drasil.SWHS.Concepts (water)
import Drasil.SWHS.DataDefs (dd1HtFluxC)
import Drasil.SWHS.IMods (eBalanceOnWtrDerivDesc1, heatEInWtr)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.Unitals (temp_W, temp_C, tau_W, w_mass, htCap_W, coil_HTC, 
  coil_SA, temp_init, time_final, ht_flux_C)

import Drasil.NoPCM.Assumptions (assumpNIHGBW, assumpWAL)
import Drasil.NoPCM.Goals (waterTempGS, waterEnergyGS)

iMods :: [InstanceModel]
iMods = [eBalanceOnWtr, heatEInWtr]

---------
-- IM1 --
---------
-- FIXME: comment on reference?
eBalanceOnWtr :: InstanceModel
eBalanceOnWtr = im eBalanceOnWtr_rc [qw temp_C, qw temp_init, qw time_final, 
  qw coil_SA, qw coil_HTC, qw htCap_W, qw w_mass] 
  [sy temp_init $<= sy temp_C] (qw temp_W) 
  --Tw(0) cannot be presented, there is one more constraint Tw(0) = Tinit
  [0 $< sy time $< sy time_final] [makeCiteInfo koothoor2013 $ RefNote "with PCM removed"] 
  eBalanceOnWtrDeriv "eBalanceOnWtr" balWtrNotes

eBalanceOnWtr_rc :: RelationConcept
eBalanceOnWtr_rc = makeRC "eBalanceOnWtr_rc" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") EmptyS balWtr_Rel
  -- (mkLabelSame "eBalnaceOnWtr" (Def Instance))

balWtr_Rel :: Relation
balWtr_Rel = (deriv (sy temp_W) time) $= 1 / (sy tau_W) *
  (((sy temp_C) - (apply1 temp_W time)))

balWtrNotes :: [Sentence]
balWtrNotes = map foldlSent [
  [E (sy tau_W) `sIs` S "calculated from", S "FIXME: Missing DD Issue 1484"],
  [S "The above", phrase equation, S "applies as long as the", phrase water,
   S "is in", phrase liquid, S "form" `sC` (E $ 0 $< sy temp_W $< 100),
   sParen (unwrap $ getUnit temp_W), S "where", E 0,
   sParen (unwrap $ getUnit temp_W) `sAnd` (E 100),
   sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
   plural boilPt `sOf` phrase water `sC` S "respectively", sParen (makeRef2S assumpWAL)]]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv =
  S "Derivation of the" +:+ phrase energy +:+ S "balance on water:" :
  (weave [eBalanceOnWtrDerivSentences, map E eBalanceOnWtrDerivEqns])

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = map foldlSentCol [
  eBalanceOnWtrDerivDesc1 EmptyS (S "over area" +:+ (E $ sy coil_SA)) EmptyS assumpNIHGBW,
  eBalanceOnWtrDerivDesc2 dd1HtFluxC,
  eBalanceOnWtrDerivDesc3 eq1,
  eBalanceOnWtrDerivDesc4 eq2]

eBalanceOnWtrDerivDesc2 :: DataDefinition -> [Sentence]
eBalanceOnWtrDerivDesc2 dd =
  [S "Using", makeRef2S dd, S ", this can be written as"]

eBalanceOnWtrDerivDesc3 :: Expr-> [Sentence]
eBalanceOnWtrDerivDesc3 eq = [S "Dividing (3) by", (E eq) `sC` S "we obtain"]

eBalanceOnWtrDerivDesc4 :: [Sentence]-> [Sentence]
eBalanceOnWtrDerivDesc4 eq = 
  [S "Setting"] ++ eq ++ [S ", Equation (4) can be written in its final form as"]

eq1:: Expr
eq1 = (sy w_mass) * (sy htCap_W)

eq2:: [Sentence]
eq2 = [ch tau_W, S "=", ch w_mass, ch htCap_W, S "/", ch coil_HTC, ch coil_SA]

eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4 :: Expr

eBalanceOnWtrDerivEqn1 = (sy w_mass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy ht_flux_C) * (sy coil_SA)

eBalanceOnWtrDerivEqn2 = (sy w_mass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy coil_HTC) * (sy coil_SA) *  ((sy temp_C) - (sy temp_W))

eBalanceOnWtrDerivEqn3 = (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W))

eBalanceOnWtrDerivEqn4 =  
  (deriv (sy temp_W) time) $= 1 / (sy tau_W) * ((sy temp_C) - (sy temp_W))

eBalanceOnWtrDerivEqns :: [Expr]
eBalanceOnWtrDerivEqns = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4]

-----------
-- Intro --
-----------

instModIntro :: Sentence
instModIntro = foldlSent [S "The", phrase goal, makeRef2S waterTempGS,
  S "is met by", makeRef2S eBalanceOnWtr `andThe` phrase goal,
  makeRef2S waterEnergyGS, S "is met by", makeRef2S heatEInWtr]