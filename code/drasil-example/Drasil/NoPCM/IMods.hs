module Drasil.NoPCM.IMods (eBalanceOnWtr, iMods, instModIntro) where

import Language.Drasil
import Theory.Drasil (DataDefinition, InstanceModel, im)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (goal)
import Data.Drasil.Concepts.Math (equation, rOfChng)
import Data.Drasil.Concepts.PhysicalProperties (liquid)
import Data.Drasil.Concepts.Thermodynamics (melting, boilPt, heatCapSpec, 
  heatTrans)

import Data.Drasil.Quantities.PhysicalProperties (mass, vol)
import Data.Drasil.Quantities.Physics (energy, time)

import Data.Drasil.SentenceStructures (foldlSent, foldlSentCol)
import Data.Drasil.Utils (unwrap, weave)

import Drasil.SWHS.Concepts (water, tank)
import Drasil.SWHS.DataDefs (dd1HtFluxC)
import Drasil.SWHS.IMods (heatEInWtr)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.Unitals (temp_W, tempC, tauW, wMass, htCapW, coilHTC, 
  coilSA, tempInit, timeFinal, wVol, htFluxC, volHtGen)
import Drasil.NoPCM.Assumptions (assumpCTNTD, assumpNIHGBW, assumpWAL)
import Drasil.NoPCM.GenDefs (rocTempSimp)
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
  [sy tempInit $<= sy tempC] (qw temp_W) 
  --Tw(0) cannot be presented, there is one more constraint Tw(0) = Tinit
  [0 $< sy time $< sy timeFinal] [makeCiteInfo koothoor2013 $ RefNote "with PCM removed"] 
  eBalanceOnWtrDeriv "eBalanceOnWtr" [balWtrDesc]

eBalanceOnWtrRC :: RelationConcept
eBalanceOnWtrRC = makeRC "eBalanceOnWtrRC" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtrRel
  -- (mkLabelSame "eBalnaceOnWtr" (Def Instance))

balWtrRel :: Relation
balWtrRel = (deriv (sy temp_W) time) $= 1 / (sy tauW) *
  (((sy tempC) - (apply1 temp_W time)))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), 
  (E $ sy tempC) `isThe` phrase tempC +:+. sParen (unwrap $ getUnit tempC),
  (E $ sy tauW $= (sy wMass * sy htCapW) / (sy coilHTC * sy coilSA)),
  S "is a constant" +:+. sParen (unwrap $ getUnit tauW),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ 0 $< sy temp_W $< 100),
  sParen (unwrap $ getUnit temp_W), S "where", E 0,
  sParen (unwrap $ getUnit temp_W) `sAnd` (E 100),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boilPt, S "of", phrase water `sC` S "respectively"
  +:+ sParen (makeRef2S assumpWAL)]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv =
  S "Derivation of the" +:+ phrase energy +:+ S "balance on water:" :
  (weave [eBalanceOnWtrDerivSentences, map E eBalanceOnWtrDerivEqns])

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = map foldlSentCol [
  eBalanceOnWtrDerivDesc1 rOfChng temp_W energy water vol wVol mass wMass heatCapSpec
    htCapW heatTrans htFluxC coilSA tank assumpCTNTD assumpNIHGBW volHtGen,
  eBalanceOnWtrDerivDesc2 dd1HtFluxC,
  eBalanceOnWtrDerivDesc3 eq1,
  eBalanceOnWtrDerivDesc4 eq2]

eBalanceOnWtrDerivDesc1 :: ConceptChunk -> ConstrConcept -> UnitalChunk -> ConceptChunk -> UnitalChunk -> 
  UnitalChunk -> UnitalChunk -> UnitalChunk -> ConceptChunk -> UncertQ -> ConceptChunk -> 
  UnitalChunk -> UncertQ -> ConceptChunk -> ConceptInstance -> ConceptInstance -> UnitalChunk -> [Sentence]
eBalanceOnWtrDerivDesc1 roc tw en wt vo wvo ms wms hcs hw ht hfc cs tk ass11 ass16 vhg =
  [S "To find the", phrase roc `sOf` (E $ sy tw) `sC` S "we look at the",
   phrase en, S "balance on" +:+. phrase wt, S "The", phrase vo, S "being considered" 
   `isThe` (phrase vo `sOf` phrase wt), (E $ sy wvo) `sC` S "which has", phrase ms,
   (E $ sy wms) `sAnd` (phrase hcs `sOf` phrase wt) `sC` (E $ sy hw), 
    S ". Heat transfer occurs in the", phrase wt, S "from the coil as", 
    (E $ sy hfc) `sC` S "over area" +:+. (E $ sy cs), S "No", phrase ht,
    S "occurs to", S "outside" `ofThe` phrase tk `sC` 
    S "since it has been assumed to be perfectly insulated", 
    (sParen (makeRef2S ass11)), S ". Assuming no volumetric", 
    S "heat generation per unit", phrase vo,
    (sParen (makeRef2S ass16)) `sC` (E $ sy vhg $= 0), S ". Therefore, the equation for",
     makeRef2S rocTempSimp, S "can be written as"]

eBalanceOnWtrDerivDesc2 :: DataDefinition -> [Sentence]
eBalanceOnWtrDerivDesc2 dd =
  [S "Using", makeRef2S dd, S ", this can be written as"]

eBalanceOnWtrDerivDesc3 :: Expr-> [Sentence]
eBalanceOnWtrDerivDesc3 eq = [S "Dividing (3) by", (E eq) `sC` S "we obtain"]

eBalanceOnWtrDerivDesc4 :: [Sentence]-> [Sentence]
eBalanceOnWtrDerivDesc4 eq = 
  [S "Setting"] ++ eq ++ [S ", Equation (4) can be written in its final form as"]

eq1:: Expr
eq1 = (sy wMass) * (sy htCapW)

eq2:: [Sentence]
eq2 = [ch tauW, S "=", ch wMass, ch htCapW, S "/", ch coilHTC, ch coilSA]

eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4 :: Expr

eBalanceOnWtrDerivEqn1 = (sy wMass) * (sy htCapW) * (deriv (sy temp_W) time) $= 
  (sy htFluxC) * (sy coilSA)

eBalanceOnWtrDerivEqn2 = (sy wMass) * (sy htCapW) * (deriv (sy temp_W) time) $= 
  (sy coilHTC) * (sy coilSA) *  ((sy tempC) - (sy temp_W))

eBalanceOnWtrDerivEqn3 = (deriv (sy temp_W) time) $= 
  ((sy coilHTC) * (sy coilSA) / 
  ((sy wMass) * (sy htCapW))) *  ((sy tempC) - (sy temp_W))

eBalanceOnWtrDerivEqn4 =  
  (deriv (sy temp_W) time) $= 1 / (sy tauW) * ((sy tempC) - (sy temp_W))

eBalanceOnWtrDerivEqns :: [Expr]
eBalanceOnWtrDerivEqns = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4]

-----------
-- Intro --
-----------

instModIntro :: Sentence
instModIntro = foldlSent [S "The", phrase goal, makeRef2S waterTempGS,
  S "is met by", makeRef2S eBalanceOnWtr `andThe` phrase goal,
  makeRef2S waterEnergyGS, S "is met by", makeRef2S heatEInWtr]