module Drasil.SWHS.TMods (tMods, consThermE, sensHtE, sensHtE_template, latentHtE, PhaseChange(Liquid)) where

import Language.Drasil
import Control.Lens ((^.))
import Theory.Drasil (TheoryModel, tm)

import Data.Drasil.Concepts.Documentation (system)
import Data.Drasil.Concepts.Math (equation, rOfChng)
import Data.Drasil.Concepts.Physics (mechEnergy)
import Data.Drasil.Concepts.Thermodynamics (phaseChange, thermalEnergy,
  heatTrans, lawConsEnergy)

import Data.Drasil.Quantities.Math (gradient)
import Data.Drasil.Quantities.PhysicalProperties (mass, density)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Quantities.Thermodynamics (temp, heatCapSpec,
  latentHeat, meltPt, boilPt, sensHeat, heatCapSpec)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma),
  foldlList, foldlSent, isThe, sAnd)
import Data.Drasil.SI_Units (joule)
import Drasil.SWHS.Assumptions (assumpTEO)
import Drasil.SWHS.Concepts (transient)
import Drasil.SWHS.DataDefs (dd3HtFusion)
import Drasil.SWHS.Unitals (melt_frac, tau, deltaT, htCap_V, htCap_S,
  htCap_L, vol_ht_gen, thFluxVect)

tMods :: [TheoryModel]
tMods = [consThermE, sensHtE, latentHtE]

-------------------------
-- Theoretical Model 1 --
-------------------------
consThermE :: TheoryModel
consThermE = tm consThermE_rc
  [qw thFluxVect, qw gradient, qw vol_ht_gen, 
    qw density, qw heatCapSpec, qw temp, qw time] ([] :: [ConceptChunk])
  [] [consThermERel] [] [consThemESrc] "consThermE" [consThermEdesc]

consThermE_rc :: RelationConcept
consThermE_rc = makeRC "consThermE_rc"
  (nounPhraseSP "Conservation of thermal energy") consThermEdesc consThermERel 

consThermERel :: Relation
consThermERel = (negate (sy gradient)) $. (sy thFluxVect) + (sy vol_ht_gen) $=
  (sy density) * (sy heatCapSpec) * (pderiv (sy temp) time)

-- the second argument is a 'ShortName'...
consThemESrc :: Reference
consThemESrc = makeURI "consThemESrc"
  "http://www.efunda.com/formulae/heat_transfer/conduction/overview_cond.cfm" $
  shortname' "Fourier Law of Heat Conduction and Heat Equation"

consThermEdesc :: Sentence
consThermEdesc = foldlSent [
  S "The above", phrase equation, S "gives the", phrase lawConsEnergy, S "for",
  phrase transient, phrase heatTrans, S "in a material of", phrase heatCapSpec,
  ch heatCapSpec, sParen (Sy (unit_symb heatCapSpec)) `sAnd` phrase density `sC`
  ch density, sParen (Sy (unit_symb density)) `sC` S "where" +:+. 
  foldlList Comma List [ch thFluxVect `isThe` phrase thFluxVect +:+ sParen (Sy (unit_symb thFluxVect)),
  ch vol_ht_gen `isThe` phrase vol_ht_gen +:+ sParen (Sy (unit_symb vol_ht_gen)),
  ch temp `isThe` phrase temp +:+ sParen (Sy (unit_symb temp)),
  ch time +:+ S "is" +:+ phrase time +:+ sParen (Sy (unit_symb time)), ch gradient +:+
  S "is the" +:+ (gradient ^. defn)], S "For this", phrase equation, S "to apply" `sC`
  S "other forms of", phrase energy `sC` S "such as", phrase mechEnergy `sC`
  S "are assumed to be negligible in the", phrase system, sParen (makeRef2S assumpTEO)]

-------------------------
-- Theoretical Model 2 --
-------------------------

sensHtE :: TheoryModel
sensHtE = sensHtE_template AllPhases sensHtEdesc

data PhaseChange = AllPhases
                 | Liquid

sensHtE_template :: PhaseChange -> Sentence -> TheoryModel
sensHtE_template pc desc = tm (sensHtE_rc pc eqn desc)
  [qw sensHeat, qw htCap_S, qw mass, 
    qw deltaT, qw meltPt, qw temp, qw htCap_L, qw boilPt, qw htCap_V] ([] :: [ConceptChunk])
  [] [eqn] [] [sensHtESrc] "sensHtE" [desc] where
    eqn = sensHtEEqn pc


sensHtE_rc :: PhaseChange -> Relation -> Sentence -> RelationConcept
sensHtE_rc pc eqn desc = makeRC "sensHtE_rc" (nounPhraseSP ("Sensible heat energy" ++ case pc of
  Liquid -> " (no state change)"
  AllPhases -> "")) desc eqn

sensHtESrc :: Reference
sensHtESrc = makeURI "sensHtESrc"
  "http://en.wikipedia.org/wiki/Sensible_heat" $
  shortname' "Definition of Sensible Heat"

sensHtEEqn :: PhaseChange -> Relation
sensHtEEqn pChange = (sy sensHeat) $= case pChange of
  Liquid -> liquidFormula
  AllPhases -> case_ [((sy htCap_S) * (sy mass) * (sy deltaT),
      ((sy temp) $< (sy meltPt))), (liquidFormula, ((sy meltPt) $< (sy temp) $<
      (sy boilPt))), ((sy htCap_V) * (sy mass) *
      (sy deltaT), ((sy boilPt) $< (sy temp)))]
  where
    liquidFormula = (sy htCap_L) * (sy mass) * (sy deltaT)

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

--Figured out why so many were defn and others were term. The unitals
-- were implemented incorrectly.
sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [
  ch sensHeat `isThe` S "change in",
  phrase sensHeat, phrase energy +:+. sParen (Sy (usymb joule)),
  ch htCap_S `sC` ch htCap_L `sC` ch htCap_V, S "are the",
  phrase htCap_S `sC` phrase htCap_L `sC` S "and", phrase htCap_V `sC`
  S "respectively" +:+. sParen (Sy (unit_symb heatCapSpec)),
  ch mass `isThe` phrase mass +:+. sParen (Sy (unit_symb mass)),
  ch temp `isThe` phrase temp,
  sParen (Sy (unit_symb temp)) `sC` S "and", ch deltaT `isThe`
  phrase deltaT +:+. sParen (Sy (unit_symb deltaT)),
  ch meltPt `sAnd` ch boilPt,
  S "are the", phrase meltPt `sAnd` phrase boilPt `sC`
  S "respectively" +:+. sParen (Sy (unit_symb temp)),
  at_start sensHeat :+: S "ing occurs as long as the material does",
  S "not reach a", phrase temp, S "where a" +:+
  phrase phaseChange, S "occurs. A",
  phrase phaseChange, S "occurs if",
  ch temp :+: S "=" :+: ch boilPt,
  S "or", ch temp :+: S "=" +:+. ch meltPt,
  S "If this" `isThe` S "case, refer to",
  (makeRef2S latentHtE) `sC` at_start latentHeat,
  phrase energy]
 
--How to have new lines in the description?
--Can't have relation and eqn chunks together since they are called in a list
----You can, you just can't map "Definition" over a list
---- you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?

-------------------------
-- Theoretical Model 3 --
-------------------------
latentHtE :: TheoryModel
latentHtE = tm latentHtE_rc
  [qw latentHeat, qw time, qw tau] ([] :: [ConceptChunk])
  [] [latHtEEqn] [] [latHtESrc] "latentHtE" [latentHtEdesc]

latentHtE_rc :: RelationConcept
latentHtE_rc = makeRC "latentHtE_rc"
  (nounPhraseSP "Latent heat energy") latentHtEdesc latHtEEqn 

latHtEEqn :: Relation
latHtEEqn = apply1 latentHeat time $= 
  defint (eqSymb tau) 0 (sy time) (deriv (apply1 latentHeat tau) tau)

-- Integrals need dTau at end

latHtESrc :: Reference
latHtESrc = makeURI "latHtESrc" "http://en.wikipedia.org/wiki/Latent_heat" $
  shortname' "Definition of Latent Heat"

latentHtEdesc :: Sentence
latentHtEdesc = foldlSent [
  ch latentHeat `isThe` S "change in",
  phrase thermalEnergy, sParen (Sy (usymb joule)) `sC`
  phrase latentHeat +:+. phrase energy, 
  E latHtEEqn `isThe` phrase rOfChng, S "of",
  ch latentHeat, S "with respect to", phrase time,
  ch tau +:+. sParen (Sy (unit_symb tau)), ch time `isThe`
  phrase time, sParen (Sy (unit_symb time)),
  S "elapsed, as long as the",
  phrase phaseChange, S "is not complete. The status of",
  S "the", phrase phaseChange,
  S "depends on the", phrase melt_frac `sC`
  (makeRef2S dd3HtFusion) :+: S ".",
  ch meltPt `sAnd` ch boilPt, S "are the",
  phrase meltPt `sAnd` phrase boilPt `sC`
  S "respectively" +:+. sParen (Sy (unit_symb temp)),
  at_start latentHeat :+: S "ing stops when all material has",
  S "changed to the new phase"]
