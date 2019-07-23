module Drasil.SWHS.TMods (tMods, consThermE, sensHtE, sensHtETemplate, latentHtE, PhaseChange(Liquid)) where

import Language.Drasil
import Control.Lens ((^.))
import Theory.Drasil (TheoryModel, tm)
import Utils.Drasil

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

import Data.Drasil.SI_Units (joule)
import Drasil.SWHS.Assumptions (assumpTEO)
import Drasil.SWHS.Concepts (transient)
import Drasil.SWHS.DataDefs (ddHtFusion)
import Drasil.SWHS.Unitals (meltFrac, tau, deltaT, htCapV, htCapS,
  htCapL, volHtGen, thFluxVect)

tMods :: [TheoryModel]
tMods = [consThermE, sensHtE, latentHtE]

-------------------------
-- Theoretical Model 1 --
-------------------------
consThermE :: TheoryModel
consThermE = tm consThermERC
  [qw thFluxVect, qw gradient, qw volHtGen, 
    qw density, qw heatCapSpec, qw temp, qw time] ([] :: [ConceptChunk])
  [] [consThermERel] [] [consThemESrc] "consThermE" [consThermEdesc]

consThermERC :: RelationConcept
consThermERC = makeRC "consThermERC"
  (nounPhraseSP "Conservation of thermal energy") consThermEdesc consThermERel 

consThermERel :: Relation
consThermERel = negate (sy gradient) $. sy thFluxVect + sy volHtGen $=
  sy density * sy heatCapSpec * pderiv (sy temp) time

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
  ch volHtGen `isThe` phrase volHtGen +:+ sParen (Sy (unit_symb volHtGen)),
  ch temp `isThe` phrase temp +:+ sParen (Sy (unit_symb temp)),
  ch time +:+ S "is" +:+ phrase time +:+ sParen (Sy (unit_symb time)), ch gradient +:+
  S "is the" +:+ (gradient ^. defn)], S "For this", phrase equation, S "to apply" `sC`
  S "other forms of", phrase energy `sC` S "such as", phrase mechEnergy `sC`
  S "are assumed to be negligible in the", phrase system, sParen (makeRef2S assumpTEO)]

-------------------------
-- Theoretical Model 2 --
-------------------------

sensHtE :: TheoryModel
sensHtE = sensHtETemplate AllPhases sensHtEdesc

data PhaseChange = AllPhases
                 | Liquid

sensHtETemplate :: PhaseChange -> Sentence -> TheoryModel
sensHtETemplate pc desc = tm (sensHtERC pc eqn desc)
  [qw sensHeat, qw htCapS, qw mass, 
    qw deltaT, qw meltPt, qw temp, qw htCapL, qw boilPt, qw htCapV] ([] :: [ConceptChunk])
  [] [eqn] [] [sensHtESrc] "sensHtE" [desc] where
    eqn = sensHtEEqn pc


sensHtERC :: PhaseChange -> Relation -> Sentence -> RelationConcept
sensHtERC pc eqn desc = makeRC "sensHtERC" (nounPhraseSP ("Sensible heat energy" ++ case pc of
  Liquid -> " (no state change)"
  AllPhases -> "")) desc eqn

sensHtESrc :: Reference
sensHtESrc = makeURI "sensHtESrc"
  "http://en.wikipedia.org/wiki/Sensible_heat" $
  shortname' "Definition of Sensible Heat"

sensHtEEqn :: PhaseChange -> Relation
sensHtEEqn pChange = sy sensHeat $= case pChange of
  Liquid -> liquidFormula
  AllPhases -> incompleteCase [(sy htCapS * sy mass * sy deltaT,
      sy temp $< sy meltPt), (liquidFormula, sy meltPt $< sy temp $<
      sy boilPt), (sy htCapV * sy mass *
      sy deltaT, sy boilPt $< sy temp)]
  where
    liquidFormula = sy htCapL * sy mass * sy deltaT

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

--Figured out why so many were defn and others were term. The unitals
-- were implemented incorrectly.
sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [
  ch sensHeat `isThe` S "change in",
  phrase sensHeat, phrase energy +:+. sParen (Sy (usymb joule)),
  ch htCapS `sC` ch htCapL `sC` ch htCapV, S "are the",
  phrase htCapS `sC` phrase htCapL `sC` S "and", phrase htCapV `sC`
  S "respectively" +:+. sParen (Sy (unit_symb heatCapSpec)),
  ch mass `isThe` phrase mass +:+. sParen (Sy (unit_symb mass)),
  ch temp `isThe` phrase temp,
  sParen (Sy (unit_symb temp)) `sC` S "and", ch deltaT `isThe`
  phrase deltaT +:+. sParen (Sy (unit_symb deltaT)),
  ch meltPt `sAnd` ch boilPt,
  S "are the", phrase meltPt `sAnd` phrase boilPt `sC`
  S "respectively" +:+. sParen (Sy (unit_symb temp)),
  atStart sensHeat :+: S "ing occurs as long as the material does",
  S "not reach a", phrase temp, S "where a" +:+
  phrase phaseChange, S "occurs. A",
  phrase phaseChange, S "occurs if",
  ch temp :+: S "=" :+: ch boilPt,
  S "or", ch temp :+: S "=" +:+. ch meltPt,
  S "If this" `isThe` S "case, refer to",
  makeRef2S latentHtE `sC` atStart latentHeat, phrase energy]
 
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
latentHtE = tm latentHtERC
  [qw latentHeat, qw time, qw tau] ([] :: [ConceptChunk])
  [] [latHtEEqn] [] [latHtESrc] "latentHtE" [latentHtEdesc]

latentHtERC :: RelationConcept
latentHtERC = makeRC "latentHtERC"
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
  S "depends on the", phrase meltFrac `sC`
  makeRef2S ddHtFusion :+: S ".",
  ch meltPt `sAnd` ch boilPt, S "are the",
  phrase meltPt `sAnd` phrase boilPt `sC`
  S "respectively" +:+. sParen (Sy (unit_symb temp)),
  atStart latentHeat :+: S "ing stops when all material has",
  S "changed to the new phase"]
