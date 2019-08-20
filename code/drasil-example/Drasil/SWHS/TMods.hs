module Drasil.SWHS.TMods (PhaseChange(Liquid), consThermE, latentHtE,
  nwtnCooling, sensHtE, sensHtETemplate, tMods) where

import Language.Drasil
import Control.Lens ((^.))
import Theory.Drasil (TheoryModel, tm)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (system)
import Data.Drasil.Concepts.Math (equation, rate, rOfChng)
import Data.Drasil.Concepts.Physics (mechEnergy)
import Data.Drasil.Concepts.Thermodynamics (heatTrans, lawConsEnergy,
  lawConvCooling, phaseChange, thermalEnergy)

import Data.Drasil.Quantities.Math (gradient)
import Data.Drasil.Quantities.PhysicalProperties (density, mass)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Quantities.Thermodynamics (boilPt, heatCapSpec,
  htFlux, latentHeat, meltPt, sensHeat, temp)

import Drasil.SWHS.Assumptions (assumpHTCC, assumpTEO)
import Drasil.SWHS.Concepts (transient)
import Drasil.SWHS.DataDefs (ddMeltFrac)
import Drasil.SWHS.References (incroperaEtAl2007)
import Drasil.SWHS.Unitals (deltaT, htCapL, htCapS, htCapV, htTransCoeff,
  meltFrac, tau, tempEnv, thFluxVect, volHtGen)

tMods :: [TheoryModel]
tMods = [consThermE, sensHtE, latentHtE, nwtnCooling]

-------------------------
-- Theoretical Model 1 --
-------------------------
consThermE :: TheoryModel
consThermE = tm consThermERC
  [qw thFluxVect, qw gradient, qw volHtGen, 
    qw density, qw heatCapSpec, qw temp, qw time] ([] :: [ConceptChunk])
  [] [consThermERel] [] [consThemESrc] "consThermE" consThermENotes

consThermERC :: RelationConcept
consThermERC = makeRC "consThermERC"
  (nounPhraseSP "Conservation of thermal energy") (lawConsEnergy ^. defn) consThermERel 

consThermERel :: Relation
consThermERel = negate (sy gradient) $. sy thFluxVect + sy volHtGen $=
  sy density * sy heatCapSpec * pderiv (sy temp) time

-- the second argument is a 'ShortName'...
consThemESrc :: Reference
consThemESrc = makeURI "consThemESrc"
  "http://www.efunda.com/formulae/heat_transfer/conduction/overview_cond.cfm" $
  shortname' "Fourier Law of Heat Conduction and Heat Equation"

consThermENotes :: [Sentence]
consThermENotes = map foldlSent [
  [S "The above", phrase equation, S "gives the", phrase lawConsEnergy,
   S "for", phrase transient, phrase heatTrans, S "in a given material"],
  [S "For this", phrase equation, S "to apply" `sC` S "other forms" `sOf`
   phrase energy `sC` S "such as", phrase mechEnergy `sC` S "are assumed",
   S "to be negligible" `inThe` phrase system, sParen (makeRef2S assumpTEO)]]

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
  atStart sensHeat :+: S "ing occurs as long as the material does not reach a",
  phrase temp, S "where a", phrase phaseChange, S "occurs. A", phrase phaseChange,
  S "occurs if" +:+. (E (sy temp $= sy boilPt) `sOr` E (sy temp $= sy meltPt)),
  S "If this is the case" `sC` S "refer to", makeRef2S latentHtE]
 
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
  [] [latHtEEqn] [] [latHtESrc] "latentHtE" latentHtENotes

latentHtERC :: RelationConcept
latentHtERC = makeRC "latentHtERC"
  (nounPhraseSP "Latent heat energy") (latentHeat ^. defn) latHtEEqn 

latHtEEqn :: Relation
latHtEEqn = apply1 latentHeat time $= 
  defint (eqSymb tau) 0 (sy time) (deriv (apply1 latentHeat tau) tau)

-- Integrals need dTau at end

latHtESrc :: Reference
latHtESrc = makeURI "latHtESrc" "http://en.wikipedia.org/wiki/Latent_heat" $
  shortname' "Definition of Latent Heat"

latentHtENotes :: [Sentence]
latentHtENotes = map foldlSent [
  [ch latentHeat `isThe` S "change" `sIn` phrase thermalEnergy,
   sParen (phrase latentHeat +:+ phrase energy)],
  [E latHtEEqn `isThe` phrase rOfChng `sOf` ch latentHeat,
   S "with respect to", phrase time, ch tau],
  [ch time `isThe` phrase time, S "elapsed" `sC` S "as long as the",
   phrase phaseChange, S "is not complete"],
  [S "status" `ofThe'` phrase phaseChange, S "depends on the",
   phrase meltFrac, sParen (S "from" +:+ makeRef2S ddMeltFrac)],
  [atStart latentHeat :+: S "ing stops when all material has changed to the new phase"]]

-------------------------
-- Theoretical Model 4 --
-------------------------
nwtnCooling :: TheoryModel
nwtnCooling = tm nwtnCoolingRC
  [qw latentHeat, qw time, qw htTransCoeff, qw deltaT] ([] :: [ConceptChunk])
  [] [nwtnCoolingEqn] [] [makeCiteInfo incroperaEtAl2007 $ Page [8]]
  "nwtnCooling" nwtnCoolingNotes

nwtnCoolingRC :: RelationConcept
nwtnCoolingRC = makeRC "nwtnCoolingRC" (nounPhraseSP "Newton's law of cooling") 
  EmptyS nwtnCoolingEqn -- nwtnCoolingL

nwtnCoolingEqn :: Relation
nwtnCoolingEqn = apply1 htFlux time $= sy htTransCoeff * apply1 deltaT time

nwtnCoolingNotes :: [Sentence]
nwtnCoolingNotes = map foldlSent [
  [atStart lawConvCooling +:+. S "describes convective cooling from a surface" +:
   S "The law is stated as", S "the", phrase rate `sOf` S "heat loss from a body" `sIs`
   S "proportional to the difference in", plural temp, S "between the body and its surroundings"],
  [ch htTransCoeff, S "is assumed to be independent" `sOf` ch temp,
   sParen (S "from" +:+ makeRef2S assumpHTCC)],
  [E (apply1 deltaT time $= apply1 temp time - apply1 tempEnv time) `isThe`
   S "time-dependant thermal gradient between the environment and the object"]]
