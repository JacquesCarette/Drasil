{-# LANGUAGE PostfixOperators #-}
module Drasil.SWHS.TMods (PhaseChange(Liquid), consThermE, latentHtE,
  nwtnCooling, sensHtE, sensHtETemplate, tMods, consThemESrc) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Control.Lens ((^.))
import Theory.Drasil (ConstraintSet, mkConstraintSet,
  TheoryModel, tm, equationalModel', equationalConstraints',
  ModelKind, equationalModel)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

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
import Drasil.SWHS.References
import Drasil.SWHS.Unitals (deltaT, htCapL, htCapS, htCapV, htTransCoeff,
  meltFrac, tau, tempEnv, thFluxVect, volHtGen)

tMods :: [TheoryModel]
tMods = [consThermE, sensHtE, latentHtE, nwtnCooling]

-------------------------
-- Theoretical Model 1 --
-------------------------
consThermE :: TheoryModel
consThermE = tm (equationalConstraints' consThermECS)
  [qw thFluxVect, qw gradient, qw volHtGen,
    qw density, qw heatCapSpec, qw temp, qw time] ([] :: [ConceptChunk])
  [] [express consThermERel] [] [dRef consThemESrc] "consThermE" consThermENotes

consThermECS :: ConstraintSet ModelExpr
consThermECS = mkConstraintSet consCC rels
  where consCC = dccWDS "consThermECS"
          (nounPhraseSP "Conservation of thermal energy") (lawConsEnergy ^. defn)
        rels   = NE.fromList [consThermERel]

consThermERel :: ModelExpr
consThermERel = negVec (sy gradient) $. sy thFluxVect $+ sy volHtGen $=
  sy density $* sy heatCapSpec $* pderiv (sy temp) time

consThermENotes :: [Sentence]
consThermENotes = map foldlSent [
  [S "The above", phrase equation, S "gives the", phrase lawConsEnergy,
   S "for", phrase transient, phrase heatTrans, S "in a given material"],
  [S "For this", phrase equation, S "to apply" `sC` S "other forms" `S.of_`
   phrase energy `sC` S "such as", phrase mechEnergy `sC` S "are assumed",
   S "to be negligible" `S.inThe` phrase system, sParen (refS assumpTEO)]]

-------------------------
-- Theoretical Model 2 --
-------------------------
sensHtE :: TheoryModel
sensHtE = sensHtETemplate AllPhases sensHtEdesc

data PhaseChange = AllPhases
                 | Liquid

sensHtETemplate :: PhaseChange -> Sentence -> TheoryModel
sensHtETemplate pc desc = tm (equationalModel' qd)
  [qw sensHeat, qw htCapS, qw mass,
    qw deltaT, qw meltPt, qw temp, qw htCapL, qw boilPt, qw htCapV] ([] :: [ConceptChunk])
  [qd] [] [] [dRef sensHtESrc] "sensHtE" [desc]
    where
      qd = sensHtEQD pc eqn desc
      eqn = sensHtEEqn pc


sensHtEQD :: PhaseChange -> ModelExpr -> Sentence -> ModelQDef
sensHtEQD pc eqn desc = fromEqnSt'' "sensHeat" np desc (symbol sensHeat) (sensHeat ^. typ) eqn
  where np = nounPhraseSP ("Sensible heat energy" ++ case pc of
                                                       Liquid -> " (no state change)"
                                                       AllPhases -> "")

sensHtEEqn :: PhaseChange -> ModelExpr
sensHtEEqn pChange = case pChange of
  Liquid -> liquidFormula
  AllPhases -> incompleteCase [(sy htCapS $* sy mass $* sy deltaT,
      sy temp $< sy meltPt), (liquidFormula, sy meltPt $< sy temp $<
      sy boilPt), (sy htCapV $* sy mass $* 
      sy deltaT, sy boilPt $< sy temp)]
  where
    liquidFormula = sy htCapL $* sy mass $* sy deltaT

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

--Figured out why so many were defn and others were term. The unitals
-- were implemented incorrectly.
sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [
  atStart sensHeat :+: S "ing occurs as long as the material does not reach a",
  phrase temp, S "where a", phrase phaseChange, (S "occurs" !.), atStartNP (a_ phaseChange),
  S "occurs if" +:+. (eS (sy temp $= sy boilPt) `S.or_` eS (sy temp $= sy meltPt)),
  S "If this" `S.is` S "the case" `sC` S "refer to", refS latentHtE]

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
latentHtE = tm latentHtEMK
  [qw latentHeat, qw time, qw tau] ([] :: [ConceptChunk])
  [] [express latentHtEFD] [] [dRef latHtESrc] "latentHtE" latentHtENotes

latentHtEMK :: ModelKind ModelExpr
latentHtEMK = equationalModel "latentHtETM"
  (nounPhraseSP "Latent heat energy") latentHtEFD

latentHtEFD :: ModelQDef
latentHtEFD = mkFuncDefByQ latentHeat [time] latentHtEExpr

latentHtEExpr :: ModelExpr
latentHtEExpr = defint (eqSymb tau) (exactDbl 0) (sy time) (deriv (apply1 latentHeat tau) tau)

latentHtENotes :: [Sentence]
latentHtENotes = map foldlSent [
  [ch latentHeat `S.isThe` S "change" `S.in_` phrase thermalEnergy,
   sParen (phrase latentHeat +:+ phrase energy)],
  [eS' latentHtEFD `S.isThe` phrase rOfChng `S.of_` ch latentHeat `S.wrt` 
   phrase time, ch tau],
  [ch time `S.isThe` phrase time, S "elapsed" `sC` S "as long as the",
   phrase phaseChange `S.is` S "not complete"],
  [S "status" `S.the_ofTheC` phrase phaseChange, S "depends on the",
   phrase meltFrac, sParen (S "from" +:+ refS ddMeltFrac)],
  [atStart latentHeat :+: S "ing stops when all material has changed to the new phase"]]

-------------------------
-- Theoretical Model 4 --
-------------------------
nwtnCooling :: TheoryModel
nwtnCooling = tm nwtnCoolingMK
  [qw latentHeat, qw time, qw htTransCoeff, qw deltaT] ([] :: [ConceptChunk])
  [] [express nwtnCoolingFD] [] [dRefInfo incroperaEtAl2007 $ Page [8]]
  "nwtnCooling" nwtnCoolingNotes

nwtnCoolingMK :: ModelKind ModelExpr
nwtnCoolingMK = equationalModel "nwtnCoolingTM"
  (nounPhraseSP "Newton's law of cooling") nwtnCoolingFD

nwtnCoolingFD :: ModelQDef
nwtnCoolingFD = mkFuncDefByQ htFlux [time] nwtnCoolingExpr

nwtnCoolingExpr :: ModelExpr
nwtnCoolingExpr = sy htTransCoeff $* apply1 deltaT time

nwtnCoolingNotes :: [Sentence]
nwtnCoolingNotes = map foldlSent [
  [atStart lawConvCooling +:+. S "describes convective cooling from a surface" +:
   (S "The law" `S.is` S "stated as"), S "the", phrase rate `S.of_` S "heat loss from a body" `S.is`
   S "proportional" `S.toThe` S "difference in", plural temp, S "between the body and its surroundings"],
  [ch htTransCoeff `S.is` S "assumed to be independent" `S.of_` ch temp,
   sParen (S "from" +:+ refS assumpHTCC)],
  [E (defines (apply1 deltaT time) (apply1 temp time $- apply1 tempEnv time)) `S.isThe`
   S "time-dependant thermal gradient between the environment and the object"]]
