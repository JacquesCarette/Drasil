module Drasil.SWHS.GenDefs (genDefs, nwtnCooling, rocTempSimp,
  rocTempSimpDeriv, nwtnCoolingDesc, rocTempSimpRC) where

import Prelude hiding (sin, cos, tan)

import Language.Drasil
import Theory.Drasil (GenDefn, gd, gdNoRefs)
import Utils.Drasil

import Data.Drasil.Concepts.Math (equation, rate, rOfChng, unit_)
import Data.Drasil.Concepts.Thermodynamics (lawConvCooling)

import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Data.Drasil.Quantities.PhysicalProperties as QPP (vol, mass, density)
import Data.Drasil.Quantities.Physics as QP (time)
import Data.Drasil.Quantities.Thermodynamics as QT (htFlux, heatCapSpec,
  temp)

import Data.Drasil.Units.Thermodynamics (thermalFlux)

import Drasil.SWHS.Assumptions (assumpHTCC, assumpCWTAT, assumpTPCAV,
  assumpDWPCoV, assumpSHECoV)
import Drasil.SWHS.Concepts (gaussDiv)
import Drasil.SWHS.References (incroperaEtAl2007)
import Drasil.SWHS.TMods (consThermE)
import Drasil.SWHS.Unitals (deltaT, htFluxIn, htFluxOut, htTransCoeff, inSA,
  outSA, tempEnv, thFluxVect, volHtGen)

---------------------------
--  General Definitions  --
---------------------------

--FIXME: genDefs, nwtnCoolingGD, and rocTempSimpGD were added--
--since referencing implementation for RelationConcept hasn't--
--stabilized yet (since RelationConcept isn't an instance of --
--the Referable class.                                       --
genDefs :: [GenDefn]
genDefs = [nwtnCooling, rocTempSimp] 

-- FIXME: page reference
nwtnCooling, rocTempSimp :: GenDefn
nwtnCooling = gd nwtnCoolingRC (Just thermalFlux) ([] :: Derivation) 
  [makeCiteInfo incroperaEtAl2007 $ Page [8]] "nwtnCooling" nwtnCoolingDesc

rocTempSimp = gdNoRefs rocTempSimpRC (Nothing :: Maybe UnitDefn)
  (rocTempSimpDeriv genDefDeriv4SWHS [assumpCWTAT, assumpTPCAV, assumpDWPCoV, assumpSHECoV])
  "rocTempSimp" [{-Notes-}]

--

nwtnCoolingRC :: RelationConcept
nwtnCoolingRC = makeRC "nwtnCooling" (nounPhraseSP "Newton's law of cooling") 
  EmptyS nwtnCoolingRel -- nwtnCoolingL

nwtnCoolingRel :: Relation
nwtnCoolingRel = apply1 htFlux QP.time $= sy htTransCoeff *
  apply1 deltaT QP.time

nwtnCoolingDesc :: [Sentence]
nwtnCoolingDesc = map foldlSent [
  [atStart lawConvCooling +:+. S "describes convective cooling from a surface" +:
   S "The law is stated as", S "the", phrase rate `sOf` S "heat loss from a body" `sIs`
   S "proportional to the difference in", plural temp, S "between the body and its surroundings"],
  [ch htTransCoeff, S "is assumed to be independent" `sOf` ch QT.temp,
   sParen (S "from" +:+ makeRef2S assumpHTCC)],
  [E (apply1 deltaT QP.time $= apply1 temp QP.time - apply1 tempEnv QP.time) `isThe`
   S "time-dependant thermal gradient between the environment and the object"]]

--
rocTempSimpRC :: RelationConcept
rocTempSimpRC = makeRC "rocTempSimp" (nounPhraseSP $ "Simplified rate " ++
  "of change of temperature") EmptyS rocTempSimpRel -- rocTempSimpL

rocTempSimpRel :: Relation
rocTempSimpRel = sy QPP.mass * sy QT.heatCapSpec *
  deriv (sy QT.temp) QP.time $= sy htFluxIn * sy inSA -
  sy htFluxOut * sy outSA + sy volHtGen * sy QPP.vol

---------------------------------------
--  General Definitions  Derivation  --
---------------------------------------

rocTempSimpDeriv :: Sentence -> [ConceptInstance] -> Derivation
rocTempSimpDeriv s a = S "Detailed derivation of simplified" +:
  (phrase rOfChng `sOf` phrase temp) :
  weave [rocTempSimpDerivSent s a, map E rocTempSimpDerivEqns]

rocTempSimpDerivSent :: Sentence -> [ConceptInstance] -> [Sentence]
rocTempSimpDerivSent s a = map foldlSentCol [genDefDeriv1, genDefDeriv2,
  genDefDeriv3, genDefDeriv4 s a, genDefDeriv5]

genDefDeriv1 :: [Sentence]
genDefDeriv1 = [S "Integrating", makeRef2S consThermE, S "over a",
  phrase vol, sParen (ch vol) `sC` S "we have"]

genDefDeriv2 :: [Sentence]
genDefDeriv2 = [S "Applying", titleize gaussDiv, S "to the first term over",
  (phrase surface +:+ ch surface `ofThe` phrase vol) `sC` S "with",
  ch thFluxVect, S "as the", phrase thFluxVect, S "for the",
  phrase surface `sAnd` ch uNormalVect, S "as a", phrase unit_,
  S "outward", phrase uNormalVect, S "for a", phrase surface]

genDefDeriv3 :: [Sentence]
genDefDeriv3 = [S "We consider an arbitrary" +:+. phrase vol, S "The",
  phrase volHtGen, S "is assumed constant. Then (1) can be written as"]

genDefDeriv4 :: Sentence -> [ConceptInstance] -> [Sentence]
genDefDeriv4 s assumps = [S "Where", 
  foldlList Comma List (map ch [htFluxIn, htFluxOut, inSA, outSA]),
  S "are explained in" +:+. makeRef2S rocTempSimp, s, S "Assuming", 
  foldlList Comma List (map ch [density, QT.heatCapSpec, QT.temp]),
  S "are constant over the", phrase vol `sC` S "which is true in our case by",
  foldlList Comma List (map makeRef2S assumps) `sC` S "we have"]

genDefDeriv4SWHS :: Sentence
genDefDeriv4SWHS = foldlSent [S "The integral over the", phrase surface,
  S "could be simplified because the thermal flux is assumed constant over",
  ch inSA `sAnd` ch outSA `sAnd` E 0, S "on all other" +:+. plural surface,
  S "Outward flux is considered positive"]

genDefDeriv5 :: [Sentence]
genDefDeriv5 = [S "Using the fact that", ch density :+: S "=" :+: ch mass :+:
  S "/" :+: ch vol `sC` S "(2) can be written as"]

genDefDerivEqn1, genDefDerivEqn2, genDefDerivEqn3,
  genDefDerivEqn4, genDefDerivEqn5 :: Expr

genDefDerivEqn1 = negate (intAll (eqSymb vol) (sy gradient $. sy thFluxVect)) + 
  intAll (eqSymb vol) (sy volHtGen) $=
  intAll (eqSymb vol) (sy density
  * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

genDefDerivEqn2 = negate (intAll (eqSymb surface) (sy thFluxVect $. sy uNormalVect)) +
  intAll (eqSymb vol) (sy volHtGen) $= 
  intAll (eqSymb vol)
  (sy density * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

genDefDerivEqn3 = sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol $= 
  intAll (eqSymb vol) (sy density * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

genDefDerivEqn4 = sy density * sy QT.heatCapSpec * sy vol * deriv
  (sy QT.temp) time $= sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol

genDefDerivEqn5 = sy mass * sy QT.heatCapSpec * deriv (sy QT.temp)
  time $= sy htFluxIn * sy inSA - sy htFluxOut
  * sy outSA + sy volHtGen * sy vol

rocTempSimpDerivEqns :: [Expr]
rocTempSimpDerivEqns = [genDefDerivEqn1, genDefDerivEqn2, genDefDerivEqn3,
  genDefDerivEqn4, genDefDerivEqn5]
