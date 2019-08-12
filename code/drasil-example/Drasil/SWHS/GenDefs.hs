module Drasil.SWHS.GenDefs (genDefs, rocTempSimp, rocTempSimpDeriv, rocTempSimpRC) where

import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)
import Utils.Drasil

import Data.Drasil.Concepts.Math (rOfChng, unit_)

import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Data.Drasil.Quantities.PhysicalProperties as QPP (vol, mass, density)
import Data.Drasil.Quantities.Physics as QP (time)
import Data.Drasil.Quantities.Thermodynamics as QT (heatCapSpec, temp)

import Drasil.SWHS.Assumptions (assumpCWTAT, assumpTPCAV, assumpDWPCoV, assumpSHECoV)
import Drasil.SWHS.Concepts (gaussDiv)
import Drasil.SWHS.TMods (consThermE)
import Drasil.SWHS.Unitals (htFluxIn, htFluxOut, inSA, outSA, thFluxVect, volHtGen)

---------------------------
--  General Definitions  --
---------------------------

--FIXME: genDefs, nwtnCoolingGD, and rocTempSimpGD were added--
--since referencing implementation for RelationConcept hasn't--
--stabilized yet (since RelationConcept isn't an instance of --
--the Referable class.                                       --
genDefs :: [GenDefn]
genDefs = [rocTempSimp] 

rocTempSimp :: GenDefn
rocTempSimp = gdNoRefs rocTempSimpRC (Nothing :: Maybe UnitDefn)
  (Just $ rocTempSimpDeriv rocTempDerivConsFlxSWHS [assumpCWTAT, assumpTPCAV, assumpDWPCoV, assumpSHECoV])
  "rocTempSimp" [{-Notes-}]

rocTempSimpRC :: RelationConcept
rocTempSimpRC = makeRC "rocTempSimp" (nounPhraseSP $ "Simplified rate " ++
  "of change of temperature") EmptyS rocTempSimpRel -- rocTempSimpL

rocTempSimpRel :: Relation
rocTempSimpRel = sy QPP.mass * sy QT.heatCapSpec *
  deriv (sy QT.temp) QP.time $= sy htFluxIn * sy inSA -
  sy htFluxOut * sy outSA + sy volHtGen * sy QPP.vol

--------------------------------------
--  General Definitions Derivation  --
--------------------------------------

rocTempSimpDeriv :: Sentence -> [ConceptInstance] -> Derivation
rocTempSimpDeriv s a = mkDerivName (S "simplified" +:+ phrase rOfChng `sOf` phrase temp)
  (weave [rocTempSimpDerivSent s a, map E rocTempSimpDerivEqns])

rocTempSimpDerivSent :: Sentence -> [ConceptInstance] -> [Sentence]
rocTempSimpDerivSent s a = map foldlSentCol [rocTempDerivInteg, rocTempDerivGauss,
  rocTempDerivArbVol, rocTempDerivConsFlx s a, rocTempDerivDens]

rocTempDerivInteg :: [Sentence]
rocTempDerivInteg = [S "Integrating", makeRef2S consThermE, S "over a",
  phrase vol, sParen (ch vol) `sC` S "we have"]

rocTempDerivGauss :: [Sentence]
rocTempDerivGauss = [S "Applying", titleize gaussDiv, S "to the first term over",
  (phrase surface +:+ ch surface `ofThe` phrase vol) `sC` S "with",
  ch thFluxVect, S "as the", phrase thFluxVect, S "for the",
  phrase surface `sAnd` ch uNormalVect, S "as a", phrase unit_,
  S "outward", phrase uNormalVect, S "for a", phrase surface]

rocTempDerivArbVol :: [Sentence]
rocTempDerivArbVol = [S "We consider an arbitrary" +:+. phrase vol, S "The",
  phrase volHtGen, S "is assumed constant. Then", eqN 1, S "can be written as"]

rocTempDerivConsFlx :: Sentence -> [ConceptInstance] -> [Sentence]
rocTempDerivConsFlx s assumps = [S "Where", 
  foldlList Comma List (map ch [htFluxIn, htFluxOut, inSA, outSA]),
  S "are explained in" +:+. makeRef2S rocTempSimp, s, S "Assuming", 
  foldlList Comma List (map ch [density, QT.heatCapSpec, QT.temp]),
  S "are constant over the", phrase vol `sC` S "which is true in our case by",
  foldlList Comma List (map makeRef2S assumps) `sC` S "we have"]

rocTempDerivConsFlxSWHS :: Sentence
rocTempDerivConsFlxSWHS = foldlSent [S "The integral over the", phrase surface,
  S "could be simplified because the thermal flux is assumed constant over",
  ch inSA `sAnd` ch outSA `sAnd` E 0, S "on all other" +:+. plural surface,
  S "Outward flux is considered positive"]

rocTempDerivDens :: [Sentence]
rocTempDerivDens = [S "Using the fact that", ch density :+: S "=" :+: ch mass :+:
  S "/" :+: ch vol `sC` eqN 2, S "can be written as"]

rocTempDerivIntegEq, rocTempDerivGaussEq, rocTempDerivArbVolEq,
  rocTempDerivConsFlxEq, rocTempDerivDensEq :: Expr

rocTempDerivIntegEq = negate (intAll (eqSymb vol) (sy gradient $. sy thFluxVect)) +
  intAll (eqSymb vol) (sy volHtGen) $=
  intAll (eqSymb vol) (sy density
  * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

rocTempDerivGaussEq = negate (intAll (eqSymb surface) (sy thFluxVect $. sy uNormalVect)) +
  intAll (eqSymb vol) (sy volHtGen) $= 
  intAll (eqSymb vol)
  (sy density * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

rocTempDerivArbVolEq = sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol $= 
  intAll (eqSymb vol) (sy density * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

rocTempDerivConsFlxEq = sy density * sy QT.heatCapSpec * sy vol * deriv
  (sy QT.temp) time $= sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol

rocTempDerivDensEq = sy mass * sy QT.heatCapSpec * deriv (sy QT.temp)
  time $= sy htFluxIn * sy inSA - sy htFluxOut
  * sy outSA + sy volHtGen * sy vol

rocTempSimpDerivEqns :: [Expr]
rocTempSimpDerivEqns = [rocTempDerivIntegEq, rocTempDerivGaussEq, rocTempDerivArbVolEq, rocTempDerivConsFlxEq,
  rocTempDerivDensEq]
