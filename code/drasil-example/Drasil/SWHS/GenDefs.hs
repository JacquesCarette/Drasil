module Drasil.SWHS.GenDefs (genDefs, htFluxWaterFromCoil, htFluxPCMFromWater,
  rocTempSimp, rocTempSimpDeriv, rocTempSimpRC) where

import Language.Drasil
import Theory.Drasil (GenDefn, gd, gdNoRefs)
import Utils.Drasil

import Control.Lens ((^.))

import Data.Drasil.Concepts.Math (rOfChng, unit_)
import Data.Drasil.Concepts.Thermodynamics (lawConvCooling)

import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Data.Drasil.Quantities.PhysicalProperties as QPP (vol, mass, density)
import Data.Drasil.Quantities.Physics as QP (time)
import Data.Drasil.Quantities.Thermodynamics as QT (heatCapSpec, temp)

import Drasil.SWHS.Assumptions (assumpCWTAT, assumpLCCCW, assumpLCCWP,
  assumpTPCAV, assumpDWPCoV, assumpSHECoV, assumpTHCCoT)
import Drasil.SWHS.Concepts (coil, gaussDiv, phaseChangeMaterial)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.TMods (consThermE, nwtnCooling)
import Drasil.SWHS.Unitals (coilHTC, htFluxC, htFluxIn, htFluxOut, htFluxP,
  inSA, outSA, pcmHTC, tempC, tempPCM, tempW, thFluxVect, volHtGen)

---------------------------
--  General Definitions  --
---------------------------

--FIXME: genDefs, nwtnCoolingGD, and rocTempSimpGD were added--
--since referencing implementation for RelationConcept hasn't--
--stabilized yet (since RelationConcept isn't an instance of --
--the Referable class.                                       --
genDefs :: [GenDefn]
genDefs = [rocTempSimp, htFluxWaterFromCoil, htFluxPCMFromWater] 

rocTempSimp :: GenDefn
rocTempSimp = gdNoRefs rocTempSimpRC (Nothing :: Maybe UnitDefn)
  (Just $ rocTempSimpDeriv rocTempDerivConsFlxSWHS
   [assumpCWTAT, assumpTPCAV, assumpDWPCoV, assumpSHECoV])
  "rocTempSimp" [{-Notes-}]

rocTempSimpRC :: RelationConcept
rocTempSimpRC = makeRC "rocTempSimpRC" (nounPhraseSP $ "Simplified rate " ++
  "of change of temperature") EmptyS rocTempSimpRel

rocTempSimpRel :: Relation
rocTempSimpRel = sy QPP.mass * sy QT.heatCapSpec *
  deriv (sy QT.temp) QP.time $= sy htFluxIn * sy inSA -
  sy htFluxOut * sy outSA + sy volHtGen * sy QPP.vol

----

htFluxWaterFromCoil :: GenDefn
htFluxWaterFromCoil = gd htFluxWaterFromCoilRC (getUnit htFluxC) Nothing
  [makeCite koothoor2013] "htFluxWaterFromCoil"
  [newtonLawNote htFluxC assumpLCCCW coil, makeRef2S assumpTHCCoT]

htFluxWaterFromCoilRC :: RelationConcept
htFluxWaterFromCoilRC = makeRC "htFluxWaterFromCoilRC" (htFluxC ^. term)
  EmptyS htFluxWaterFromCoilRel

htFluxWaterFromCoilRel :: Relation
htFluxWaterFromCoilRel = sy htFluxC $= sy coilHTC * (sy tempC - apply1 tempW time)

--Can't include info in description beyond definition of variables?
----

htFluxPCMFromWater :: GenDefn
htFluxPCMFromWater = gd htFluxPCMFromWaterRC (getUnit htFluxP) Nothing
  [makeCite koothoor2013] "htFluxPCMFromWater"
  [newtonLawNote htFluxP assumpLCCWP phaseChangeMaterial]

htFluxPCMFromWaterRC :: RelationConcept
htFluxPCMFromWaterRC = makeRC "htFluxPCMFromWaterRC" (htFluxP ^. term)
  EmptyS htFluxPCMFromWaterRel

htFluxPCMFromWaterRel :: Relation
htFluxPCMFromWaterRel = sy htFluxP $= sy pcmHTC * (apply1 tempW time - apply1 tempPCM time)

newtonLawNote :: UnitalChunk -> ConceptInstance -> ConceptChunk -> Sentence
newtonLawNote u a c = foldlSent [ch u `sIs` S "found by assuming that",
  phrase lawConvCooling, S "applies" +:+. sParen (makeRef2S a), S "This law",
  sParen (S "defined" `sIn` makeRef2S nwtnCooling) `sIs` S "used on",
  phrase surface `ofThe` phrase c]

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
