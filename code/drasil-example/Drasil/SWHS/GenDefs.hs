module Drasil.SWHS.GenDefs (genDefs, htFluxWaterFromCoil, htFluxPCMFromWater,
  rocTempSimp, rocTempSimpDeriv, rocTempSimpRC) where

import Language.Drasil
import Theory.Drasil (GenDefn, gd, gdNoRefs, ModelKinds (OthModel, EquationalModel))
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

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
rocTempSimp = gdNoRefs (OthModel rocTempSimpRC) (Nothing :: Maybe UnitDefn)
  (Just $ rocTempSimpDeriv rocTempDerivConsFlxSWHS
   [assumpCWTAT, assumpTPCAV, assumpDWPCoV, assumpSHECoV])
  "rocTempSimp" [{-Notes-}]

rocTempSimpRC :: RelationConcept
rocTempSimpRC = makeRC "rocTempSimpRC" (nounPhraseSP $ "Simplified rate " ++
  "of change of temperature") EmptyS rocTempSimpRel

rocTempSimpRel :: Relation
rocTempSimpRel = sy QPP.mass `mulRe` sy QT.heatCapSpec `mulRe`
  deriv (sy QT.temp) QP.time $= (sy htFluxIn `mulRe` sy inSA $-
  (sy htFluxOut `mulRe` sy outSA)) `addRe` (sy volHtGen `mulRe` sy QPP.vol)

----

htFluxWaterFromCoil :: GenDefn
htFluxWaterFromCoil = gd (EquationalModel htFluxWaterFromCoilQD) (getUnit htFluxC) Nothing
  [makeCite koothoor2013] "htFluxWaterFromCoil"
  [newtonLawNote htFluxC assumpLCCCW coil, makeRef2S assumpTHCCoT]

htFluxWaterFromCoilQD :: QDefinition
htFluxWaterFromCoilQD = mkQuantDef htFluxC htFluxWaterFromCoilExpr

htFluxWaterFromCoilExpr :: Relation
htFluxWaterFromCoilExpr = sy coilHTC `mulRe` (sy tempC $- apply1 tempW time)

--Can't include info in description beyond definition of variables?
----

htFluxPCMFromWater :: GenDefn
htFluxPCMFromWater = gd (EquationalModel htFluxPCMFromWaterQD) (getUnit htFluxP) Nothing
  [makeCite koothoor2013] "htFluxPCMFromWater"
  [newtonLawNote htFluxP assumpLCCWP phaseChangeMaterial]

htFluxPCMFromWaterQD :: QDefinition
htFluxPCMFromWaterQD = mkQuantDef htFluxP htFluxPCMFromWaterExpr

htFluxPCMFromWaterExpr :: Expr
htFluxPCMFromWaterExpr = sy pcmHTC `mulRe` (apply1 tempW time $- apply1 tempPCM time)

newtonLawNote :: UnitalChunk -> ConceptInstance -> ConceptChunk -> Sentence
newtonLawNote u a c = foldlSent [ch u `S.sIs` S "found by assuming that",
  phrase lawConvCooling, S "applies" +:+. sParen (makeRef2S a), S "This law",
  sParen (S "defined" `S.sIn` makeRef2S nwtnCooling) `S.sIs` S "used on",
  phrase surface `S.the_ofThe` phrase c]

--------------------------------------
--  General Definitions Derivation  --
--------------------------------------

rocTempSimpDeriv :: Sentence -> [ConceptInstance] -> Derivation
rocTempSimpDeriv s a = mkDerivName (S "simplified" +:+ phrase rOfChng `S.sOf` phrase temp)
  (weave [rocTempSimpDerivSent s a, map E rocTempSimpDerivEqns])

rocTempSimpDerivSent :: Sentence -> [ConceptInstance] -> [Sentence]
rocTempSimpDerivSent s a = map foldlSentCol [rocTempDerivInteg, rocTempDerivGauss,
  rocTempDerivArbVol, rocTempDerivConsFlx s a, rocTempDerivDens]

rocTempDerivInteg :: [Sentence]
rocTempDerivInteg = [S "Integrating", makeRef2S consThermE, S "over a",
  phrase vol, sParen (ch vol) `sC` S "we have"]

rocTempDerivGauss :: [Sentence]
rocTempDerivGauss = [S "Applying", titleize gaussDiv, S "to the first term over",
  (phrase surface +:+ ch surface `S.the_ofThe` phrase vol) `sC` S "with",
  ch thFluxVect, S "as the", phrase thFluxVect, S "for the",
  phrase surface `S.sAnd` ch uNormalVect, S "as a", phrase unit_,
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
  ch inSA `S.sAnd` ch outSA `S.sAnd` E (int 0), S "on all other" +:+. plural surface,
  S "Outward flux is considered positive"]

rocTempDerivDens :: [Sentence]
rocTempDerivDens = [S "Using the fact that", ch density :+: S "=" :+: ch mass :+:
  S "/" :+: ch vol `sC` eqN 2, S "can be written as"]

rocTempDerivIntegEq, rocTempDerivGaussEq, rocTempDerivArbVolEq,
  rocTempDerivConsFlxEq, rocTempDerivDensEq :: Expr

rocTempDerivIntegEq = neg (intAll (eqSymb vol) (sy gradient $. sy thFluxVect)) `addRe`
  intAll (eqSymb vol) (sy volHtGen) $=
  intAll (eqSymb vol) (sy density
  `mulRe` sy QT.heatCapSpec `mulRe` pderiv (sy QT.temp) time)

rocTempDerivGaussEq = neg (intAll (eqSymb surface) (sy thFluxVect $. sy uNormalVect)) `addRe`
  intAll (eqSymb vol) (sy volHtGen) $= 
  intAll (eqSymb vol)
  (sy density `mulRe` sy QT.heatCapSpec `mulRe` pderiv (sy QT.temp) time)

rocTempDerivArbVolEq = (sy htFluxIn `mulRe` sy inSA $- (sy htFluxOut `mulRe`
  sy outSA)) `addRe` (sy volHtGen `mulRe` sy vol) $= 
  intAll (eqSymb vol) (sy density `mulRe` sy QT.heatCapSpec `mulRe` pderiv (sy QT.temp) time)

rocTempDerivConsFlxEq = sy density `mulRe` sy QT.heatCapSpec `mulRe` sy vol `mulRe` deriv
  (sy QT.temp) time $= (sy htFluxIn `mulRe` sy inSA $- (sy htFluxOut `mulRe`
  sy outSA)) `addRe` (sy volHtGen `mulRe` sy vol)

rocTempDerivDensEq = sy mass `mulRe` sy QT.heatCapSpec `mulRe` deriv (sy QT.temp)
  time $= (sy htFluxIn `mulRe` sy inSA $- (sy htFluxOut
  `mulRe` sy outSA)) `addRe` (sy volHtGen `mulRe` sy vol)

rocTempSimpDerivEqns :: [Expr]
rocTempSimpDerivEqns = [rocTempDerivIntegEq, rocTempDerivGaussEq, rocTempDerivArbVolEq, rocTempDerivConsFlxEq,
  rocTempDerivDensEq]
