module Drasil.NoPCM.GenDefs (rocTempSimp, genDefs) where

import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)
import Utils.Drasil

import Data.Drasil.Concepts.Math (rOfChng, unit_)
import Data.Drasil.Concepts.Thermodynamics (temp)

import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Physics (time)
import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heatCapSpec)

import Drasil.NoPCM.Assumptions (assumpDWCoW, assumpSHECoW)
import Drasil.SWHS.Assumptions (assumpCWTAT)
import Drasil.SWHS.Concepts (gaussDiv)
import Drasil.SWHS.GenDefs (nwtnCooling, rocTempSimpRC, rocTempSimpDesc)
import Drasil.SWHS.TMods (consThermE)
import Drasil.SWHS.Unitals (inSA, outSA, volHtGen, thFluxVect, htFluxIn, 
  htFluxOut)

genDefs :: [GenDefn]
genDefs = [nwtnCooling, rocTempSimp] 

rocTempSimp :: GenDefn
rocTempSimp = gdNoRefs rocTempSimpRC (Nothing :: Maybe UnitDefn) rocTempSimpDeriv 
  "rocTempSimp" [rocTempSimpDesc]

rocTempSimpDeriv :: Derivation
rocTempSimpDeriv =
  S "Detailed derivation of simplified" +:+ phrase rOfChng +:+ S "of" +:+.
    phrase temp :
  weave [rocTempSimpDerivSent, map E rocTempSimpDerivEqns]

rocTempSimpDerivSent :: [Sentence]
rocTempSimpDerivSent = map foldlSentCol [
  rocTempDerivInteg consThermE vol,
  rocTempDerivGauss gaussDiv surface vol thFluxVect uNormalVect unit_,
  rocTempDerivArbVol vol volHtGen,
  genDefDesc4 htFluxIn htFluxOut inSA outSA density QT.heatCapSpec
    QT.temp vol [makeRef2S assumpCWTAT, makeRef2S assumpDWCoW, makeRef2S assumpSHECoW],
  genDefDesc5 density mass vol]

rocTempDerivInteg :: (HasShortName x, Referable x) => x -> UnitalChunk -> [Sentence]
rocTempDerivInteg t1c vo =
  [S "Integrating", makeRef2S t1c, S "over a", phrase vo, sParen (ch vo) `sC` S "we have"]

rocTempDerivGauss :: (Quantity b, Quantity e) => ConceptChunk -> b -> UnitalChunk ->
  UnitalChunk -> e -> ConceptChunk -> [Sentence]
rocTempDerivGauss gad su vo tfv unv un =
  [S "Applying", titleize gad, S "to the first term over",
  (phrase su +:+ ch su `ofThe` phrase vo) `sC` S "with",
  ch tfv, S "as the", phrase tfv, S "for the",
  phrase su `sAnd` ch unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

rocTempDerivArbVol :: UnitalChunk -> UnitalChunk -> [Sentence]
rocTempDerivArbVol vo vhg = [S "We consider an arbitrary" +:+. phrase vo, S "The",
  phrase vhg, S "is assumed constant. Then (1) can be written as"]

genDefDesc4 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  [Sentence] -> [Sentence]
genDefDesc4 hfi hfo iS oS den hcs te vo assumps = [S "Where", ch hfi `sC`
  ch hfo `sC` ch iS `sC` S "and", ch oS, S "are explained in" +:+.
  makeRef2S rocTempSimp, S "Assuming", ch den `sC` ch hcs `sAnd` ch te,
  S "are constant over the", phrase vo `sC` S "which is true in our case by",
  foldlList Comma List assumps `sC` S "we have"]

genDefDesc5 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> [Sentence]
genDefDesc5 den ma vo = [S "Using the fact that", ch den :+: S "=" :+:
  ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

genDefEq1, genDefEq2, genDefEq3, genDefEq4, genDefEq5 :: Expr

genDefEq1 = negate (intAll (eqSymb vol) (sy gradient $. sy thFluxVect)) + 
  intAll (eqSymb vol) (sy volHtGen) $=
  intAll (eqSymb vol) (sy density
  * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

genDefEq2 = negate (intAll (eqSymb surface) (sy thFluxVect $. sy uNormalVect)) +
  intAll (eqSymb vol) (sy volHtGen) $= 
  intAll (eqSymb vol)
  (sy density * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

genDefEq3 = sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol $= 
  intAll (eqSymb vol) (sy density * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

genDefEq4 = sy density * sy QT.heatCapSpec * sy vol * deriv
  (sy QT.temp) time $= sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol

genDefEq5 = sy mass * sy QT.heatCapSpec * deriv (sy QT.temp)
  time $= sy htFluxIn * sy inSA - sy htFluxOut
  * sy outSA + sy volHtGen * sy vol

rocTempSimpDerivEqns :: [Expr]
rocTempSimpDerivEqns = [genDefEq1, genDefEq2, genDefEq3, genDefEq4,
  genDefEq5]
