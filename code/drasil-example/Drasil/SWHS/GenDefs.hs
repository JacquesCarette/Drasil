module Drasil.SWHS.GenDefs (genDefs, nwtnCooling, rocTempSimp,
  rocTempSimpDeriv, nwtnCoolingDesc, rocTempSimpRC, rocTempSimpDesc) where

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
import Drasil.SWHS.Unitals (volHtGen, deltaT, tempEnv, pcmSA,
  outSA, inSA, htFluxIn, htFluxOut, htTransCoeff, thFluxVect)

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
  [makeCiteInfo incroperaEtAl2007 $ Page [8]] "nwtnCooling" [nwtnCoolingDesc]

rocTempSimp = gdNoRefs rocTempSimpRC (Nothing :: Maybe UnitDefn) rocTempSimpDeriv
                 "rocTempSimp" [rocTempSimpDesc]

--

nwtnCoolingRC :: RelationConcept
nwtnCoolingRC = makeRC "nwtnCooling" (nounPhraseSP "Newton's law of cooling") 
  nwtnCoolingDesc nwtnCoolingRel -- nwtnCoolingL

nwtnCoolingRel :: Relation
nwtnCoolingRel = apply1 htFlux QP.time $= sy htTransCoeff *
  apply1 deltaT QP.time

nwtnCoolingDesc :: Sentence
nwtnCoolingDesc = foldlSent [atStart lawConvCooling +:+.
  S "describes convective cooling from a surface" +:
  S "The law is stated as", S "the", phrase rate,
  S "of heat loss from a body is proportional to the",
  S "difference in", plural temp, S "between the body" +:+.
  S "and its surroundings", E (apply1 thFluxVect QP.time) `isThe`
  S "thermal flux" +:+. sParen (Sy $ unit_symb thFluxVect),
  ch htTransCoeff `isThe` S "heat transfer coefficient" `sC`
  S "assumed independant of", ch QT.temp, sParen (makeRef2S assumpHTCC) +:+.
  sParen (Sy $ unit_symb htTransCoeff), E (apply1 deltaT QP.time $= 
  apply1 temp QP.time - apply1 tempEnv QP.time) `isThe` 
  S "time-dependant thermal gradient between the environment and the object",
  sParen (Sy $ unit_symb deltaT)]

--
rocTempSimpRC :: RelationConcept
rocTempSimpRC = makeRC "rocTempSimp" (nounPhraseSP $ "Simplified rate " ++
  "of change of temperature") rocTempSimpDesc rocTempSimpRel -- rocTempSimpL

rocTempSimpRel :: Relation
rocTempSimpRel = sy QPP.mass * sy QT.heatCapSpec *
  deriv (sy QT.temp) QP.time $= sy htFluxIn * sy inSA -
  sy htFluxOut * sy outSA + sy volHtGen * sy QPP.vol

rocTempSimpDesc :: Sentence
rocTempSimpDesc = foldlSent [S "The basic", phrase equation,
  S "governing the", phrase rOfChng, S "of", phrase temp `sC`
  S "for a given", phrase QPP.vol, ch QPP.vol `sC` S "with" +:+.
  phrase QP.time, ch QPP.mass `isThe` phrase QPP.mass +:+.
  sParen (Sy $ unit_symb QPP.mass), ch QT.heatCapSpec `isThe` 
  phrase QT.heatCapSpec +:+. sParen (Sy $ unit_symb QT.heatCapSpec),
  ch temp `isThe` phrase temp, sParen (Sy $ unit_symb temp) `sAnd`
  ch QP.time `isThe` phrase QP.time +:+. sParen (Sy $ unit_symb QP.time),
  ch htFluxIn `sAnd` ch htFluxOut, S "are the in and out heat",
  S "transfer rates, respectively" +:+. sParen (Sy $ unit_symb QT.htFlux),
  ch inSA `sAnd` ch outSA, S "are the surface areas over which the",
  S "heat is being transferred in and out, respectively" +:+.
  sParen (unwrap $ getUnit pcmSA), ch volHtGen `isThe`
  S "volumetric heat generated" +:+. sParen (Sy $ unit_symb volHtGen),
  ch QPP.vol `isThe` phrase QPP.vol, sParen (Sy $ unit_symb QPP.vol)]

---------------------------------------
--  General Definitions  Derivation  --
---------------------------------------

rocTempSimpDeriv :: Derivation
rocTempSimpDeriv =
  S "Detailed derivation of simplified" +:+ phrase rOfChng +:+ S "of" +:+
    phrase temp +:+ S ":" :
  weave [rocTempSimpDerivSent, map E rocTempSimpDerivEqns]

rocTempSimpDerivSent :: [Sentence]
rocTempSimpDerivSent = map foldlSentCol [
  rocTempDerivInteg consThermE vol,
  rocTempDerivGauss gaussDiv surface vol thFluxVect uNormalVect unit_,
  rocTempDerivArbVol vol volHtGen,
  rocTempDerivConsFlx htFluxIn htFluxOut inSA outSA density QT.heatCapSpec
    QT.temp vol [makeRef2S assumpCWTAT, makeRef2S assumpTPCAV,
                 makeRef2S assumpDWPCoV, makeRef2S assumpSHECoV],
  s4_2_3_desc5 density mass vol]

rocTempDerivInteg :: (HasShortName x, Referable x) => x -> UnitalChunk -> [Sentence]
rocTempDerivInteg t1c vo =
  [S "Integrating", makeRef2S t1c, S "over a", phrase vo, sParen (ch vo) `sC` S "we have"]

rocTempDerivGauss :: (NamedIdea b, HasSymbol b) => ConceptChunk -> b -> UnitalChunk -> UnitalChunk ->
  DefinedQuantityDict -> ConceptChunk -> [Sentence]
rocTempDerivGauss cchn su vo tfv unv un =
  [S "Applying", titleize cchn, S "to the first term over",
  (phrase su +:+ ch su `ofThe` phrase vo) `sC` S "with",
  ch tfv, S "as the", phrase tfv, S "for the",
  phrase su `sAnd` ch unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

rocTempDerivArbVol :: UnitalChunk -> UnitalChunk -> [Sentence]
rocTempDerivArbVol vo vhg = [S "We consider an arbitrary" +:+. phrase vo, S "The",
  phrase vhg, S "is assumed constant. Then (1) can be written as"]

rocTempDerivConsFlx :: UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  [Sentence] -> [Sentence]
rocTempDerivConsFlx hfi hfo iS oS den hcs te vo assumps = [S "Where", ch hfi `sC`
  ch hfo `sC` ch iS `sC` S "and", ch oS, S "are explained in" +:+.
  makeRef2S rocTempSimp, S "The integral over the", phrase surface, 
  S "could be simplified because the thermal flux is assumed constant over",
  ch inSA `sAnd` ch outSA `sAnd` E 0, S "on all other" +:+. plural surface +:+.
  S "Outward flux is considered positive", S "Assuming", ch den `sC` ch hcs `sAnd` ch te,
  S "are constant over the", phrase vo `sC` S "which is true in our case by",
  foldlList Comma List assumps `sC` S "we have"]

s4_2_3_desc5 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> [Sentence]
s4_2_3_desc5 den ma vo = [S "Using the fact that", ch den :+: S "=" :+:
  ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

s4_2_3_eq1, s4_2_3_eq2, s4_2_3_eq3, s4_2_3_eq4, s4_2_3_eq5 :: Expr

s4_2_3_eq1 = negate (intAll (eqSymb vol) (sy gradient $. sy thFluxVect)) + 
  intAll (eqSymb vol) (sy volHtGen) $=
  intAll (eqSymb vol) (sy density
  * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

s4_2_3_eq2 = negate (intAll (eqSymb surface) (sy thFluxVect $. sy uNormalVect)) +
  intAll (eqSymb vol) (sy volHtGen) $= 
  intAll (eqSymb vol)
  (sy density * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

s4_2_3_eq3 = sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol $= 
  intAll (eqSymb vol) (sy density * sy QT.heatCapSpec * pderiv (sy QT.temp) time)

s4_2_3_eq4 = sy density * sy QT.heatCapSpec * sy vol * deriv
  (sy QT.temp) time $= sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol

s4_2_3_eq5 = sy mass * sy QT.heatCapSpec * deriv (sy QT.temp)
  time $= sy htFluxIn * sy inSA - sy htFluxOut
  * sy outSA + sy volHtGen * sy vol

rocTempSimpDerivEqns :: [Expr]
rocTempSimpDerivEqns = [s4_2_3_eq1, s4_2_3_eq2, s4_2_3_eq3, s4_2_3_eq4,
  s4_2_3_eq5]
