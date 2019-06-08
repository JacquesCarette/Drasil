module Drasil.SWHS.GenDefs (genDefs, nwtnCooling, rocTempSimp,
  roc_temp_simp_deriv, nwtnCooling_desc, rocTempSimpRC, rocTempSimp_desc) where

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
import Data.Drasil.Utils (unwrap, weave)

import Drasil.SWHS.Assumptions (assumpHTCC, assumpCWTAT, assumpTPCAV,
  assumpDWPCoV, assumpSHECoV)
import Drasil.SWHS.Concepts (gauss_div)
import Drasil.SWHS.References (incroperaEtAl2007)
import Drasil.SWHS.TMods (consThermE)
import Drasil.SWHS.Unitals (vol_ht_gen, deltaT, temp_env, pcm_SA,
  out_SA, in_SA, ht_flux_in, ht_flux_out, htTransCoeff, thFluxVect)

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
  [makeCiteInfo incroperaEtAl2007 $ Page [8]] "nwtnCooling" [nwtnCooling_desc]

rocTempSimp = gdNoRefs rocTempSimpRC (Nothing :: Maybe UnitDefn) roc_temp_simp_deriv
                 "rocTempSimp" [rocTempSimp_desc]

--

nwtnCoolingRC :: RelationConcept
nwtnCoolingRC = makeRC "nwtnCooling" (nounPhraseSP "Newton's law of cooling") 
  nwtnCooling_desc nwtnCooling_rel -- nwtnCoolingL

nwtnCooling_rel :: Relation
nwtnCooling_rel = apply1 htFlux QP.time $= sy htTransCoeff *
  apply1 deltaT QP.time

nwtnCooling_desc :: Sentence
nwtnCooling_desc = foldlSent [at_start lawConvCooling +:+.
  S "describes convective cooling from a surface" +:
  S "The law is stated as", S "the", phrase rate,
  S "of heat loss from a body is proportional to the",
  S "difference in", plural temp, S "between the body" +:+.
  S "and its surroundings", E (apply1 thFluxVect QP.time) `isThe`
  S "thermal flux" +:+. sParen (Sy $ unit_symb thFluxVect),
  ch htTransCoeff `isThe` S "heat transfer coefficient" `sC`
  S "assumed independant of", ch QT.temp, sParen (makeRef2S assumpHTCC) +:+.
  sParen (Sy $ unit_symb htTransCoeff), E (apply1 deltaT QP.time $= 
  apply1 temp QP.time - apply1 temp_env QP.time) `isThe` 
  S "time-dependant thermal gradient between the environment and the object",
  sParen (Sy $ unit_symb deltaT)]

--
rocTempSimpRC :: RelationConcept
rocTempSimpRC = makeRC "rocTempSimp" (nounPhraseSP $ "Simplified rate " ++
  "of change of temperature") rocTempSimp_desc rocTempSimp_rel -- rocTempSimpL

rocTempSimp_rel :: Relation
rocTempSimp_rel = (sy QPP.mass) * (sy QT.heatCapSpec) *
  deriv (sy QT.temp) QP.time $= sy ht_flux_in * sy in_SA -
  sy ht_flux_out * sy out_SA + sy vol_ht_gen * sy QPP.vol

rocTempSimp_desc :: Sentence
rocTempSimp_desc = foldlSent [S "The basic", phrase equation,
  S "governing the", phrase rOfChng, S "of", phrase temp `sC`
  S "for a given", phrase QPP.vol, ch QPP.vol `sC` S "with" +:+.
  phrase QP.time, ch QPP.mass `isThe` phrase QPP.mass +:+.
  sParen (Sy $ unit_symb QPP.mass), ch QT.heatCapSpec `isThe` 
  phrase QT.heatCapSpec +:+. sParen (Sy $ unit_symb QT.heatCapSpec),
  ch temp `isThe` phrase temp, sParen (Sy $ unit_symb temp) `sAnd`
  ch QP.time `isThe` phrase QP.time +:+. sParen (Sy $ unit_symb QP.time),
  ch ht_flux_in `sAnd` ch ht_flux_out, S "are the in and out heat",
  S "transfer rates, respectively" +:+. sParen (Sy $ unit_symb QT.htFlux),
  ch in_SA `sAnd` ch out_SA, S "are the surface areas over which the",
  S "heat is being transferred in and out, respectively" +:+.
  sParen (unwrap $ getUnit pcm_SA), ch vol_ht_gen `isThe`
  S "volumetric heat generated" +:+. sParen (Sy $ unit_symb vol_ht_gen),
  ch QPP.vol `isThe` phrase QPP.vol, sParen (Sy $ unit_symb QPP.vol)]

---------------------------------------
--  General Definitions  Derivation  --
---------------------------------------

roc_temp_simp_deriv :: Derivation
roc_temp_simp_deriv =
  S "Detailed derivation of simplified" +:+ phrase rOfChng +:+ S "of" +:+
    phrase temp +:+ S ":" :
  weave [roc_temp_simp_deriv_sentences, map E roc_temp_simp_deriv_eqns]

roc_temp_simp_deriv_sentences :: [Sentence]
roc_temp_simp_deriv_sentences = map foldlSentCol [
  s4_2_3_desc1 consThermE vol,
  s4_2_3_desc2 gauss_div surface vol thFluxVect uNormalVect unit_,
  s4_2_3_desc3 vol vol_ht_gen,
  s4_2_3_desc4 ht_flux_in ht_flux_out in_SA out_SA density QT.heatCapSpec
    QT.temp vol [makeRef2S assumpCWTAT, makeRef2S assumpTPCAV,
                 makeRef2S assumpDWPCoV, makeRef2S assumpSHECoV],
  s4_2_3_desc5 density mass vol]

s4_2_3_desc1 :: (HasShortName x, Referable x) => x -> UnitalChunk -> [Sentence]
s4_2_3_desc1 t1c vo =
  [S "Integrating", makeRef2S t1c, S "over a", phrase vo, sParen (ch vo) `sC` S "we have"]

s4_2_3_desc2 :: (NamedIdea b, HasSymbol b) => ConceptChunk -> b -> UnitalChunk -> UnitalChunk ->
  DefinedQuantityDict -> ConceptChunk -> [Sentence]
s4_2_3_desc2 cchn su vo tfv unv un =
  [S "Applying", titleize cchn, S "to the first term over",
  (phrase su +:+ ch su `ofThe` phrase vo) `sC` S "with",
  ch tfv, S "as the", phrase tfv, S "for the",
  phrase su `sAnd` ch unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

s4_2_3_desc3 :: UnitalChunk -> UnitalChunk -> [Sentence]
s4_2_3_desc3 vo vhg = [S "We consider an arbitrary" +:+. phrase vo, S "The",
  phrase vhg, S "is assumed constant. Then (1) can be written as"]

s4_2_3_desc4 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  [Sentence] -> [Sentence]
s4_2_3_desc4 hfi hfo iS oS den hcs te vo assumps = [S "Where", ch hfi `sC`
  ch hfo `sC` ch iS `sC` S "and", ch oS, S "are explained in" +:+.
  makeRef2S rocTempSimp, S "Assuming", ch den `sC` ch hcs `sAnd` ch te,
  S "are constant over the", phrase vo `sC` S "which is true in our case by",
  (foldlList Comma List assumps) `sC` S "we have"]

s4_2_3_desc5 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> [Sentence]
s4_2_3_desc5 den ma vo = [S "Using the fact that", ch den :+: S "=" :+:
  ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

s4_2_3_eq1, s4_2_3_eq2, s4_2_3_eq3, s4_2_3_eq4, s4_2_3_eq5 :: Expr

s4_2_3_eq1 = (negate (int_all (eqSymb vol) ((sy gradient) $. (sy thFluxVect)))) + 
  (int_all (eqSymb vol) (sy vol_ht_gen)) $=
  (int_all (eqSymb vol) ((sy density)
  * (sy QT.heatCapSpec) * pderiv (sy QT.temp) time))

s4_2_3_eq2 = (negate (int_all (eqSymb surface) ((sy thFluxVect) $. (sy uNormalVect)))) +
  (int_all (eqSymb vol) (sy vol_ht_gen)) $= 
  (int_all (eqSymb vol)
  ((sy density) * (sy QT.heatCapSpec) * pderiv (sy QT.temp) time))

s4_2_3_eq3 = (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol) $= 
  (int_all (eqSymb vol) ((sy density) * (sy QT.heatCapSpec) * pderiv (sy QT.temp) time))

s4_2_3_eq4 = (sy density) * (sy QT.heatCapSpec) * (sy vol) * deriv
  (sy QT.temp) time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol)

s4_2_3_eq5 = (sy mass) * (sy QT.heatCapSpec) * deriv (sy QT.temp)
  time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out)
  * (sy out_SA) + (sy vol_ht_gen) * (sy vol)

roc_temp_simp_deriv_eqns :: [Expr]
roc_temp_simp_deriv_eqns = [s4_2_3_eq1, s4_2_3_eq2, s4_2_3_eq3, s4_2_3_eq4,
  s4_2_3_eq5]
