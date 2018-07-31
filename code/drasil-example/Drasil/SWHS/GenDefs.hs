module Drasil.SWHS.GenDefs (swhsGenDefs, nwtnCooling, rocTempSimp, roc_temp_simp_deriv,
  generalDefinitions, nwtnCooling_desc, rocTempSimp_desc) where

import Prelude hiding (sin, cos, tan)
import Language.Drasil
import Drasil.DocLang (refA)

import Data.Drasil.SentenceStructures (foldlSent)
import Data.Drasil.Quantities.PhysicalProperties as QPP (vol, mass, density)
import Data.Drasil.Quantities.Thermodynamics as QT (ht_flux, heat_cap_spec,
  temp)
import Data.Drasil.Quantities.Physics as QP (time)
import Drasil.SWHS.Unitals (vol_ht_gen, deltaT, temp_env, pcm_SA,
  out_SA, in_SA, ht_flux_in, ht_flux_out, htTransCoeff, thFluxVect)
import Data.Drasil.SentenceStructures (isThe, sAnd, ofThe, acroGD, foldlSentCol,
  foldlList, SepType(Comma), FoldType(List))
import Data.Drasil.Utils (unwrap, weave)
import Data.Drasil.Concepts.Math (equation, rOfChng, rate, unit_)
import Data.Drasil.Concepts.Thermodynamics (law_conv_cooling)
import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Data.Drasil.Concepts.Documentation (assumption)
import Data.Drasil.Units.Thermodynamics (thermal_flux)

import Drasil.SWHS.TMods (t1ConsThermE)
import Drasil.SWHS.Concepts (gauss_div)
import Drasil.SWHS.Assumptions

---------------------------
--  General Definitions  --
---------------------------

swhsGenDefs :: [RelationConcept]
swhsGenDefs = [nwtnCooling, rocTempSimp]

generalDefinitions :: [GenDefn]
generalDefinitions = [gd' nwtnCooling (Just thermal_flux) ([] :: Derivation) "nwtnCooling" [nwtnCooling_desc],
  gd' rocTempSimp (Nothing :: Maybe UnitDefn) roc_temp_simp_deriv "rocTempSimp" [rocTempSimp_desc]]

--
nwtnCooling :: RelationConcept
nwtnCooling = makeRC "nwtnCooling" (nounPhraseSP "Newton's law of cooling") 
  nwtnCooling_desc nwtnCooling_rel

nwtnCooling_rel :: Relation
nwtnCooling_rel = apply1 ht_flux QP.time $= sy htTransCoeff *
  apply1 deltaT QP.time

nwtnCooling_desc :: Sentence
nwtnCooling_desc = foldlSent [at_start law_conv_cooling +:+.
  S "describes convective cooling from a surface" +:
  S "The law is stated as", S "the", phrase rate,
  S "of heat loss from a body is proportional to the",
  S "difference in", plural temp, S "between the body" +:+.
  S "and its surroundings", E (apply1 thFluxVect QP.time) `isThe`
  S "thermal flux" +:+. sParen (Sy $ unit_symb thFluxVect),
  ch htTransCoeff `isThe` S "heat transfer coefficient" `sC`
  S "assumed independant of", ch QT.temp, sParen (refA swhsRefDB newA2) +:+.
  sParen (Sy $ unit_symb htTransCoeff),
  E (apply1 deltaT QP.time $= apply1 temp QP.time -
  apply1 temp_env QP.time) `isThe` S "time-dependant thermal gradient",
  S "between the environment and the object",
  sParen (Sy $ unit_symb deltaT)]

--
rocTempSimp :: RelationConcept
rocTempSimp = makeRC "rocTempSimp" (nounPhraseSP $ "Simplified rate " ++
  "of change of temperature") rocTempSimp_desc rocTempSimp_rel 

rocTempSimp_rel :: Relation
rocTempSimp_rel = (sy QPP.mass) * (sy QT.heat_cap_spec) *
  deriv (sy QT.temp) QP.time $= sy ht_flux_in * sy in_SA -
  sy ht_flux_out * sy out_SA + sy vol_ht_gen * sy QPP.vol

rocTempSimp_desc :: Sentence
rocTempSimp_desc = foldlSent [S "The basic", phrase equation,
  S "governing the", phrase rOfChng, S "of", phrase temp `sC`
  S "for a given", phrase QPP.vol, ch QPP.vol `sC` S "with" +:+.
  phrase QP.time, ch QPP.mass `isThe` phrase QPP.mass +:+.
  sParen (Sy $ unit_symb QPP.mass), ch QT.heat_cap_spec `isThe` 
  phrase QT.heat_cap_spec +:+. sParen (Sy $ unit_symb QT.heat_cap_spec),
  ch temp `isThe` phrase temp, sParen (Sy $ unit_symb temp) `sAnd`
  ch QP.time `isThe` phrase QP.time +:+. sParen (Sy $ unit_symb QP.time),
  ch ht_flux_in `sAnd` ch ht_flux_out, S "are the in and out heat",
  S "transfer rates, respectively" +:+. sParen (Sy $ unit_symb QT.ht_flux),
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
  [S "Detailed derivation of simplified" +:+ phrase rOfChng +:+ S "of" +:+
    phrase temp +:+ S ":"] ++
  (weave [roc_temp_simp_deriv_sentences, map E roc_temp_simp_deriv_eqns])

roc_temp_simp_deriv_sentences :: [Sentence]
roc_temp_simp_deriv_sentences = map foldlSentCol [
  s4_2_3_desc1 t1ConsThermE vol,
  s4_2_3_desc2 gauss_div surface vol thFluxVect uNormalVect unit_,
  s4_2_3_desc3 vol vol_ht_gen,
  s4_2_3_desc4 ht_flux_in ht_flux_out in_SA out_SA density QT.heat_cap_spec
    QT.temp vol [makeRef newA3, makeRef newA4, 
                 makeRef newA5, makeRef newA6],
  s4_2_3_desc5 density mass vol]

s4_2_3_desc1 :: RelationConcept -> UnitalChunk -> [Sentence]
s4_2_3_desc1 t1c vo =
  [S "Integrating", makeRef $ reldefn t1c,
  S "over a", phrase vo, sParen (ch vo) `sC` S "we have"]

s4_2_3_desc2 :: ConceptChunk -> DefinedQuantityDict -> UnitalChunk -> UnitalChunk ->
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
  acroGD 2, S "Assuming", ch den `sC` ch hcs `sAnd` ch te,
  S "are constant over the", phrase vo `sC` S "which is true in our case by",
  titleize' assumption, (foldlList Comma List $ (map (\d -> sParen (d)))
  assumps) `sC` S "we have"]

s4_2_3_desc5 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> [Sentence]
s4_2_3_desc5 den ma vo = [S "Using the fact that", ch den :+: S "=" :+:
  ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

s4_2_3_eq1, s4_2_3_eq2, s4_2_3_eq3, s4_2_3_eq4, s4_2_3_eq5 :: Expr

s4_2_3_eq1 = (negate (int_all (eqSymb vol) ((sy gradient) $. (sy thFluxVect)))) + 
  (int_all (eqSymb vol) (sy vol_ht_gen)) $=
  (int_all (eqSymb vol) ((sy density)
  * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

s4_2_3_eq2 = (negate (int_all (eqSymb surface) ((sy thFluxVect) $. (sy uNormalVect)))) +
  (int_all (eqSymb vol) (sy vol_ht_gen)) $= 
  (int_all (eqSymb vol)
  ((sy density) * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

s4_2_3_eq3 = (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol) $= 
  (int_all (eqSymb vol) ((sy density) * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

s4_2_3_eq4 = (sy density) * (sy QT.heat_cap_spec) * (sy vol) * deriv
  (sy QT.temp) time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol)

s4_2_3_eq5 = (sy mass) * (sy QT.heat_cap_spec) * deriv (sy QT.temp)
  time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out)
  * (sy out_SA) + (sy vol_ht_gen) * (sy vol)

roc_temp_simp_deriv_eqns :: [Expr]
roc_temp_simp_deriv_eqns = [s4_2_3_eq1, s4_2_3_eq2, s4_2_3_eq3, s4_2_3_eq4,
  s4_2_3_eq5]
