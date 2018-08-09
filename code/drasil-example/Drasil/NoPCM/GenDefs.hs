module Drasil.NoPCM.GenDefs (roc_temp_simp_deriv) where

import Language.Drasil
import Drasil.SWHS.TMods (t1ConsThermE_new)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Concepts.Math (unit_, rOfChng)
import Data.Drasil.Concepts.Thermodynamics (temp)
import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heat_cap_spec)
import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Data.Drasil.Utils (weave)
import Data.Drasil.SentenceStructures (sAnd, foldlList, SepType(Comma), 
  FoldType(List), ofThe, foldlSentCol)
import Data.Drasil.Concepts.Documentation (assumption)
import Data.Drasil.Quantities.Physics (time)

import Drasil.SWHS.Concepts (gauss_div)
import Drasil.SWHS.Labels (genDef2Label)
import Drasil.SWHS.Unitals (in_SA, out_SA, vol_ht_gen, thFluxVect, ht_flux_in, 
  ht_flux_out)

roc_temp_simp_deriv :: Derivation
roc_temp_simp_deriv =
  [S "Detailed derivation of simplified" +:+ phrase rOfChng +:+ S "of" +:+.
    phrase temp] ++
  (weave [roc_temp_simp_deriv_sentences, map E roc_temp_simp_deriv_eqns])

roc_temp_simp_deriv_sentences :: [Sentence]
roc_temp_simp_deriv_sentences = map foldlSentCol [
  genDefDesc1 t1ConsThermE_new vol,
  genDefDesc2 gauss_div surface vol thFluxVect uNormalVect unit_,
  genDefDesc3 vol vol_ht_gen,
  genDefDesc4 ht_flux_in ht_flux_out in_SA out_SA density QT.heat_cap_spec
    QT.temp vol [S "A3", S "A4", S "A5"],
  genDefDesc5 density mass vol]

genDefDesc1 :: (HasShortName x, Referable x) => x -> UnitalChunk -> [Sentence]
genDefDesc1 t1c vo =
  [S "Integrating", makeRef t1c, S "over a", phrase vo, sParen (ch vo) `sC` S "we have"]

genDefDesc2 :: ConceptChunk -> DefinedQuantityDict -> UnitalChunk -> UnitalChunk ->
  DefinedQuantityDict -> ConceptChunk -> [Sentence]
genDefDesc2 gad su vo tfv unv un =
  [S "Applying", titleize gad, S "to the first term over",
  (phrase su +:+ ch su `ofThe` phrase vo) `sC` S "with",
  ch tfv, S "as the", phrase tfv, S "for the",
  phrase su `sAnd` ch unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

genDefDesc3 :: UnitalChunk -> UnitalChunk -> [Sentence]
genDefDesc3 vo vhg = [S "We consider an arbitrary" +:+. phrase vo, S "The",
  phrase vhg, S "is assumed constant. Then (1) can be written as"]

genDefDesc4 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  [Sentence] -> [Sentence]
genDefDesc4 hfi hfo iS oS den hcs te vo assumps = [S "Where", ch hfi `sC`
  ch hfo `sC` ch iS `sC` S "and", ch oS, S "are explained in" +:+.
  makeRef genDef2Label, S "Assuming", ch den `sC` ch hcs `sAnd` ch te,
  S "are constant over the", phrase vo `sC` S "which is true in our case by",
  titleize' assumption, (foldlList Comma List $ (map sParen)
  assumps) `sC` S "we have"]

genDefDesc5 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> [Sentence]
genDefDesc5 den ma vo = [S "Using the fact that", ch den :+: S "=" :+:
  ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

genDefEq1, genDefEq2, genDefEq3, genDefEq4, genDefEq5 :: Expr

genDefEq1 = (negate (int_all (eqSymb vol) ((sy gradient) $. (sy thFluxVect)))) + 
  (int_all (eqSymb vol) (sy vol_ht_gen)) $=
  (int_all (eqSymb vol) ((sy density)
  * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

genDefEq2 = (negate (int_all (eqSymb surface) ((sy thFluxVect) $. (sy uNormalVect)))) +
  (int_all (eqSymb vol) (sy vol_ht_gen)) $= 
  (int_all (eqSymb vol)
  ((sy density) * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

genDefEq3 = (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol) $= 
  (int_all (eqSymb vol) ((sy density) * (sy QT.heat_cap_spec) * pderiv (sy QT.temp) time))

genDefEq4 = (sy density) * (sy QT.heat_cap_spec) * (sy vol) * deriv
  (sy QT.temp) time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol)

genDefEq5 = (sy mass) * (sy QT.heat_cap_spec) * deriv (sy QT.temp)
  time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out)
  * (sy out_SA) + (sy vol_ht_gen) * (sy vol)

roc_temp_simp_deriv_eqns :: [Expr]
roc_temp_simp_deriv_eqns = [genDefEq1, genDefEq2, genDefEq3, genDefEq4,
  genDefEq5]
