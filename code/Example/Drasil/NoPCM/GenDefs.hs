module Drasil.NoPCM.GenDefs (roc_temp_simp_deriv) where

import Language.Drasil
import Drasil.SWHS.TMods (t1ConsThermE)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Concepts.Math (unit_, rOfChng)
import Data.Drasil.Concepts.Thermodynamics (temp)
import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heat_cap_spec)
import Drasil.SWHS.Concepts (gauss_div)
import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Drasil.SWHS.Unitals (in_SA, out_SA, vol_ht_gen, thFluxVect, ht_flux_in, 
                            ht_flux_out)
import Data.Drasil.Utils (getES, weave)
import Data.Drasil.SentenceStructures (sAnd, foldlList, ofThe, acroGD, foldlSentCol)
import Data.Drasil.Concepts.Documentation (assumption)
import Data.Drasil.Quantities.Physics (time)
import Drasil.SWHS.Assumptions (newA3, newA4, newA5)
roc_temp_simp_deriv :: Derivation
roc_temp_simp_deriv =
  [S "Detailed derivation of simplified" +:+ phrase rOfChng +:+ S "of" +:+ phrase temp +:+ S ":"] ++
  (weave [roc_temp_simp_deriv_sentences, map E roc_temp_simp_deriv_eqns])

roc_temp_simp_deriv_sentences :: [Sentence]
roc_temp_simp_deriv_sentences = map foldlSentCol [
  genDefDesc1 t1ConsThermE vol,
  genDefDesc2 gauss_div surface vol thFluxVect uNormalVect unit_,
  genDefDesc3 vol vol_ht_gen,
  genDefDesc4 ht_flux_in ht_flux_out in_SA out_SA density QT.heat_cap_spec
    QT.temp vol newA3 newA4 newA5,
  genDefDesc5 density mass vol]

genDefDesc1 :: RelationConcept -> UnitalChunk -> [Sentence]
genDefDesc1 t1c vo =
  [S "Integrating", makeRef $ reldefn t1c,
  S "over a", phrase vo, sParen (getES vo) `sC` S "we have"]

genDefDesc2 :: ConceptChunk -> DefinedQuantityDict -> UnitalChunk -> UnitalChunk ->
  DefinedQuantityDict -> ConceptChunk -> [Sentence]
genDefDesc2 gad su vo tfv unv un =
  [S "Applying", titleize gad, S "to the first term over",
  (phrase su +:+ getES su `ofThe` phrase vo) `sC` S "with",
  getES tfv, S "as the", phrase tfv, S "for the",
  phrase su `sAnd` getES unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

genDefDesc3 :: UnitalChunk -> UnitalChunk -> [Sentence]
genDefDesc3 vo vhg = [S "We consider an arbitrary" +:+. phrase vo, S "The",
  phrase vhg, S "is assumed constant. Then (1) can be written as"]

genDefDesc4 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  AssumpChunk -> AssumpChunk -> AssumpChunk -> [Sentence]
genDefDesc4 hfi hfo iS oS den hcs te vo assump3 assump4 assump5 = [S "Where", getES hfi `sC`
  getES hfo `sC` getES iS `sC` S "and", getES oS, S "are explained in" +:+.
  acroGD 2, S "Assuming", getES den `sC` getES hcs `sAnd` getES te,
  S "are constant over the", phrase vo `sC` S "which is true in our case by",
  titleize' assumption, (sParen (makeRef assump3)) `sC` (sParen (makeRef assump4))
  `sC` S "and", (sParen (makeRef assump5)) `sC` S "we have"]

genDefDesc5 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> [Sentence]
genDefDesc5 den ma vo = [S "Using the fact that", getES den :+: S "=" :+:
  getES ma :+: S "/" :+: getES vo `sC` S "(2) can be written as"]

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
