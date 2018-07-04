module Drasil.NoPCM.IMods (eBalanceOnWtr, eBalanceOnWtr_new) where

import Language.Drasil

import Drasil.SWHS.Concepts (water, tank)
import Drasil.SWHS.Unitals (temp_W, temp_C, tau_W, w_mass, htCap_W, coil_HTC, coil_SA, temp_init
  , time_final, w_vol, ht_flux_C, vol_ht_gen)
import Data.Drasil.Utils (unwrap, weave)
import Data.Drasil.SentenceStructures (foldlSent, isThe,
  sAnd, foldlList, ofThe, acroGD, foldlSentCol, sOf, getES)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Concepts.Math (equation, rOfChng)
import Data.Drasil.Concepts.PhysicalProperties (liquid)
import Data.Drasil.Concepts.Thermodynamics (melting, boil_pt, heat_cap_spec, 
  heat_trans)

import Data.Drasil.Quantities.PhysicalProperties (vol, mass)
import Drasil.SWHS.DataDefs(dd1HtFluxC)
import Drasil.SWHS.Assumptions
---------
-- IM1 --
---------
--im :: RelationConcept -> Inputs -> InputConstraints -> Outputs -> 
-- OutputConstraints -> Attributes -> InstanceModel
--Tcon :: Expr -> Constraint
eBalanceOnWtr_new :: InstanceModel
eBalanceOnWtr_new = im'' eBalanceOnWtr [qw temp_C, qw temp_init, qw time_final, 
  qw coil_SA, qw coil_HTC, qw htCap_W, qw w_mass] 
  [TCon AssumedCon $sy temp_init $<= sy temp_C] (qw temp_W) 
  --Tw(0) cannot be presented, there is one more constraint Tw(0) = Tinit
  [TCon AssumedCon $ 0 $< sy time $< sy time_final] eBalanceOnWtr_deriv_nopcm "eBalanceOnWtr" [balWtrDesc_note]

eBalanceOnWtr :: RelationConcept
eBalanceOnWtr = makeRC "eBalanceOnWtr" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtr_Rel

balWtr_Rel :: Relation
balWtr_Rel = (deriv (sy temp_W) time) $= 1 / (sy tau_W) *
  (((sy temp_C) - (apply1 temp_W time)))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), 
  (E $ sy temp_C) `isThe` phrase temp_C +:+. sParen (unwrap $ getUnit temp_C),
  (E $ sy tau_W $= (sy w_mass * sy htCap_W) / (sy coil_HTC * sy coil_SA)),
  S "is a constant" +:+. sParen (unwrap $ getUnit tau_W),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ 0 $< sy temp_W $< 100),
  sParen (unwrap $ getUnit temp_W), S "where", E 0,
  sParen (unwrap $ getUnit temp_W) `sAnd` (E 100),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boil_pt, S "of", phrase water `sC` S "respectively",
  sParen (makeRef assump10)]

balWtrDesc_note :: Sentence
balWtrDesc_note = foldlSent [S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ 0 $< sy temp_W $< 100),
  sParen (unwrap $ getUnit temp_W), S "where", E 0,
  sParen (unwrap $ getUnit temp_W) `sAnd` (E 100),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boil_pt, S "of", phrase water `sC` S "respectively",
  sParen (makeRef assump10)]
  
----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
eBalanceOnWtr_deriv_nopcm :: Derivation
eBalanceOnWtr_deriv_nopcm =
  [S "Derivation of the" +:+ phrase energy +:+ S "balance on water:"] ++
  (weave [eBalanceOnWtr_deriv_sentences_nopcm, map E eBalanceOnWtr_deriv_eqns_nopcm])

eBalanceOnWtr_deriv_sentences_nopcm :: [Sentence]
eBalanceOnWtr_deriv_sentences_nopcm = map foldlSentCol [
  s4_2_3_desc1_nopcm rOfChng temp_W energy water vol w_vol mass w_mass heat_cap_spec
    htCap_W heat_trans ht_flux_C coil_SA tank newA11 newA12 vol_ht_gen, 
  s4_2_3_desc2_nopcm dd1HtFluxC,
  s4_2_3_desc3_nopcm eq1,
  s4_2_3_desc4_nopcm eq2]

s4_2_3_desc1_nopcm :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk -> UnitalChunk -> 
  UnitalChunk -> UnitalChunk -> UnitalChunk -> ConceptChunk -> UncertQ -> ConceptChunk -> 
  UnitalChunk -> UncertQ -> ConceptChunk -> AssumpChunk -> AssumpChunk -> UnitalChunk -> [Sentence]
s4_2_3_desc1_nopcm roc tw en wt vo wvo ms wms hcs hw ht hfc cs tk ass11 ass12 vhg =
  [S "To find the", phrase roc `sOf` (E $ sy tw) `sC` S "we look at the",
   phrase en, S "balance on" +:+. phrase wt, S "The", phrase vo, S "being considered" 
   `isThe` (phrase vo `sOf` phrase wt), (E $ sy wvo) `sC` S "which has", phrase ms,
   (E $ sy wms) `sAnd` (phrase hcs `sOf` phrase wt) `sC` (E $ sy hw), 
    S ". Heat transfer occurs in the", phrase wt, S "from the coil as", 
    (E $ sy hfc) `sC` S "over area" +:+. (E $ sy cs), S "No", phrase ht,
    S "occurs to", S "outside" `ofThe` phrase tk `sC` 
    S "since it has been assumed to be perfectly insulated", 
    (sParen (makeRef ass11)), S ". Assuming no volumetric", 
    S "heat generation per unit", phrase vo,
    (sParen (makeRef ass12)) `sC` (E $ sy vhg $= 0), S ". Therefore, the equation for",
     acroGD 2, S "can be written as"]

s4_2_3_desc2_nopcm :: QDefinition -> [Sentence]
s4_2_3_desc2_nopcm dd1HtFluxC =
  [S "Using", makeRef $ datadefn dd1HtFluxC, S ", this can be written as"]

s4_2_3_desc3_nopcm :: Expr-> [Sentence]
s4_2_3_desc3_nopcm eq11 = [S "Dividing (3) by", (E eq11) `sC` S "we obtain"]

s4_2_3_desc4_nopcm :: [Sentence]-> [Sentence]
s4_2_3_desc4_nopcm eq22 = 
  [S "Setting"] ++ eq22 ++ [S ", Equation (4) can be written in its final form as"]

eq1:: Expr
eq1 = (sy w_mass) * (sy htCap_W)

eq2:: [Sentence]
eq2 = [getES tau_W, S "=", getES w_mass, getES htCap_W, S "/", getES coil_HTC, getES coil_SA]

s4_2_3_eq1_nopcm, s4_2_3_eq2_nopcm, s4_2_3_eq3_nopcm, s4_2_3_eq4_nopcm :: Expr

s4_2_3_eq1_nopcm = (sy w_mass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy ht_flux_C) * (sy coil_SA)

s4_2_3_eq2_nopcm = (sy w_mass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy coil_HTC) * (sy coil_SA) *  ((sy temp_C) - (sy temp_W))

s4_2_3_eq3_nopcm = (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W))

s4_2_3_eq4_nopcm =  
  (deriv (sy temp_W) time) $= 1 / (sy tau_W) * ((sy temp_C) - (sy temp_W))


eBalanceOnWtr_deriv_eqns_nopcm :: [Expr]
eBalanceOnWtr_deriv_eqns_nopcm = [s4_2_3_eq1_nopcm, s4_2_3_eq2_nopcm, s4_2_3_eq3_nopcm, s4_2_3_eq4_nopcm]
