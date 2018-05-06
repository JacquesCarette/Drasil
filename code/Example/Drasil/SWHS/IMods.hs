module Drasil.SWHS.IMods (insta_model_IMods,
   eBalanceOnWtr_new,  heatEInWtr_new,
   heatEInPCM_new,  eBalanceOnPCM_new, heatEInWtr) where

import Language.Drasil

import Drasil.DocumentLanguage (mkAssump)

import Drasil.SWHS.Unitals {-(t_init_melt, latentE_P, pcm_E, pcm_initMltE,
  temp_melt_P, temp_PCM, htCap_L_P, pcm_mass, htFusion, temp_init, htCap_S_P,
  melt_frac, temp_W, w_mass, w_E, htCap_W, tau_S_P, pcm_SA, tau_L_P, pcm_HTC,
  coil_SA, coil_HTC, eta, tau_W, temp_C, time_final)-}
import Data.Drasil.Utils (getES, unwrap, weave)
import Data.Drasil.SentenceStructures (acroT, acroDD, foldlSent, isThe,
  sAnd, ofThe, foldlSent, isThe, foldlList, acroGD, foldlSentCol, sOf)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Concepts.Math (equation, change, rOfChng, surface, area)
import Drasil.SWHS.Concepts (phsChgMtrl, water, coil, tank)
import Data.Drasil.Concepts.PhysicalProperties (solid, liquid, mass, vol)
import Data.Drasil.Concepts.Thermodynamics (boiling, heat, temp, melting,
  latent_heat, sens_heat, heat_cap_spec, thermal_energy, boil_pt, heat_trans,
  phase_change, ht_flux)

import Drasil.SWHS.DataDefs(dd1HtFluxC, dd2HtFluxP)
import Data.Drasil.Concepts.Documentation (assumption, dataDefn)
--s4_2_5_IMods
insta_model_IMods :: [RelationConcept]
insta_model_IMods = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

---------
-- IM1 --
---------
eBalanceOnWtr_new :: InstanceModel
eBalanceOnWtr_new = im eBalanceOnWtr [qw time, qw tau_W, qw temp_C, qw eta,
 qw temp_PCM, qw time_final, qw temp_init, qw coil_SA]
  [TCon AssumedCon $ sy temp_init $< sy temp_C] [qw temp_W]
   [TCon AssumedCon $ 0 $< sy time $< sy time_final] [D eBalanceOnWtr_deriv_swhs]

eBalanceOnWtr :: RelationConcept
eBalanceOnWtr = makeRC "eBalanceOnWtr" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtr_Rel

balWtr_Rel :: Relation
balWtr_Rel = (deriv (sy temp_W) time) $= 1 / (sy tau_W) *
  (((sy temp_C) - (apply1 temp_W time)) +
  (sy eta) * ((apply1 temp_PCM time) - (apply1 temp_W time)))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ sy temp_C) `isThe` phrase temp_C +:+. sParen (unwrap $ getUnit temp_C),
  (E $ sy tau_W $= (sy w_mass * sy htCap_W) / (sy coil_HTC * sy coil_SA)),
  S "is a constant" +:+. sParen (unwrap $ getUnit tau_W),
  (E $ sy eta $= (sy pcm_HTC * sy pcm_SA) / (sy coil_HTC * sy coil_SA)),
  S "is a constant" +:+. sParen (S "dimensionless"),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ real_interval temp_W (Bounded (Exc 0) (Exc 100))),
  sParen (unwrap $ getUnit temp_W), S "where", E 0,
  sParen (unwrap $ getUnit temp_W) `sAnd` (E 100),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boil_pt, S "of", phrase water `sC` S "respectively",
  sParen (makeRef (mkAssump "assump14" EmptyS) `sC`
  makeRef (mkAssump "assump19" EmptyS))]


  ----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
eBalanceOnWtr_deriv_swhs :: Derivation
eBalanceOnWtr_deriv_swhs =
  [S "Detailed derivation of the" +:+ phrase energy +:+ S "balance on water"] ++
  (weave [eBalanceOnWtr_deriv_sentences_swhs_im1, map E eBalanceOnWtr_deriv_eqns_swhs_im1])

eBalanceOnWtr_deriv_sentences_swhs_im1 :: [Sentence]
eBalanceOnWtr_deriv_sentences_swhs_im1 = map foldlSentCol [
  s4_2_3_desc1_swhs_im1 rOfChng temp_W energy water vol w_vol mass w_mass heat_cap_spec
    htCap_W heat_trans coil ht_flux_C coil_SA pcm_SA tank [S "A11"] [S "A12"] ht_flux_P surface,
  s4_2_3_desc2_swhs_im1 dd1HtFluxC ht_flux_C ht_flux_P,
  s4_2_3_desc3_swhs_im1 eq1,
  s4_2_3_desc4_swhs_im1 eq2,
  s4_2_3_desc5_swhs_im1,
  s4_2_3_desc6_swhs_im1 eq3 eq4,
  s4_2_3_desc7_swhs_im1 eq5]

s4_2_3_desc1_swhs_im1 :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk -> 
  ConceptChunk -> UnitalChunk -> ConceptChunk -> UnitalChunk -> ConceptChunk -> UncertQ -> 
  ConceptChunk -> ConceptChunk -> UnitalChunk -> UncertQ -> UncertQ -> ConceptChunk ->
  [Sentence] -> [Sentence] -> UnitalChunk -> ConceptChunk -> [Sentence]
s4_2_3_desc1_swhs_im1 roc tw en wt vo wvo ms wms hcs hw ht cl hfc cs ps tk ass11 ass12 hfp su =
  [S "To find the", phrase roc `sOf` (E $ sy tw) `sC` S "we look at the",
   phrase en, S "balance on" +:+. phrase wt, S "The", phrase vo, S "being considered" 
   `isThe` (phrase vo `sOf` phrase wt), (E $ sy wvo) `sC` S "which has", phrase ms,
   (E $ sy wms) `sAnd` (phrase hcs `sOf` phrase wt) `sC` (E $ sy hw), 
    (E $ sy hfc), S "represents the", phrase ht, S "flux into the", phrase wt,
    S "from the", (phrase cl `sAnd` (E $ sy hfp)), S "represents the", phrase ht,
    S "flux into the PCM from", phrase wt `sC` S "over heating", phrase cl, phrase su,
    S "area and", phrase phase_change, S "material", phrase su, S "area of",
    (E $ sy cs) `sAnd` (E $ sy ps) `sC` S "respectively.",  S "No", phrase ht,
    S "occurs to", S "outside" `ofThe` phrase tk `sC` 
    S "since it has been assumed to be perfectly insulted", 
    (foldlList $ (map (\d -> sParen (d))) ass11), S ". Assuming no volumetric", 
    phrase ht, S "generation per unit", phrase vo,
    (foldlList $ (map (\d -> sParen (d))) ass12) `sC` S "g = 0. Therefore, the equation for",
     acroGD 2, S "can be written as"]

s4_2_3_desc2_swhs_im1 :: QDefinition -> UnitalChunk -> UnitalChunk -> [Sentence]
s4_2_3_desc2_swhs_im1 dd1 hfc hfp =
  [S "Using", titleize' dataDefn, makeRef $ datadefn dd1, S "and",
  titleize' dataDefn, makeRef $ datadefn dd1,
   S "for", (E $ sy hfc) `sAnd` (E $ sy hfp), S "respectively, this can be written as"]

s4_2_3_desc3_swhs_im1 :: [Sentence] -> [Sentence]
s4_2_3_desc3_swhs_im1 eq11 = [S "Dividing (3) by"] ++ eq11 ++ [S "we obtain"]

s4_2_3_desc4_swhs_im1 :: [Sentence] -> [Sentence]
s4_2_3_desc4_swhs_im1 eq22 = [S "Factoring the negative sign out of the second term",
  S "of the RHS of Equation (4) and multiplying it by"] ++ eq22 ++ [S "yields"]

s4_2_3_desc5_swhs_im1 ::[Sentence]
s4_2_3_desc5_swhs_im1 = [S "Which simplifies to"]

s4_2_3_desc6_swhs_im1 :: Expr -> Expr -> [Sentence]
s4_2_3_desc6_swhs_im1 eq33 eq44 = 
  [S "Setting", (E eq33) `sAnd` (E eq44), S "Equation (5) can be written in its final form as"]

s4_2_3_desc7_swhs_im1 :: Expr -> [Sentence]
s4_2_3_desc7_swhs_im1 eq55 = 
  [S "Finally, factoring out", (E eq55), S ", we are left with the governing ODE for IM1"]

eq1, eq2:: [Sentence]
eq1 = [getES w_mass, getES htCap_W]
eq2 = [getES tau_W, S "= (", getES w_mass, S "*", getES htCap_W, S ") / (", getES ht_flux_C,
  S "*", getES coil_SA, S ")"]

eq3, eq4, eq5:: Expr
eq3 = (sy tau) $= ((sy w_mass) * (sy htCap_W)) / ((sy coil_HTC) * (sy coil_SA))
eq4 = (sy eta) $= ((sy pcm_HTC) * (sy pcm_SA)) / 
  ((sy coil_HTC) * (sy coil_SA))
eq5 = 1 / (sy tau_W)

s4_2_3_eq1_swhs_im1, s4_2_3_eq2_swhs_im1, s4_2_3_eq3_swhs_im1,
 s4_2_3_eq4_swhs_im1, s4_2_3_eq5_swhs_im1, s4_2_3_eq6_swhs_im1, s4_2_3_eq7_swhs_im1 :: Expr

s4_2_3_eq1_swhs_im1 = (sy w_mass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy ht_flux_C) * (sy coil_SA) - (sy ht_flux_P) * (sy pcm_SA)

s4_2_3_eq2_swhs_im1 = (sy w_mass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy coil_HTC) * (sy coil_SA) *  ((sy temp_C) - (sy temp_W)) -
  (sy pcm_HTC) * (sy pcm_SA) *  ((sy temp_W) - (sy temp_PCM))

s4_2_3_eq3_swhs_im1 = (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) -
  ((sy pcm_mass) * (sy pcm_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_W) - (sy temp_PCM))

s4_2_3_eq4_swhs_im1 = 
  (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) +
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy coil_HTC) * (sy coil_SA))) * ((sy pcm_HTC) * (sy pcm_SA) / 
  ((sy w_mass) * (sy coil_HTC))) * ((sy temp_PCM) - (sy temp_W))

s4_2_3_eq5_swhs_im1 =  
  (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) +
  ((sy pcm_HTC) * (sy pcm_SA) / 
  ((sy coil_HTC) * (sy coil_SA))) * ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy coil_HTC))) * ((sy temp_PCM) - (sy temp_W))


s4_2_3_eq6_swhs_im1 = (deriv (sy temp_W) time) $= 
  1 / (sy tau_W) * ((sy temp_C) - (sy temp_W)) +
  (sy eta) / (sy tau_W) * ((sy temp_PCM) - (sy temp_W))

s4_2_3_eq7_swhs_im1 =  
  (deriv (sy temp_W) time) $= 1 / (sy tau_W) * (((sy temp_C) - (sy temp_W)) +
  sy eta * ((sy temp_PCM) - (sy temp_W)))


eBalanceOnWtr_deriv_eqns_swhs_im1 :: [Expr]
eBalanceOnWtr_deriv_eqns_swhs_im1 = [s4_2_3_eq1_swhs_im1, s4_2_3_eq2_swhs_im1,
 s4_2_3_eq3_swhs_im1, s4_2_3_eq4_swhs_im1, s4_2_3_eq5_swhs_im1, s4_2_3_eq6_swhs_im1,
 s4_2_3_eq7_swhs_im1]


---------
-- IM2 --
---------
eBalanceOnPCM_new :: InstanceModel
eBalanceOnPCM_new = im eBalanceOnPCM [qw time, qw tau_W, qw temp_C, qw eta,
 qw temp_PCM, qw time_final, qw temp_init, qw coil_SA]
  [TCon AssumedCon $ sy temp_init $< sy temp_C] [qw temp_W]
   [TCon AssumedCon $ 0 $< sy time $< sy time_final] [D eBalanceOnPCM_deriv_swhs]

eBalanceOnPCM :: RelationConcept
eBalanceOnPCM = makeRC "eBalanceOnPCM" (nounPhraseSP
  "Energy balance on PCM to find T_p")
  --FIXME: T_p should be called from symbol
  balPCMDesc balPCM_Rel

balPCM_Rel :: Relation
balPCM_Rel = (deriv (sy temp_PCM) time) $= case_ [case1, case2, case3, case4]
  where case1 = ((1 / (sy tau_S_P)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpTo $ Exc (sy temp_melt_P)))

        case2 = ((1 / (sy tau_L_P)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpFrom $ Exc (sy temp_melt_P)))

        case3 = (0, (sy temp_PCM) $= (sy temp_melt_P))

        case4 = (0, real_interval melt_frac (Bounded (Exc 0) (Exc 1)))

balPCMDesc :: Sentence
balPCMDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ (sy tau_S_P) $= ((sy pcm_mass) * (sy htCap_S_P)) /
  ((sy pcm_HTC) * (sy pcm_SA))), S "is a constant" +:+.
  sParen (unwrap $ getUnit tau_S_P), (E $ (sy tau_L_P) $=
  ((sy pcm_mass) * (sy htCap_L_P)) / ((sy pcm_HTC) * (sy pcm_SA))),
  S "is a constant", sParen (unwrap $ getUnit tau_S_P)]




 ----------------------------------------------
--    Derivation of eBalanceOnPCM          --
----------------------------------------------
eBalanceOnPCM_deriv_swhs :: Derivation
eBalanceOnPCM_deriv_swhs =
  [S "Detailed derivation of the" +:+ phrase energy +:+ S "balance on the PCM during " +:+ 
    phrase sens_heat +:+ S "phase:" ] ++
  (weave [eBalanceOnPCM_deriv_sentences_swhs_im2, map E eBalanceOnPCM_deriv_eqns_swhs_im2])

eBalanceOnPCM_deriv_sentences_swhs_im2 :: [Sentence]
eBalanceOnPCM_deriv_sentences_swhs_im2 = map foldlSentCol [
  s4_2_3_desc1_swhs_im2 rOfChng temp_PCM energy water vol pcm_vol pcm_mass heat_cap_spec
    htCap_S_P ht_flux ht_flux_P phase_change pcm_SA heat_trans [S "A12"],
  s4_2_3_desc2_swhs_im2 dd2HtFluxP ht_flux_P,
  s4_2_3_desc3_swhs_im2 eq6,
  s4_2_3_desc4_swhs_im2 eq7,
  s4_2_3_desc5_swhs_im2 htCap_S_P htCap_L_P tau_S_P tau_L_P surface area melting vol temp_PCM temp_melt_P
    temp phase_change boiling solid liquid]

s4_2_3_desc1_swhs_im2 :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk -> 
  ConceptChunk-> UncertQ -> UnitalChunk -> ConceptChunk -> UncertQ -> 
  ConceptChunk -> UnitalChunk -> ConceptChunk -> UncertQ ->  ConceptChunk ->
  [Sentence] -> [Sentence]
s4_2_3_desc1_swhs_im2 roc tempP en wt vo pcmvo pm hcs hsp hf hfp pc ps ht ass13 =
  [S "To find the", phrase roc `sOf` (E $ sy tempP) `sC` S "we look at the",
   phrase en, S "balance on" +:+. S "PCM", S "The", phrase vo, S "being considered" 
   `isThe` (phrase vo `sOf` S "PCM,") +:+. (E $ sy pcmvo), S "The derivation that follows is" +:+. 
   S "initially for the solid PCM", S "The mass of phase change material is", (E $ sy pm) `sAnd` S "the",
   phrase hcs `sOf` S "PCM as a solid is" +:+. (E $ sy hsp), S "The", phrase hf,
   S "into the PCM from", phrase wt, S "is", (E $ sy hfp), S "over", phrase pc, 
   S "material is surface area" +:+. (E $ sy ps), S "There is no", phrase hf +:+. S "output",
   S "Assuming no volumetric", phrase ht, S "generation per unit", phrase vo,
    (foldlList $ (map (\d -> sParen (d))) ass13) `sC` S "g = 0, the equation for",
     acroGD 2, S "can be written as"]

s4_2_3_desc2_swhs_im2 :: QDefinition -> UnitalChunk -> [Sentence]
s4_2_3_desc2_swhs_im2 dd2 hfp =
  [S "Using", titleize' dataDefn, makeRef $ datadefn dd2,
   S "for", (E $ sy hfp) `sC` S "this equation can be written as"]

s4_2_3_desc3_swhs_im2 :: [Sentence] -> [Sentence]
s4_2_3_desc3_swhs_im2 eq66 = 
  [S "Dividing by"] ++ eq66 ++ [S "we obtain"]

s4_2_3_desc4_swhs_im2 :: Expr -> [Sentence]
s4_2_3_desc4_swhs_im2 eq77 = 
  [S "Setting", (E eq77) `sC` S "this can be written as"]

s4_2_3_desc5_swhs_im2 ::  UncertQ -> UncertQ -> UnitalChunk -> UnitalChunk -> ConceptChunk -> ConceptChunk-> ConceptChunk
  -> ConceptChunk -> UncertQ -> UncertQ -> ConceptChunk -> ConceptChunk -> ConceptChunk -> ConceptChunk -> ConceptChunk -> [Sentence]
s4_2_3_desc5_swhs_im2 hsp hlp tsp tlp sur ar melt vo cg tp tempa pc boil sld lqd= 
  [S "Equation (6) applies for the solid PCM. In the case where all of the PCM is melted, the same",
   S "derivation applies, except that", (E $ sy hsp), S "is replaced by", (E $ sy hlp),
   S "and thus", (E $ sy tsp), S "is replaced by" +:+. (E $ sy tlp), S "Although a small change in",
   phrase sur, phrase ar, S "would be expected with", phrase melt, S "this is not included" `sC`
   S "since the", phrase vo, phrase cg, S "of the PCM with", phrase melting, S "is assumed to be negligible" +:+.
   S "A(13)", S "In the case where", (E $ sy tp), S "and not all of the PCM is melted" `sC`
   phrase tempa, S "of the", phrase pc +:+. S "material does not change", S "Therefore" `sC` 
   S "in this case" +:+. S "This derivation does not consider the",
   phrase boil, S "of the PCM" `sC` S "as the PCM is assumed to either be in a", phrase sld, S "state or a",
   phrase lqd, S "state (A14)"]
--(E $ ((deriv (sy temp_PCm) time) $= 0)
eq6:: [Sentence]
eq6 = [getES pcm_mass, getES htCap_S_P]

eq7 :: Expr
eq7 = (sy tau_S_P) $= ((sy pcm_mass) * (sy htCap_S_P)) / 
  ((sy ht_flux_P) * (sy pcm_SA))

s4_2_3_eq1_swhs_im2, s4_2_3_eq2_swhs_im2, s4_2_3_eq3_swhs_im2,
 s4_2_3_eq4_swhs_im2 :: Expr

s4_2_3_eq1_swhs_im2 = (sy pcm_mass) * (sy htCap_S_P) * (deriv (sy temp_PCM) time) $= 
  (sy ht_flux_P) * (sy pcm_SA)

s4_2_3_eq2_swhs_im2 = (sy pcm_mass) * (sy htCap_S_P) * (deriv (sy temp_PCM) time) $= 
  (sy pcm_HTC) * (sy pcm_SA) *  ((sy temp_W) - (sy temp_PCM))

s4_2_3_eq3_swhs_im2 = (deriv (sy temp_PCM) time) $= 
  (((sy pcm_HTC) * (sy pcm_SA)) / ((sy pcm_mass) * (sy htCap_S_P))) *  ((sy temp_W) - (sy temp_PCM))

s4_2_3_eq4_swhs_im2 = 
  (deriv (sy temp_PCM) time) $= 
  (1 / sy tau_S_P) * ((sy temp_W) - (sy temp_PCM))


eBalanceOnPCM_deriv_eqns_swhs_im2 :: [Expr]
eBalanceOnPCM_deriv_eqns_swhs_im2 = [s4_2_3_eq1_swhs_im2, s4_2_3_eq2_swhs_im2,
 s4_2_3_eq3_swhs_im2, s4_2_3_eq4_swhs_im2]


---------
-- IM3 --
---------
heatEInWtr_new :: InstanceModel
heatEInWtr_new = im heatEInWtr [qw temp_init, qw coil_SA, qw htCap_W, qw w_mass] 
  [] [qw w_E] [TCon AssumedCon $ 0 $< sy time $< sy time_final] []

heatEInWtr :: RelationConcept
heatEInWtr = makeRC "heatEInWtr" (nounPhraseSP "Heat energy in the water")
  htWtrDesc htWtr_Rel

htWtr_Rel :: Relation
htWtr_Rel = (apply1 w_E time) $= (sy htCap_W) * (sy w_mass) *
  ((apply1 temp_W time) - sy temp_init)

htWtrDesc :: Sentence
htWtrDesc = foldlSent [S "The above", phrase equation,
  S "is derived using" +:+. acroT 2, getES w_E `isThe` phrase change,
  S "in", phrase thermal_energy, S "of the", phrase liquid,
  phrase water, S "relative to the", phrase energy, S "at the initial",
  phrase temp, sParen (getES temp_init) +:+.
  sParen (unwrap $ getUnit pcm_initMltE), (getES htCap_W) `isThe`
  phrase heat_cap_spec, S "of", phrase liquid, phrase water,
  sParen (unwrap $ getUnit htCap_S_P) `sAnd` (getES w_mass)
  `isThe` phrase mass, S "of the", phrase water +:+.
  sParen (unwrap $ getUnit w_mass), S "The", phrase change, S "in",
  phrase temp, S "is the difference between the", phrase temp, S "at",
  phrase time, getES time, sParen (unwrap $ getUnit t_init_melt) `sC`
  (getES temp_W) `sAnd` S "the", phrase temp_init `sC` getES temp_init +:+.
  sParen (unwrap $ getUnit temp_init), S "This", phrase equation,
  S "applies as long as", (E $ real_interval temp_W (Bounded (Exc 0) (Exc 100)))
  :+: (unwrap $ getUnit temp_W),
  sParen $ makeRef (mkAssump "assump14" EmptyS) `sC`
  makeRef (mkAssump "assump19" EmptyS)]

---------
-- IM4 --
---------
heatEInPCM_new :: InstanceModel
heatEInPCM_new = im heatEInPCM [qw time, qw tau_W, qw temp_C, qw eta,
 qw temp_PCM, qw time_final, qw temp_init, qw coil_SA]
  [TCon AssumedCon $ sy temp_init $< sy temp_C] [qw temp_W]
   [TCon AssumedCon $ 0 $< sy time $< sy time_final] []

heatEInPCM :: RelationConcept
heatEInPCM = makeRC "heatEInPCM" (nounPhraseSP "Heat energy in the PCM")
  htPCMDesc htPCM_Rel

htPCM_Rel :: Relation
htPCM_Rel = sy pcm_E $= case_ [case1, case2, case3, case4]
  where case1 = (sy htCap_S_P * sy pcm_mass * ((apply1 temp_PCM time) -
          sy temp_init), real_interval temp_PCM (UpTo $ Exc $ sy temp_melt_P))

        case2 = (sy pcm_initMltE + (sy htFusion * sy pcm_mass) +
          (sy htCap_L_P * sy pcm_mass * ((apply1 temp_PCM time) -
          sy temp_melt_P)), real_interval temp_PCM (UpFrom $ Exc $ sy temp_melt_P))

        case3 = (sy pcm_initMltE + (apply1 latentE_P time),
          (sy temp_PCM) $= (sy temp_melt_P))

        case4 = (sy pcm_initMltE + (apply1 latentE_P time),
          real_interval melt_frac (Bounded (Exc 0) (Exc 1)))

htPCMDesc :: Sentence
htPCMDesc = foldlSent [S "The above", phrase equation,
  S "is derived using" +:+. (acroT 2 `sAnd` acroT 3), getES pcm_E `isThe`
  phrase change, S "in", phrase thermal_energy, S "of the", short phsChgMtrl,
  S "relative to the", phrase energy, S "at the", phrase temp_init,
  sParen (getES temp_init) +:+. (unwrap $ getUnit pcm_initMltE), getES pcm_E,
  S "for the", phrase solid, short phsChgMtrl, S "is found using", acroT 2,
  S "for", phrase sens_heat, S "ing, with", phrase heat_cap_spec `ofThe`
  phrase solid, short phsChgMtrl `sC` getES htCap_S_P,
  sParen (unwrap $ getUnit htCap_S_P), S "and the", phrase change, S "in the",
  short phsChgMtrl, phrase temp, S "from the", phrase temp_init +:+.
  sParen (unwrap $ getUnit temp_init), getES pcm_E, S "for the melted",
  short phsChgMtrl, sParen (E (sy temp_PCM $> sy pcm_initMltE)),
  S "is found using", acroT 2, S "for", phrase sens_heat, S "of the" +:+.
  phrase liquid, short phsChgMtrl, S "plus the", phrase energy, S "when",
  phrase melting, S "starts, plus the", phrase energy,
  S "required to melt all of the", short phsChgMtrl, S "The", phrase energy,
  S "when", phrase melting, S "starts is", getES pcm_initMltE +:+.
  sParen (unwrap $ getUnit pcm_initMltE), S "The", phrase energy,
  S "required to melt all of the", short phsChgMtrl, S "is",
  E (sy htFusion * sy pcm_mass), sParen (unwrap $ getUnit pcm_initMltE) +:+.
  sParen (acroDD 3), phrase heat_cap_spec `ofThe` phrase liquid,
  short phsChgMtrl, S "is", getES htCap_L_P,
  sParen (unwrap $ getUnit htCap_L_P) `sAnd` S "the", phrase change, S "in",
  phrase temp, S "is", E (sy temp_PCM - sy temp_melt_P) +:+.
  sParen (unwrap $ getUnit temp_melt_P), getES pcm_E, S "during",
  phrase melting, S "of the", short phsChgMtrl, S "is found using the", 
  phrase energy, S "required at", S "instant" +:+ phrase melting `ofThe`
  short phsChgMtrl, S "begins" `sC` getES pcm_initMltE, S "plus the",
  phrase latent_heat, phrase energy, S "added to the", short phsChgMtrl `sC`
  getES latentE_P, sParen (unwrap $ getUnit latentE_P), S "since the",
  phrase time, S "when", phrase melting, S "began", getES t_init_melt +:+.
  sParen (unwrap $ getUnit t_init_melt), S "The", phrase heat, phrase energy,
  S "for", phrase boiling, S "of the", short phsChgMtrl,
  S "is not detailed" `sC` S "since the", short phsChgMtrl,
  S "is assumed to either be in a", phrase solid, S "or", phrase liquid,
  S "state", sParen (makeRef (mkAssump "assump18" EmptyS))]



{--varWithDesc :: N c => c -> Sentence
varWithDesc conceptVar = (E $ sy conceptVar) `isThe` phrase conceptVar +:+.
  sParen (unwrap $ getUnit conceptVar)

--need to create a wrapper
--}
