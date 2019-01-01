module Drasil.SWHS.IMods (swhsIMods, eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM) where

import Language.Drasil
import Language.Drasil.Development (getUnit) -- FIXME? Points to an oddity

import Data.Drasil.Utils (unwrap, weave)
import Data.Drasil.SentenceStructures (foldlSent, foldlSentCol, isThe, ofThe, sAnd, sOf)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Concepts.Math (area, change, equation, rOfChng, surface)
import Data.Drasil.Concepts.PhysicalProperties (liquid, mass, solid, vol)
import Data.Drasil.Concepts.Thermodynamics (boil_pt, boiling, heat, heat_cap_spec, 
  heat_trans, ht_flux, latent_heat, melting, phase_change, sens_heat, temp, thermal_energy)

import Drasil.SWHS.Assumptions (newA11, newA12, newA13, newA14, newA15, newA16, newA17, newA18, newA19)
import Drasil.SWHS.Concepts (coil, phsChgMtrl, tank, water)
import Drasil.SWHS.DataDefs (dd1HtFluxC, dd2HtFluxP, dd3HtFusion, dd4MeltFrac)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.TMods (sensHtE, latentHtE)
import Drasil.SWHS.Unitals (coil_HTC, coil_SA, eta, ht_flux_C, ht_flux_P, htCap_L_P, 
  htCap_S_P, htCap_W, htFusion, latentE_P, melt_frac, pcm_E, pcm_HTC, pcm_initMltE, 
  pcm_mass, pcm_SA, pcm_vol, t_init_melt, tau_L_P, tau_S_P, tau_W, temp_C, temp_init, 
  temp_melt_P, temp_PCM, temp_W, time_final, vol_ht_gen, w_E, w_mass, w_vol)
import Drasil.SWHS.GenDefs (rocTempSimp)

import Control.Lens ((^.))

swhsIMods :: [InstanceModel]
swhsIMods = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

---------
-- IM1 --
---------
eBalanceOnWtr :: InstanceModel
eBalanceOnWtr = deModel eBalanceOnWtr_rc [qw w_mass, qw htCap_W, qw coil_HTC, qw pcm_SA,
 qw pcm_HTC, qw coil_SA, qw temp_PCM, qw time_final, qw temp_C, qw temp_init]
  [sy temp_init $< sy temp_C] (qw temp_W)
   [0 $< sy time $< sy time_final] [koothoor2013] eBalanceOnWtrDeriv
   "eBalanceOnWtr" [balWtrDesc]

eBalanceOnWtr_rc :: RelationConcept
eBalanceOnWtr_rc = makeRC "eBalanceOnWtr_rc" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtr_Rel 
  -- eBalanceOnWtrL

balWtr_Rel :: Relation
balWtr_Rel = (deriv (sy temp_W) time) $= 1 / (sy tau_W) *
  (((sy temp_C) - (apply1 temp_W time)) +
  (sy eta) * ((apply1 temp_PCM time) - (apply1 temp_W time)))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ sy tau_W) `sC` (E $ sy time_final)
  `sC` (E $ sy temp_C) `sC` (E $ sy temp_PCM), S "from" +:+.
  sParen (makeRef2S eBalanceOnPCM), S "The input is constrained so that" +:+.
  (E $ sy temp_init $<= (sy temp_C)), sParen (makeRef2S newA11),
  (E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ sy temp_C) `isThe` phrase temp_C +:+. sParen (unwrap $ getUnit temp_C),
  (E $ sy tau_W $= (sy w_mass * sy htCap_W) / (sy coil_HTC * sy coil_SA)),
  S "is a constant", sParen (makeRef2S dd3HtFusion) +:+. sParen (unwrap $ getUnit tau_W),
  (E $ sy eta $= (sy pcm_HTC * sy pcm_SA) / (sy coil_HTC * sy coil_SA)),
  S "is a constant" +:+. sParen (S "dimensionless"),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ real_interval temp_W (Bounded (Exc,0) (Exc,100))),
  sParen (unwrap $ getUnit temp_W), S "where", E 0,
  sParen (unwrap $ getUnit temp_W) `sAnd` (E 100),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boil_pt, S "of", phrase water `sC` S "respectively",
  sParen (makeRef2S newA14 `sC` makeRef2S newA19)]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
-- type Derivation = [Sentence]
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv =
  [S "Derivation of the" +:+ phrase energy +:+ S "balance on water:"] ++
  (weave [eBalanceOnWtrDerivSentences, map E eBalanceOnWtr_deriv_eqns__im1])

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = map foldlSentCol [
  eBalanceOnWtrDerivDesc1 rOfChng temp_W energy water vol w_vol mass w_mass heat_cap_spec
    htCap_W heat_trans coil ht_flux_C coil_SA pcm_SA tank ht_flux_P surface vol_ht_gen,
  eBalanceOnWtrDerivDesc2 dd1HtFluxC dd2HtFluxP,
  eBalanceOnWtrDerivDesc3 w_mass htCap_W,
  eBalanceOnWtrDerivDesc4 eq2,
  eBalanceOnWtrDerivDesc5,
  eBalanceOnWtrDerivDesc6 eq3 eq4,
  eBalanceOnWtrDerivDesc7 eq5]

eBalanceOnWtrDerivDesc1 :: ConceptChunk -> ConstrConcept -> UnitalChunk -> ConceptChunk -> 
  ConceptChunk -> UnitalChunk -> ConceptChunk -> UnitalChunk -> ConceptChunk -> UncertQ -> 
  ConceptChunk -> ConceptChunk -> UnitalChunk -> UncertQ -> UncertQ -> ConceptChunk ->
  UnitalChunk -> ConceptChunk -> UnitalChunk -> [Sentence]
eBalanceOnWtrDerivDesc1 roc tw en wt vo wvo ms wms hcs hw ht cl hfc cs ps tk hfp su vhg =
  [S "To find the", phrase roc `sOf` (E $ sy tw) `sC` S "we look at the",
   phrase en, S "balance on" +:+. phrase wt, S "The", phrase vo, S "being considered" 
   `isThe` (phrase vo `sOf` phrase wt), (E $ sy wvo) `sC` S "which has", 
   (phrase ms `sOf` phrase wt) +:+.
   ((E $ sy wms) `sAnd` (phrase hcs `sOf` phrase wt) `sC` (E $ sy hw)), 
    (E $ sy hfc), S "represents the", phrase ht, S "into the", phrase wt,
    S "from the", (phrase cl `sAnd` (E $ sy hfp)), S "represents the", phrase ht,
    S "into the PCM from", phrase wt `sC` S "over", phrase cl, phrase su,
    S "area and", phrase phase_change, S "material", phrase su, S "area of",
    (E $ sy cs) `sAnd` (E $ sy ps) `sC` S "respectively.",  S "No", phrase ht,
    S "occurs to", S "outside" `ofThe` phrase tk `sC` 
    S "since it has been assumed to be perfectly insulated", 
    sParen (makeRef2S newA15) :+: S ". Assuming no volumetric", 
    S "heat generation per unit", phrase vo +:+.
    (sParen (makeRef2S newA16) `sC` (E $ sy vhg $= 0)), S "Therefore, the equation for",
     makeRef2S rocTempSimp, S "can be written as"]

eBalanceOnWtrDerivDesc2 :: DataDefinition -> DataDefinition -> [Sentence]
eBalanceOnWtrDerivDesc2 dd1 dd2 =
  [S "Using", makeRef2S dd1, S "and", makeRef2S dd2, S "for", 
  (E $ sy dd1) `sAnd` (E $ sy dd2),
  S "respectively, this can be written as"]

eBalanceOnWtrDerivDesc3 ::  UnitalChunk -> UncertQ -> [Sentence]
eBalanceOnWtrDerivDesc3 wm hcw = 
  [S "Dividing (3) by", ch wm :+: ch hcw `sC` S "we obtain"]

eBalanceOnWtrDerivDesc4 :: [Sentence] -> [Sentence]
eBalanceOnWtrDerivDesc4 eq22 = [S "Factoring the negative sign out of the second term",
  S "of the RHS of Equation (4) and multiplying it by"] ++ eq22 ++ [S "yields"]

eBalanceOnWtrDerivDesc5 ::[Sentence]
eBalanceOnWtrDerivDesc5 = [S "Which simplifies to"]

eBalanceOnWtrDerivDesc6 :: Expr -> Expr -> [Sentence]
eBalanceOnWtrDerivDesc6 eq33 eq44 = 
  [S "Setting", (E eq33), ((sParen (makeRef2S dd3HtFusion)) `sAnd` (E eq44)),
  (sParen (makeRef2S dd4MeltFrac)) `sC` S "Equation (5) can be written as"]

eBalanceOnWtrDerivDesc7 :: Expr -> [Sentence]
eBalanceOnWtrDerivDesc7 eq55 = 
  [S "Finally, factoring out", (E eq55), S ", we are left with the governing ODE for",
  sParen (makeRef2S eBalanceOnWtr)]

eq2 :: [Sentence]
eq2 = [ch coil_HTC, ch coil_SA, S "/", ch coil_HTC, ch coil_SA]

eq3, eq4, eq5:: Expr
eq3 = (sy tau_W) $= ((sy w_mass) * (sy htCap_W)) / ((sy coil_HTC) * (sy coil_SA))
eq4 = (sy eta) $= ((sy pcm_HTC) * (sy pcm_SA)) / 
  ((sy coil_HTC) * (sy coil_SA))
eq5 = 1 / (sy tau_W)

eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3,
 eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7 :: Expr

eBalanceOnWtrDerivEqn1 = (sy w_mass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy ht_flux_C) * (sy coil_SA) - (sy ht_flux_P) * (sy pcm_SA)

eBalanceOnWtrDerivEqn2 = (sy w_mass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy coil_HTC) * (sy coil_SA) *  ((sy temp_C) - (sy temp_W)) -
  (sy pcm_HTC) * (sy pcm_SA) *  ((sy temp_W) - (sy temp_PCM))

eBalanceOnWtrDerivEqn3 = (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) -
  ((sy pcm_mass) * (sy pcm_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_W) - (sy temp_PCM))

eBalanceOnWtrDerivEqn4 = 
  (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) +
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy coil_HTC) * (sy coil_SA))) * ((sy pcm_HTC) * (sy pcm_SA) / 
  ((sy w_mass) * (sy htCap_W))) * ((sy temp_PCM) - (sy temp_W))

eBalanceOnWtrDerivEqn5 =  
  (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) +
  ((sy pcm_HTC) * (sy pcm_SA) / 
  ((sy coil_HTC) * (sy coil_SA))) * ((sy coil_HTC) * (sy coil_SA) / 
  ((sy w_mass) * (sy htCap_W))) * ((sy temp_PCM) - (sy temp_W))


eBalanceOnWtrDerivEqn6 = (deriv (sy temp_W) time) $= 
  1 / (sy tau_W) * ((sy temp_C) - (sy temp_W)) +
  (sy eta) / (sy tau_W) * ((sy temp_PCM) - (sy temp_W))

eBalanceOnWtrDerivEqn7 =  
  (deriv (sy temp_W) time) $= 1 / (sy tau_W) * (((sy temp_C) - (sy temp_W)) +
  sy eta * ((sy temp_PCM) - (sy temp_W)))

eBalanceOnWtr_deriv_eqns__im1 :: [Expr]
eBalanceOnWtr_deriv_eqns__im1 = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2,
 eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6,
 eBalanceOnWtrDerivEqn7]

---------
-- IM2 --
---------
eBalanceOnPCM :: InstanceModel
eBalanceOnPCM = deModel eBalanceOnPCM_rc [qw temp_melt_P, qw time_final, qw temp_init, qw pcm_SA,
 qw pcm_HTC, qw pcm_mass, qw htCap_S_P, qw htCap_L_P]
  [sy temp_init $< sy temp_melt_P] (qw temp_PCM)
   [0 $< sy time $< sy time_final] [koothoor2013] eBalanceOnPCMDeriv 
   "eBalanceOnPCM" [balPCMDesc_note]

eBalanceOnPCM_rc :: RelationConcept
eBalanceOnPCM_rc = makeRC "eBalanceOnPCM_rc" (nounPhraseSP
  "Energy Balance on PCM to Find T_p")
  --FIXME: T_p should be called from symbol
  balPCMDesc balPCM_Rel -- eBalanceOnPCML

balPCM_Rel :: Relation
balPCM_Rel = (deriv (sy temp_PCM) time) $= case_ [case1, case2, case3, case4]
  where case1 = ((1 / (sy tau_S_P)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpTo (Exc,sy temp_melt_P)))

        case2 = ((1 / (sy tau_L_P)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpFrom (Exc,sy temp_melt_P)))

        case3 = (0, (sy temp_PCM) $= (sy temp_melt_P))

        case4 = (0, real_interval melt_frac (Bounded (Exc,0) (Exc,1)))

balPCMDesc :: Sentence
balPCMDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ (sy tau_S_P) $= ((sy pcm_mass) * (sy htCap_S_P)) /
  ((sy pcm_HTC) * (sy pcm_SA))), S "is a constant" +:+.
  sParen (unwrap $ getUnit tau_S_P), (E $ (sy tau_L_P) $=
  ((sy pcm_mass) * (sy htCap_L_P)) / ((sy pcm_HTC) * (sy pcm_SA))),
  S "is a constant", sParen (unwrap $ getUnit tau_S_P)]

balPCMDesc_note :: Sentence
balPCMDesc_note = foldlSent [
  (E $ (sy temp_melt_P)) `sC` (E $ (sy time_final)) `sC` (E $ (sy temp_init)) `sC`
  (E $ (sy pcm_HTC)) `sC` (E $ (sy pcm_mass)) `sC` (E $ (sy htCap_S_P)) `sC`
  (E $ (sy htCap_S_P)), S "form" +:+. sParen (makeRef2S eBalanceOnWtr),
  S "The input is constrained so that", (E $ (sy temp_init $< sy temp_melt_P)),
  sParen (makeRef2S newA13),
  (E $ (sy temp_PCM)) `sC` (E $ (0 $< sy time $< sy time_final)) `sC`
  (S "with initial conditions")
  `sC` (E $ (sy temp_W $= sy temp_PCM $= sy temp_init)) `sC` (S "FIXME t_w(0) = t_p(0)") `sC` 
  makeRef2S newA12 `sC` (S "and"), (E $ (sy temp_W)), 
  S "from IM1, such that the following governing ODE is satisfied.", 
  S "The temperature remains constant at",
  (E $ (sy temp_melt_P)) `sC` 
  (S "even with the heating (or cool-ing), until the phase change has occurred for all of the material; that is as long as"),
  (E $ (0 $< sy melt_frac $< 1)), S "(from", makeRef2S dd4MeltFrac,
  S ") is determined as part of the heat energy in the PCM, as given in",
   sParen (makeRef2S heatEInPCM)]

 ----------------------------------------------
--    Derivation of eBalanceOnPCM          --
----------------------------------------------
eBalanceOnPCMDeriv :: Derivation
eBalanceOnPCMDeriv =
  [S "Detailed derivation of the" +:+ phrase energy +:+ S "balance on the PCM during " +:+ 
    S "sensible heating phase:" ] ++
  (weave [eBalanceOnPCMDerivSentences, map E eBalanceOnPCM_deriv_eqns__im2])
  ++ (eBalanceOnPCMDerivDesc5 htCap_S_P htCap_L_P tau_S_P tau_L_P surface area melting vol newA17)
  ++ (eBalanceOnPCMDerivDesc6 temp_PCM)
  ++ (eBalanceOnPCMDerivDesc7 boiling solid liquid newA18)

eBalanceOnPCMDerivSentences :: [Sentence]
eBalanceOnPCMDerivSentences = map foldlSentCol [
  eBalanceOnPCMDerivDesc1 rOfChng temp_PCM energy water vol pcm_vol pcm_mass heat_cap_spec
    htCap_S_P ht_flux ht_flux_P phase_change pcm_SA heat newA16 vol_ht_gen,
  eBalanceOnPCMDerivDesc2 dd2HtFluxP ht_flux_P,
  eBalanceOnPCMDerivDesc3 eq6,
  eBalanceOnPCMDerivDesc4 eq7
   ]

eBalanceOnPCMDerivDesc1 :: ConceptChunk -> ConstrConcept -> UnitalChunk -> ConceptChunk -> 
  ConceptChunk-> UncertQ -> UnitalChunk -> ConceptChunk -> UncertQ -> 
  ConceptChunk -> UnitalChunk -> ConceptChunk -> UncertQ ->  ConceptChunk ->
  AssumpChunk -> UnitalChunk -> [Sentence]
eBalanceOnPCMDerivDesc1 roc tempP en wt vo pcmvo pm hcs hsp hf hfp pc ps ht ass16 vhg=
  [S "To find the", phrase roc `sOf` (E $ sy tempP) `sC` S "we look at the",
   phrase en, S "balance on the" +:+. S "PCM", S "The", phrase vo, S "being considered" 
   `isThe` (phrase vo `sOf` S "PCM,") +:+. (E $ sy pcmvo), S "The derivation that follows is" +:+. 
   S "initially for the solid PCM", S "The mass of phase change material is", (E $ sy pm) `sAnd` S "the",
   phrase hcs `sOf` S "PCM as a solid is" +:+. (E $ sy hsp), S "The", phrase hf,
   S "into the PCM from", phrase wt, S "is", (E $ sy hfp), S "over", phrase pc, 
   S "material surface area" +:+. (E $ sy ps), S "There is no", phrase hf +:+. S "output",
   S "Assuming no volumetric", phrase ht, S "generation per unit", phrase vo,
   (sParen (makeRef2S ass16)) `sC` (E $ sy vhg $= 0), S ", the equation for",
   makeRef2S rocTempSimp, S "can be written as"]

eBalanceOnPCMDerivDesc2 :: DataDefinition -> UnitalChunk -> [Sentence]
eBalanceOnPCMDerivDesc2 dd2 hfp =
  [S "Using", makeRef2S dd2, S "for", (E $ sy hfp) `sC` 
  S "this equation can be written as"]

eBalanceOnPCMDerivDesc3 :: [Sentence] -> [Sentence]
eBalanceOnPCMDerivDesc3 eq66 = 
  [S "Dividing by"] ++ eq66 ++ [S "we obtain"]

eBalanceOnPCMDerivDesc4 :: [Sentence] -> [Sentence]
eBalanceOnPCMDerivDesc4 eq77 = 
  [S "Setting"] ++ eq77 ++ [S ", this can be written as"]

eBalanceOnPCMDerivDesc5 ::  UncertQ -> UncertQ -> UnitalChunk -> UnitalChunk -> ConceptChunk -> ConceptChunk-> ConceptChunk
  -> ConceptChunk -> AssumpChunk -> [Sentence]
eBalanceOnPCMDerivDesc5 hsp hlp tsp tlp sur ar melt vo ass17= 
  [S "Equation (6) applies for the solid PCM. In the case where all of the PCM is melted, the same" +:+
   S "derivation applies, except that" +:+ (E $ sy hsp) +:+ S "is replaced by" +:+ (E $ sy hlp) `sC`
   S "and thus" +:+ (E $ sy tsp) +:+ S "is replaced by" +:+. (E $ sy tlp) +:+ S "Although a small change in" +:+
   phrase sur +:+ phrase ar +:+ S "would be expected with" +:+ phrase melt `sC` S "this is not included" `sC`
   S "since the" +:+ phrase vo +:+ S "change of the PCM with" +:+ phrase melting +:+ S "is assumed to be negligible" +:+.
   (sParen (makeRef2S ass17))]

eBalanceOnPCMDerivDesc6 :: NamedIdea a => a -> [Sentence]
eBalanceOnPCMDerivDesc6 tp =
    [S "In the case where" +:+ (E eq6_1) +:+ S "and not all of the PCM is melted" `sC`
   S "the" +:+ phrase tp +:+. S "does not change" +:+ S "Therefore" `sC` 
   S "in this case" +:+ (foldlSent eq6_2)]

eBalanceOnPCMDerivDesc7 :: ConceptChunk -> ConceptChunk -> ConceptChunk -> AssumpChunk-> [Sentence]
eBalanceOnPCMDerivDesc7 boil sld lqd assp18 =
   [S "This derivation does not consider the" +:+
   phrase boil +:+ S "of the PCM" `sC` S "as the PCM is assumed to either be in a" +:+ phrase sld +:+ S "state or a" +:+
   phrase lqd +:+ S "state" +:+. (sParen (makeRef2S assp18))]
--(E $ ((deriv (sy temp_PCm) time) $= 0)
eq6:: [Sentence]
eq6 = [ch pcm_mass, ch htCap_S_P]

eq7:: [Sentence]
eq7 = [ch tau_S_P, S "=", ch pcm_mass, ch htCap_S_P, S "/", 
  ch pcm_HTC, ch pcm_SA]
eq6_1 :: Expr
eq6_1 = (sy temp_PCM) $= (sy temp_melt_P)
eq6_2 :: [Sentence]
eq6_2 = [S "d" +:+ ch temp_PCM +:+ S "/ d" +:+ ch time +:+ S "= 0"]


eBalanceOnPCM_Eqn1, eBalanceOnPCM_Eqn2, eBalanceOnPCM_Eqn3,
 eBalanceOnPCM_Eqn4 :: Expr

eBalanceOnPCM_Eqn1 = (sy pcm_mass) * (sy htCap_S_P) * (deriv (sy temp_PCM) time) $= 
  (sy ht_flux_P) * (sy pcm_SA)

eBalanceOnPCM_Eqn2 = (sy pcm_mass) * (sy htCap_S_P) * (deriv (sy temp_PCM) time) $= 
  (sy pcm_HTC) * (sy pcm_SA) *  ((sy temp_W) - (sy temp_PCM))

eBalanceOnPCM_Eqn3 = (deriv (sy temp_PCM) time) $= 
  (((sy pcm_HTC) * (sy pcm_SA)) / ((sy pcm_mass) * (sy htCap_S_P))) *  ((sy temp_W) - (sy temp_PCM))

eBalanceOnPCM_Eqn4 = 
  (deriv (sy temp_PCM) time) $= 
  (1 / sy tau_S_P) * ((sy temp_W) - (sy temp_PCM))

eBalanceOnPCM_deriv_eqns__im2 :: [Expr]
eBalanceOnPCM_deriv_eqns__im2 = [eBalanceOnPCM_Eqn1, eBalanceOnPCM_Eqn2,
 eBalanceOnPCM_Eqn3, eBalanceOnPCM_Eqn4]

---------
-- IM3 --
---------
-- FIXME: this is an 'other' model because we don't have Functional models yet
heatEInWtr :: InstanceModel
heatEInWtr = othModel heatEInWtr_rc [qw temp_init, qw w_mass, qw htCap_W, qw w_mass] 
  [] (qw w_E) [0 $< sy time $< sy time_final] [koothoor2013] [] "heatEInWtr"
  [htWtrDesc]

heatEInWtr_rc :: RelationConcept
heatEInWtr_rc = makeRC "heatEInWtr_rc" (nounPhraseSP "Heat energy in the water")
  htWtrDesc htWtr_Rel -- heatEInWtrL

htWtr_Rel :: Relation
htWtr_Rel = (apply1 w_E time) $= (sy htCap_W) * (sy w_mass) *
  ((apply1 temp_W time) - sy temp_init)

htWtrDesc :: Sentence
htWtrDesc = foldlSent [S "The above", phrase equation, S "is derived using" +:+. 
  makeRef2S sensHtE, ch w_E `isThe` phrase change, S "in", 
  phrase thermal_energy, S "of the", phrase liquid, phrase water, 
  S "relative to the", phrase energy, S "at the initial", phrase temp, 
  sParen (ch temp_init) +:+. sParen (unwrap $ getUnit pcm_initMltE), 
  (ch htCap_W) `isThe` phrase heat_cap_spec, S "of", phrase liquid, phrase water,
  sParen (unwrap $ getUnit htCap_S_P) `sAnd` (ch w_mass) `isThe` phrase mass, 
  S "of the", phrase water +:+. sParen (unwrap $ getUnit w_mass), S "The", 
  phrase change, S "in", phrase temp, S "is the difference between the", 
  phrase temp, S "at", phrase time, ch time, sParen (unwrap $ getUnit t_init_melt) `sC`
  (ch temp_W) `sAnd` S "the", phrase temp_init `sC` ch temp_init +:+.
  sParen (unwrap $ getUnit temp_init), S "This", phrase equation,
  S "applies as long as", (E $ real_interval temp_W (Bounded (Exc,0) (Exc,100)))
  :+: (unwrap $ getUnit temp_W), sParen $ makeRef2S newA14 `sC` makeRef2S newA19]

---------
-- IM4 --
---------
heatEInPCM :: InstanceModel
heatEInPCM = eqModel heatEInPCM_defn [qw temp_melt_P, qw time_final, qw temp_init, qw pcm_SA,
 qw pcm_HTC, qw pcm_mass, qw htCap_S_P, qw htCap_L_P, qw temp_PCM, qw htFusion, qw t_init_melt]
  [sy temp_init $< sy temp_melt_P] (qw pcm_E)
  [0 $< sy time $< sy time_final] [koothoor2013] []
  "heatEInPCM" [htPCMDesc]

heatEInPCM_defn :: QDefinition
heatEInPCM_defn = fromEqn' "heatEInPCM" (pcm_E ^. term) htPCMDesc (eqSymb pcm_E) htPCM

htPCM :: Expr
htPCM = case_ [case1, case2, case3, case4]
  where case1 = (sy htCap_S_P * sy pcm_mass * ((apply1 temp_PCM time) -
          sy temp_init), real_interval temp_PCM (UpTo (Exc, sy temp_melt_P)))

        case2 = (sy pcm_initMltE + (sy htFusion * sy pcm_mass) +
          (sy htCap_L_P * sy pcm_mass * ((apply1 temp_PCM time) -
          sy temp_melt_P)), real_interval temp_PCM (UpFrom (Exc, sy temp_melt_P)))

        case3 = (sy pcm_initMltE + (apply1 latentE_P time),
          (sy temp_PCM) $= (sy temp_melt_P))

        case4 = (sy pcm_initMltE + (apply1 latentE_P time),
          real_interval melt_frac (Bounded (Exc,0) (Exc,1)))

htPCMDesc :: Sentence
htPCMDesc = foldlSent [S "The above", phrase equation,S "is derived using" +:+.
  (makeRef2S sensHtE `sAnd` makeRef2S latentHtE), ch pcm_E `isThe` phrase change,
  S "in", phrase thermal_energy, S "of the", short phsChgMtrl, S "relative to the",
  phrase energy, S "at the", phrase temp_init, sParen (ch temp_init) +:+.
  (unwrap $ getUnit pcm_initMltE), ch pcm_E, S "for the", phrase solid,
  short phsChgMtrl, S "is found using", makeRef2S sensHtE, S "for", phrase sens_heat,
  S "ing, with", phrase heat_cap_spec `ofThe` phrase solid, short phsChgMtrl `sC`
  ch htCap_S_P, sParen (unwrap $ getUnit htCap_S_P), S "and the", phrase change, S "in the",
  short phsChgMtrl, phrase temp, S "from the", phrase temp_init +:+.
  sParen (unwrap $ getUnit temp_init), ch pcm_E, S "for the melted", short phsChgMtrl,
  sParen (E (sy temp_PCM $> sy pcm_initMltE)), S "is found using", makeRef2S sensHtE,
  S "for", phrase sens_heat, S "of the" +:+. phrase liquid, short phsChgMtrl,
  S "plus the", phrase energy, S "when", phrase melting, S "starts, plus the", phrase energy,
  S "required to melt all of the", short phsChgMtrl, S "The", phrase energy, S "when",
  phrase melting, S "starts is", ch pcm_initMltE +:+. sParen (unwrap $ getUnit pcm_initMltE),
  S "The", phrase energy, S "required to melt all of the", short phsChgMtrl, S "is",
  E (sy htFusion * sy pcm_mass), sParen (unwrap $ getUnit pcm_initMltE) +:+.
  sParen (makeRef2S dd3HtFusion), phrase heat_cap_spec `ofThe` phrase liquid, short phsChgMtrl,
  S "is", ch htCap_L_P, sParen (unwrap $ getUnit htCap_L_P) `sAnd` S "the", phrase change,
  S "in", phrase temp, S "is", E (sy temp_PCM - sy temp_melt_P) +:+.
  sParen (unwrap $ getUnit temp_melt_P), ch pcm_E, S "during", phrase melting, S "of the",
  short phsChgMtrl, S "is found using the", phrase energy, S "required at", S "instant" +:+
  phrase melting `ofThe` short phsChgMtrl, S "begins" `sC` ch pcm_initMltE, S "plus the",
  phrase latent_heat, phrase energy, S "added to the", short phsChgMtrl `sC`
  ch latentE_P, sParen (unwrap $ getUnit latentE_P), S "since the", phrase time, S "when",
  phrase melting, S "began", ch t_init_melt +:+. sParen (unwrap $ getUnit t_init_melt),
  S "The", phrase heat, phrase energy, S "for", phrase boiling, S "of the", short phsChgMtrl,
  S "is not detailed" `sC` S "since the", short phsChgMtrl, S "is assumed to either be in a", 
  phrase solid, S "or", phrase liquid, S "state", sParen (makeRef2S newA18),
  sParen (makeRef2S newA13)]
