module Drasil.SWHS.IMods (iMods, eBalanceOnWtr, eBalanceOnWtrDerivDesc1,
  eBalanceOnPCM, heatEInWtr, heatEInPCM, instModIntro) where

import Language.Drasil
import Theory.Drasil (DataDefinition, InstanceModel, im, imNoDeriv)
import Utils.Drasil

import Data.Drasil.SentenceStructures (follows)
import Data.Drasil.Utils (unwrap, weave)
import Data.Drasil.Concepts.Documentation (assumption, condition, constraint,
  goal, input_, solution, term_, output_)
import Data.Drasil.Concepts.Math (area, change, equation, ode, rOfChng, surface)
import Data.Drasil.Concepts.PhysicalProperties (liquid, mass, solid, vol)
import Data.Drasil.Concepts.Thermodynamics (boilPt, boiling, heat, heatCapSpec, 
  heatTrans, htFlux, latentHeat, melting, phaseChange, sensHeat, temp)
import Data.Drasil.Quantities.Physics (energy, time)

import Drasil.SWHS.Assumptions (assumpCTNOD, assumpSITWP, assumpPIS, assumpWAL,
  assumpPIT, assumpNIHGBWP, assumpVCMPN, assumpNGSP, assumpAPT, assumpTHCCoL,
  assumpCWTAT, assumpTPCAV)
import Drasil.SWHS.Concepts (coil, phsChgMtrl, tank, water)
import Drasil.SWHS.DataDefs (dd1HtFluxC, dd2HtFluxP, dd3HtFusion, dd4MeltFrac,
  ddBalanceSolidPCM, ddBalanceLiquidPCM)
import Drasil.SWHS.Goals (waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.TMods (sensHtE, latentHtE)
import Drasil.SWHS.Unitals (coil_HTC, coil_SA, eta, ht_flux_C, ht_flux_P, htCap_L_P, 
  htCap_S_P, htCap_W, htFusion, latentE_P, melt_frac, pcm_E, pcm_HTC, pcm_initMltE, 
  pcm_mass, pcm_SA, pcm_vol, t_init_melt, tau_L_P, tau_S_P, tau_W, temp_C, temp_init, 
  temp_melt_P, temp_PCM, temp_W, time_final, vol_ht_gen, w_E, w_mass, w_vol) 
import Drasil.SWHS.GenDefs (rocTempSimp)

iMods :: [InstanceModel]
iMods = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

---------
-- IM1 --
---------
eBalanceOnWtr :: InstanceModel
eBalanceOnWtr = im eBalanceOnWtr_rc [qw w_mass, qw htCap_W, qw coil_HTC, qw pcm_SA,
 qw pcm_HTC, qw coil_SA, qw temp_PCM, qw time_final, qw temp_C, qw temp_init]
  [sy temp_init $<= sy temp_C] (qw temp_W)
   [0 $<= sy time $<= sy time_final] [makeCite koothoor2013] eBalanceOnWtrDeriv
   "eBalanceOnWtr" balWtrDesc

eBalanceOnWtr_rc :: RelationConcept
eBalanceOnWtr_rc = makeRC "eBalanceOnWtr_rc" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") EmptyS balWtr_Rel 
  -- eBalanceOnWtrL

balWtr_Rel :: Relation
balWtr_Rel = (deriv (sy temp_W) time) $= 1 / (sy tau_W) *
  (((sy temp_C) - (apply1 temp_W time)) +
  (sy eta) * ((apply1 temp_PCM time) - (apply1 temp_W time)))

balWtrDesc :: [Sentence]
balWtrDesc = map foldlSent [[E (sy temp_PCM) `sIs` S "defined by", makeRef2S eBalanceOnPCM],
  [S "The", phrase input_, phrase constraint, E $ sy temp_init $<= sy temp_C,
   S "comes from", makeRef2S assumpCTNOD],
  [E (sy tau_W) `sIs` S "calculated from", S "FIXME: Missing DD Issue 1484"],
  [E (sy eta) `sIs` S "calculated from", S "FIXME: Missing DD Issue 1484"],
  [S "The initial", plural condition, S "for the", getAcc ode `sAre` 
   E ((apply1Int temp_W 0) $= (apply1Int temp_PCM 0) $= sy temp_init) `follows` assumpSITWP],
  [S "The", getAcc ode, S "applies as long as the", phrase water `sIs` EmptyS `sIn`
  phrase liquid, S "form" `sC` (E $ real_interval temp_W (Bounded (Exc,0) (Exc,100))),
  sParen (unwrap $ getUnit temp_W), S "where", E 0, sParen (unwrap $ getUnit temp_W) `sAnd`
  (E 100), sParen (unwrap $ getUnit temp_W) `sAre` S "the", phrase melting `sAnd`
  plural boilPt `sOf` phrase water `sC` S "respectively",
  sParen (S "from" +:+ makeRef2S assumpWAL `sAnd` makeRef2S assumpAPT)]]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
-- type Derivation = [Sentence]
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv =
  S "Derivation" `sOf` S "the" +:+ phrase energy +:+ S "balance on water:" :
  weave [eBalanceOnWtrDerivSentences, map E eBalanceOnWtr_deriv_eqns__im1]

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = map foldlSentCol [
  eBalanceOnWtrDerivDesc1 htTransEnd overAreas extraAssumps assumpNIHGBWP,
  eBalanceOnWtrDerivDesc2 dd1HtFluxC dd2HtFluxP,
  eBalanceOnWtrDerivDesc3 w_mass htCap_W,
  eBalanceOnWtrDerivDesc4 eq2,
  eBalanceOnWtrDerivDesc5,
  eBalanceOnWtrDerivDesc6 eq3 eq4,
  eBalanceOnWtrDerivDesc7 eq5]

eBalanceOnWtrDerivDesc1 :: Sentence -> Sentence-> Sentence -> ConceptInstance -> [Sentence]
eBalanceOnWtrDerivDesc1 htEnd oa ea htA = [S "To find the", phrase rOfChng `sOf` (E $ sy temp_W) `sC`
  S "we look at the", phrase energy, S "balance on" +:+. phrase water, S "The",
  phrase vol, S "being considered" `isThe` (phrase vol `sOf` phrase water) `sIn` S "the",
  phrase tank, (E $ sy w_vol) `sC` S "which has", phrase mass +:+. ((E $ sy w_mass) `sAnd`
  phrase heatCapSpec `sC` (E $ sy htCap_W)), at_start heatTrans, S "occurs in the",
  phrase water, S "from the", phrase coil, S "as", (E $ sy ht_flux_C),
  sParen (makeRef2S dd1HtFluxC) :+: htEnd `sC` EmptyS +:+. oa, ea, S "No",
  phrase heatTrans, S "occurs to", S "outside" `ofThe` phrase tank `sC`
  S "since it has been assumed to be perfectly insulated" +:+. sParen (makeRef2S assumpPIT),
  S "Since the", phrase assumption `sIs` S "made that no internal heat" `sIs`
  S "generated" +:+. (sParen (makeRef2S htA) `sC` (E $ sy vol_ht_gen $= 0)),
  S "Therefore" `sC` S "the", phrase equation, S "for", makeRef2S rocTempSimp, S "can be written as"]

htTransEnd :: Sentence
htTransEnd = foldlSent_ [S " " `sAnd` S "from the", phrase water, S "into the",
  getAcc phsChgMtrl, S "as", (E $ sy ht_flux_P), sParen (makeRef2S dd2HtFluxP)]

overAreas :: Sentence
overAreas = S "over areas" +:+ ((E $ sy coil_SA) `sAnd` (E $ sy pcm_SA) `sC` S "respectively")

extraAssumps :: Sentence
extraAssumps = foldlSent [S "The thermal flux" `sIs` S "constant over", (E $ sy coil_SA) `sC`
  S "since", phrase temp `ofThe` phrase coil `sIs` S "assumed to not vary along its length",
  sParen (makeRef2S assumpTHCCoL) `sC` EmptyS `andThe` S "thermal flux" `sIs` S "constant over",
  (E $ sy pcm_SA) `sC` S "since", phrase temp `ofThe` getAcc phsChgMtrl `isThe`
  S "same throughout its", phrase vol, sParen (makeRef2S assumpTPCAV) `andThe`
  phrase water `sIs` S "fully mixed" +:+ sParen (makeRef2S assumpCWTAT)]

eBalanceOnWtrDerivDesc2 :: DataDefinition -> DataDefinition -> [Sentence]
eBalanceOnWtrDerivDesc2 dd1 dd2 = [S "Using", makeRef2S dd1 `sAnd` makeRef2S dd2,
  S "for",  (E $ sy dd1) `sAnd` (E $ sy dd2), S "respectively" `sC` S "this can be written as"]

eBalanceOnWtrDerivDesc3 ::  UnitalChunk -> UncertQ -> [Sentence]
eBalanceOnWtrDerivDesc3 wm hcw = 
  [S "Dividing (3) by", ch wm :+: ch hcw `sC` S "we obtain"]

eBalanceOnWtrDerivDesc4 :: [Sentence] -> [Sentence]
eBalanceOnWtrDerivDesc4 eq22 = [S "Factoring", S "negative sign out" `ofThe` S "second", phrase term_,
  S "of the RHS of Equation (4) and multiplying it by"] ++ eq22 ++ [S "yields"]

eBalanceOnWtrDerivDesc5 ::[Sentence]
eBalanceOnWtrDerivDesc5 = [S "Which simplifies to"]

eBalanceOnWtrDerivDesc6 :: Expr -> Expr -> [Sentence]
eBalanceOnWtrDerivDesc6 eq33 eq44 = 
  [S "Setting", (E eq33), ((sParen (makeRef2S dd3HtFusion)) `sAnd` (E eq44)),
  (sParen (makeRef2S dd4MeltFrac)) `sC` S "Equation (5) can be written as"]

eBalanceOnWtrDerivDesc7 :: Expr -> [Sentence]
eBalanceOnWtrDerivDesc7 eq55 = 
  [S "Finally" `sC` S "factoring out", E eq55, S ", we are left with the governing",
  getAcc ode, S "for", sParen (makeRef2S eBalanceOnWtr)]

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
  ((sy pcm_HTC) * (sy pcm_SA) / 
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
eBalanceOnPCM = im eBalanceOnPCM_rc [qw temp_melt_P, qw time_final, qw temp_init, qw pcm_SA,
 qw pcm_HTC, qw pcm_mass, qw htCap_S_P, qw htCap_L_P]
  [sy temp_init $< sy temp_melt_P] (qw temp_PCM)
   [0 $<= sy time $<= sy time_final] [makeCite koothoor2013] eBalanceOnPCMDeriv 
   "eBalanceOnPCM" balPCMNotes

eBalanceOnPCM_rc :: RelationConcept
eBalanceOnPCM_rc = makeRC "eBalanceOnPCM_rc" (nounPhraseSP
  "Energy Balance on PCM to find temperature of PCM")
  balPCMDesc balPCM_Rel -- eBalanceOnPCML

balPCM_Rel :: Relation
balPCM_Rel = (deriv (sy temp_PCM) time) $= case_ [case1, case2, case3]
  where case1 = ((1 / (sy tau_S_P)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpTo (Exc,sy temp_melt_P)))

        case2 = ((1 / (sy tau_L_P)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpFrom (Exc,sy temp_melt_P)))

        case3 = (0, (sy temp_PCM) $= (sy temp_melt_P) $&& real_interval melt_frac (Bounded (Exc,0) (Exc,1)))

balPCMDesc :: Sentence
balPCMDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ (sy tau_S_P) $= ((sy pcm_mass) * (sy htCap_S_P)) /
  ((sy pcm_HTC) * (sy pcm_SA))), S "is a constant",
  sParen (unwrap $ getUnit tau_S_P) +:+.
  sParen (makeRef2S ddBalanceSolidPCM),
  (E $ (sy tau_L_P) $= ((sy pcm_mass) * (sy htCap_L_P)) /
  ((sy pcm_HTC) * (sy pcm_SA))), S "is a constant",
  sParen (unwrap $ getUnit tau_S_P),
  sParen (makeRef2S ddBalanceLiquidPCM)]

balPCMNotes :: [Sentence]
balPCMNotes = map foldlSent [
  [E (sy temp_W) `sIs` S "defined by", makeRef2S eBalanceOnWtr],
  [S "The", phrase input_, phrase constraint, E $ sy temp_init $<= sy temp_melt_P,
   S "comes from", makeRef2S assumpPIS],
  [S "The", phrase temp, S "remains constant at", (E $ sy temp_melt_P) `sC`
   S "even with the heating", sParen (S "or cooling") `sC` S "until the",
   phrase phaseChange, S "has occurred for all" `sOf` S "the material; that" `sIs`
   S "as long as" +:+. E (0 $< sy melt_frac $< 1), E $ sy melt_frac,
   sParen (S "from" +:+ makeRef2S dd4MeltFrac) `sIs`
   S "determined as part" `sOf` S "the", phrase heat, phrase energy `sIn`
   S "the", getAcc phsChgMtrl `sC` S "as given" `sIn` sParen (makeRef2S heatEInPCM)],
  [E (sy tau_S_P) `sIs` S "calculated" `sIn` makeRef2S ddBalanceSolidPCM],
  [E (sy tau_L_P) `sIs` S "calculated" `sIn` makeRef2S ddBalanceLiquidPCM],
  [S "The initial", plural condition, S "for the", getAcc ode `sAre` 
   E ((apply1Int temp_W 0) $= (apply1Int temp_PCM 0) $= sy temp_init) `follows` assumpSITWP]]

 ----------------------------------------------
--    Derivation of eBalanceOnPCM          --
----------------------------------------------
eBalanceOnPCMDeriv :: Derivation
eBalanceOnPCMDeriv = foldlSentCol [S "Detailed derivation of the", phrase energy,
  S "balance on the PCM during sensible heating phase"] :
  (weave [eBalanceOnPCMDerivSentences, map E eBalanceOnPCM_deriv_eqns__im2])
  ++ (eBalanceOnPCMDerivDesc5 htCap_S_P htCap_L_P tau_S_P tau_L_P surface area melting vol assumpVCMPN)
  ++ (eBalanceOnPCMDerivDesc6 temp_PCM)
  ++ (eBalanceOnPCMDerivDesc7 boiling solid liquid assumpNGSP)

eBalanceOnPCMDerivSentences :: [Sentence]
eBalanceOnPCMDerivSentences = map foldlSentCol [
  eBalanceOnPCMDerivDesc1 rOfChng temp_PCM energy water vol pcm_vol pcm_mass heatCapSpec
  htCap_S_P htFlux ht_flux_P phaseChange pcm_SA heat assumpNIHGBWP vol_ht_gen,
  eBalanceOnPCMDerivDesc2 dd2HtFluxP ht_flux_P,
  eBalanceOnPCMDerivDesc3 eq6,
  eBalanceOnPCMDerivDesc4 eq7
  ]

eBalanceOnPCMDerivDesc1 :: ConceptChunk -> ConstrConcept -> UnitalChunk -> ConceptChunk -> 
  ConceptChunk-> UncertQ -> UnitalChunk -> ConceptChunk -> UncertQ -> 
  ConceptChunk -> UnitalChunk -> ConceptChunk -> UncertQ ->  ConceptChunk ->
  ConceptInstance -> UnitalChunk -> [Sentence]
eBalanceOnPCMDerivDesc1 roc tempP en wt vo pcmvo pm hcs hsp hf hfp pc ps ht ass16 vhg=
  [S "To find the", phrase roc `sOf` (E $ sy tempP) `sC` S "we look at the",
   phrase en, S "balance on the" +:+. S "PCM", S "The", phrase vo, S "being considered" 
   `isThe` (phrase vo `sOf` S "PCM,") +:+. (E $ sy pcmvo), S "The derivation that follows is" +:+. 
   S "initially for the solid PCM", S "The mass of phase change material is", (E $ sy pm) `andThe`
   phrase hcs `sOf` S "PCM as a solid is" +:+. (E $ sy hsp), S "The", phrase hf,
   S "into the PCM from", phrase wt `sIs` (E $ sy hfp), sParen (makeRef2S dd2HtFluxP),
   S "over", phrase pc, S "material surface area" +:+. (E $ sy ps),
   S "The thermal flux" `sIs` S "constant over", (E $ sy pcm_SA) `sC` S "since",
   phrase temp `ofThe` getAcc phsChgMtrl `isThe` S "same throughout its", phrase vol,
   sParen (makeRef2S assumpTPCAV) `andThe` phrase water `sIs` S "fully mixed" +:+.
   sParen (makeRef2S assumpCWTAT), S "There is no", phrase hf, phrase output_,
   S "from the" +:+. getAcc phsChgMtrl, S "Assuming no volumetric", phrase ht,
   S "generation per unit", phrase vo, sParen (makeRef2S ass16) `sC` (E $ sy vhg $= 0) `sC`
   S "the equation for", makeRef2S rocTempSimp, S "can be written as"]

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
  -> ConceptChunk -> ConceptInstance -> [Sentence]
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

eBalanceOnPCMDerivDesc7 :: ConceptChunk -> ConceptChunk -> ConceptChunk -> ConceptInstance -> [Sentence]
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
heatEInWtr :: InstanceModel
heatEInWtr = im heatEInWtr_rc [qw temp_init, qw w_mass, qw htCap_W, qw w_mass] 
  [] (qw w_E) [0 $<= sy time $<= sy time_final] [makeCite koothoor2013] [] "heatEInWtr"
  htWtrNotes

heatEInWtr_rc :: RelationConcept
heatEInWtr_rc = makeRC "heatEInWtr_rc" (nounPhraseSP "Heat energy in the water")
  EmptyS htWtr_Rel -- heatEInWtrL

htWtr_Rel :: Relation
htWtr_Rel = (apply1 w_E time) $= (sy htCap_W) * (sy w_mass) *
  ((apply1 temp_W time) - sy temp_init)

htWtrNotes :: [Sentence]
htWtrNotes = map foldlSent [
  [S "The above", phrase equation, S "is derived using", makeRef2S sensHtE],
  [S "The", phrase change `sIn` phrase temp `isThe` S "difference between the", 
   phrase temp, S "at", phrase time, ch time, sParen (unwrap $ getUnit t_init_melt) `sC`
  (ch temp_W) `andThe` phrase temp_init `sC` ch temp_init, sParen (unwrap $ getUnit temp_init)],
  [S "This", phrase equation, S "applies as long as",
  (E $ real_interval temp_W (Bounded (Exc,0) (Exc,100))) :+:
  unwrap (getUnit temp_W), sParen $ makeRef2S assumpWAL `sC` makeRef2S assumpAPT]]

---------
-- IM4 --
---------
heatEInPCM :: InstanceModel
heatEInPCM = imNoDeriv heatEInPCM_rc [qw temp_melt_P, qw time_final, qw temp_init, qw pcm_SA,
 qw pcm_HTC, qw pcm_mass, qw htCap_S_P, qw htCap_L_P, qw temp_PCM, qw htFusion, qw t_init_melt]
  [sy temp_init $< sy temp_melt_P] (qw pcm_E)
  [0 $<= sy time $<= sy time_final] [makeCite koothoor2013]
  "heatEInPCM" htPCMNotes

heatEInPCM_rc :: RelationConcept
heatEInPCM_rc = makeRC "heatEInPCM_rc" (nounPhraseSP "Heat energy in the PCM")
  EmptyS htPCM_Rel

htPCM_Rel :: Relation
htPCM_Rel = sy pcm_E $= case_ [case1, case2, case3, case4]
  where case1 = (sy htCap_S_P * sy pcm_mass * ((apply1 temp_PCM time) -
          sy temp_init), real_interval temp_PCM (UpTo (Exc, sy temp_melt_P)))

        case2 = (sy pcm_initMltE + (sy htFusion * sy pcm_mass) +
          (sy htCap_L_P * sy pcm_mass * ((apply1 temp_PCM time) -
          sy temp_melt_P)), real_interval temp_PCM (UpFrom (Exc, sy temp_melt_P)))

        case3 = (sy pcm_initMltE + (apply1 latentE_P time),
          (sy temp_PCM) $= (sy temp_melt_P))

        case4 = (sy pcm_initMltE + (apply1 latentE_P time),
          real_interval melt_frac (Bounded (Exc,0) (Exc,1)))

htPCMNotes :: [Sentence]
htPCMNotes = map foldlSent [
  [S "The above", phrase equation `sIs` S "derived using",
  (makeRef2S sensHtE `sAnd` makeRef2S latentHtE)],
  [ch pcm_E, S "for the", phrase solid, short phsChgMtrl, S "is found using",
   makeRef2S sensHtE, S "for", phrase sensHeat :+: S "ing, with",
   phrase heatCapSpec `ofThe` phrase solid, short phsChgMtrl `sC` ch htCap_S_P,
   sParen (unwrap $ getUnit htCap_S_P) `andThe` phrase change, S "in the",
   short phsChgMtrl, phrase temp, S "from the", phrase temp_init, sParen (unwrap $ getUnit temp_init)],
  [ch pcm_E, S "for the melted", short phsChgMtrl, sParen (E (sy temp_PCM $> sy pcm_initMltE)),
   S "is found using", makeRef2S sensHtE, S "for", phrase sensHeat, S "of the", phrase liquid,
   short phsChgMtrl, S "plus the", phrase energy, S "when", phrase melting, S "starts" `sC`
   S "plus", (phrase energy +:+ S "required to melt all") `ofThe` short phsChgMtrl], 
  [S "The", phrase energy, S "required to melt all of the", short phsChgMtrl `sIs`
   E (sy htFusion * sy pcm_mass), sParen (unwrap $ getUnit pcm_initMltE),
   sParen (S "from" +:+ makeRef2S dd3HtFusion)],
  [S "The", phrase change `sIn` phrase temp `sIs` E (sy temp_PCM - sy temp_melt_P),
   sParen (unwrap $ getUnit temp_melt_P)],
  [ch pcm_E, S "during", phrase melting, S "of the", short phsChgMtrl,
   S "is found using the", phrase energy, S "required at", S "instant" +:+
   phrase melting `ofThe` short phsChgMtrl, S "begins" `sC` ch pcm_initMltE, S "plus the",
   phrase latentHeat, phrase energy, S "added" `toThe` short phsChgMtrl `sC`
   ch latentE_P, sParen (unwrap $ getUnit latentE_P), S "since the", phrase time, S "when",
   phrase melting, S "began", ch t_init_melt, sParen (unwrap $ getUnit t_init_melt)],
  [S "The", phrase heat, phrase energy, S "for", phrase boiling, S "of the", short phsChgMtrl,
   S "is not detailed" `sC` S "since the", short phsChgMtrl, S "is assumed to either be in a", 
   phrase solid `sOr` phrase liquid, S "state", sParen (makeRef2S assumpNGSP),
   sParen (makeRef2S assumpPIS)]]

-----------
-- Intro --
-----------

instModIntro :: Sentence
instModIntro = S "The" +:+ plural goal +:+ makeRef2S waterTempGS `sC` 
  makeRef2S pcmTempGS `sC` makeRef2S waterEnergyGS `sC` S "and" +:+ 
  makeRef2S pcmEnergyGS +:+ S "are solved by" +:+ makeRef2S eBalanceOnWtr `sC`
  makeRef2S eBalanceOnPCM `sC` makeRef2S heatEInWtr `sC` S "and" +:+.
  makeRef2S heatEInPCM +:+ S "The" +:+ plural solution +:+ S "for" +:+
  makeRef2S eBalanceOnWtr `sAnd` makeRef2S eBalanceOnPCM +:+ 
  S "are coupled since the" +:+ plural solution +:+ S "for" +:+ ch temp_W `sAnd`
  ch temp_PCM +:+. S "depend on one another" +:+ makeRef2S heatEInWtr +:+
  S "can be solved once" +:+ makeRef2S eBalanceOnWtr +:+. 
  S "has been solved" +:+ S "The" +:+ plural solution `sOf` 
  makeRef2S eBalanceOnPCM `sAnd` makeRef2S heatEInPCM +:+ 
  S "are also coupled" `sC` S "since the" +:+ phrase temp_PCM `andThe` 
  phrase pcm_E +:+ S "depend on the" +:+. phrase phaseChange
