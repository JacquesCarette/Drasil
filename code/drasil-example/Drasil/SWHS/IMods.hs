module Drasil.SWHS.IMods (iMods, eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM, instModIntro) where

import Language.Drasil
import Theory.Drasil (DataDefinition, InstanceModel, im, imNoDeriv)
import Utils.Drasil

import Data.Drasil.Utils (unwrap, weave)
import Data.Drasil.SentenceStructures (foldlSent, foldlSentCol)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Concepts.Documentation (goal, solution)
import Data.Drasil.Concepts.Math (area, change, equation, rOfChng, surface)
import Data.Drasil.Concepts.PhysicalProperties (liquid, mass, solid, vol)
import Data.Drasil.Concepts.Thermodynamics (boilPt, boiling, heat, heatCapSpec, 
  heatTrans, htFlux, latentHeat, melting, phaseChange, sensHeat, temp,
  thermalEnergy)

import Drasil.SWHS.Assumptions (assumpCTNOD, assumpSITWP, assumpPIS, assumpWAL,
  assumpPIT, assumpNIHGBWP, assumpVCMPN, assumpNGSP, assumpAPT)
import Drasil.SWHS.Concepts (coil, phsChgMtrl, tank, water)
import Drasil.SWHS.DataDefs (dd1HtFluxC, dd2HtFluxP, dd3HtFusion, dd4MeltFrac,
  ddBalanceSolidPCM, ddBalanceLiquidPCM)
import Drasil.SWHS.Goals (waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.TMods (sensHtE, latentHtE)
import Drasil.SWHS.Unitals (coil_HTC, coil_SA, eta, htFluxC, htFluxP, htCapLP, 
  htCapSP, htCap_W, htFusion, latentEP, meltFrac, pcm_E, pcm_HTC, pcmInitMltE, 
  pcmMass, pcmSA, pcmVol, tInitMelt, tauLP, tauSP, tauW, temp_C, tempInit, 
  tempMeltP, temp_PCM, temp_W, time_final, volHtGen, w_E, wMass, wVol) 
import Drasil.SWHS.GenDefs (rocTempSimp)

iMods :: [InstanceModel]
iMods = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

---------
-- IM1 --
---------
eBalanceOnWtr :: InstanceModel
eBalanceOnWtr = im eBalanceOnWtrRC [qw wMass, qw htCap_W, qw coil_HTC, qw pcmSA,
 qw pcm_HTC, qw coil_SA, qw temp_PCM, qw time_final, qw temp_C, qw tempInit]
  [sy tempInit $< sy temp_C] (qw temp_W)
   [0 $< sy time $< sy time_final] [makeCite koothoor2013] eBalanceOnWtrDeriv
   "eBalanceOnWtr" [balWtrDesc]

eBalanceOnWtrRC :: RelationConcept
eBalanceOnWtrRC = makeRC "eBalanceOnWtrRC" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtrRel 
  -- eBalanceOnWtrL

balWtrRel :: Relation
balWtrRel = (deriv (sy temp_W) time) $= 1 / (sy tauW) *
  (((sy temp_C) - (apply1 temp_W time)) +
  (sy eta) * ((apply1 temp_PCM time) - (apply1 temp_W time)))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ sy tauW) `sC` (E $ sy time_final)
  `sC` (E $ sy temp_C) `sC` (E $ sy temp_PCM), S "from" +:+.
  sParen (makeRef2S eBalanceOnPCM), S "The input is constrained so that" +:+.
  (E $ sy tempInit $<= (sy temp_C)), sParen (makeRef2S assumpCTNOD),
  (E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ sy temp_C) `isThe` phrase temp_C +:+. sParen (unwrap $ getUnit temp_C),
  (E $ sy tauW $= (sy wMass * sy htCap_W) / (sy coil_HTC * sy coil_SA)),
  S "is a constant", sParen (makeRef2S dd3HtFusion) +:+. sParen (unwrap $ getUnit tauW),
  (E $ sy eta $= (sy pcm_HTC * sy pcmSA) / (sy coil_HTC * sy coil_SA)),
  S "is a constant" +:+. sParen (S "dimensionless"),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ real_interval temp_W (Bounded (Exc,0) (Exc,100))),
  sParen (unwrap $ getUnit temp_W), S "where", E 0,
  sParen (unwrap $ getUnit temp_W) `sAnd` (E 100),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boilPt, S "of", phrase water `sC` S "respectively",
  sParen (makeRef2S assumpWAL `sC` makeRef2S assumpAPT)]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
-- type Derivation = [Sentence]
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv =
  S "Derivation of the" +:+ phrase energy +:+ S "balance on water:" :
  weave [eBalanceOnWtrDerivSentences, map E eBalanceOnWtr_deriv_eqns__im1]

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = map foldlSentCol [
  eBalanceOnWtrDerivDesc1 rOfChng temp_W energy water vol wVol mass wMass heatCapSpec
    htCap_W heatTrans coil htFluxC coil_SA pcmSA tank htFluxP surface volHtGen,
  eBalanceOnWtrDerivDesc2 dd1HtFluxC dd2HtFluxP,
  eBalanceOnWtrDerivDesc3 wMass htCap_W,
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
    S "area and", phrase phaseChange, S "material", phrase su, S "area of",
    (E $ sy cs) `sAnd` (E $ sy ps) `sC` S "respectively.",  S "No", phrase ht,
    S "occurs to", S "outside" `ofThe` phrase tk `sC`
    S "since it has been assumed to be perfectly insulated",
    sParen (makeRef2S assumpPIT) :+: S ". Assuming no volumetric",
    S "heat generation per unit", phrase vo +:+.
    (sParen (makeRef2S assumpNIHGBWP) `sC` (E $ sy vhg $= 0)), S "Therefore, the equation for",
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
eq3 = (sy tauW) $= ((sy wMass) * (sy htCap_W)) / ((sy coil_HTC) * (sy coil_SA))
eq4 = (sy eta) $= ((sy pcm_HTC) * (sy pcmSA)) / 
  ((sy coil_HTC) * (sy coil_SA))
eq5 = 1 / (sy tauW)

eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3,
 eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7 :: Expr

eBalanceOnWtrDerivEqn1 = (sy wMass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy htFluxC) * (sy coil_SA) - (sy htFluxP) * (sy pcmSA)

eBalanceOnWtrDerivEqn2 = (sy wMass) * (sy htCap_W) * (deriv (sy temp_W) time) $= 
  (sy coil_HTC) * (sy coil_SA) *  ((sy temp_C) - (sy temp_W)) -
  (sy pcm_HTC) * (sy pcmSA) *  ((sy temp_W) - (sy temp_PCM))

eBalanceOnWtrDerivEqn3 = (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy wMass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) -
  ((sy pcmMass) * (sy pcmSA) / 
  ((sy wMass) * (sy htCap_W))) *  ((sy temp_W) - (sy temp_PCM))

eBalanceOnWtrDerivEqn4 = 
  (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy wMass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) +
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy coil_HTC) * (sy coil_SA))) * ((sy pcm_HTC) * (sy pcmSA) / 
  ((sy wMass) * (sy htCap_W))) * ((sy temp_PCM) - (sy temp_W))

eBalanceOnWtrDerivEqn5 =  
  (deriv (sy temp_W) time) $= 
  ((sy coil_HTC) * (sy coil_SA) / 
  ((sy wMass) * (sy htCap_W))) *  ((sy temp_C) - (sy temp_W)) +
  ((sy pcm_HTC) * (sy pcmSA) / 
  ((sy coil_HTC) * (sy coil_SA))) * ((sy coil_HTC) * (sy coil_SA) / 
  ((sy wMass) * (sy htCap_W))) * ((sy temp_PCM) - (sy temp_W))


eBalanceOnWtrDerivEqn6 = (deriv (sy temp_W) time) $= 
  1 / (sy tauW) * ((sy temp_C) - (sy temp_W)) +
  (sy eta) / (sy tauW) * ((sy temp_PCM) - (sy temp_W))

eBalanceOnWtrDerivEqn7 =  
  (deriv (sy temp_W) time) $= 1 / (sy tauW) * (((sy temp_C) - (sy temp_W)) +
  sy eta * ((sy temp_PCM) - (sy temp_W)))

eBalanceOnWtr_deriv_eqns__im1 :: [Expr]
eBalanceOnWtr_deriv_eqns__im1 = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2,
 eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6,
 eBalanceOnWtrDerivEqn7]

---------
-- IM2 --
---------
eBalanceOnPCM :: InstanceModel
eBalanceOnPCM = im eBalanceOnPCMRC [qw tempMeltP, qw time_final, qw tempInit, qw pcmSA,
 qw pcm_HTC, qw pcmMass, qw htCapSP, qw htCapLP]
  [sy tempInit $< sy tempMeltP] (qw temp_PCM)
   [0 $< sy time $< sy time_final] [makeCite koothoor2013] eBalanceOnPCMDeriv 
   "eBalanceOnPCM" [balPCMDescNote]

eBalanceOnPCMRC :: RelationConcept
eBalanceOnPCMRC = makeRC "eBalanceOnPCMRC" (nounPhraseSP
  "Energy Balance on PCM to find temperature of PCM")
  balPCMDesc balPCMRel -- eBalanceOnPCML

balPCMRel :: Relation
balPCMRel = (deriv (sy temp_PCM) time) $= case_ [case1, case2, case3, case4]
  where case1 = ((1 / (sy tauSP)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpTo (Exc,sy tempMeltP)))

        case2 = ((1 / (sy tauLP)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpFrom (Exc,sy tempMeltP)))

        case3 = (0, (sy temp_PCM) $= (sy tempMeltP))

        case4 = (0, real_interval meltFrac (Bounded (Exc,0) (Exc,1)))

balPCMDesc :: Sentence
balPCMDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ (sy tauSP) $= ((sy pcmMass) * (sy htCapSP)) /
  ((sy pcm_HTC) * (sy pcmSA))), S "is a constant",
  sParen (unwrap $ getUnit tauSP) +:+.
  sParen (makeRef2S ddBalanceSolidPCM),
  (E $ (sy tauLP) $= ((sy pcmMass) * (sy htCapLP)) /
  ((sy pcm_HTC) * (sy pcmSA))), S "is a constant",
  sParen (unwrap $ getUnit tauSP),
  sParen (makeRef2S ddBalanceLiquidPCM)]

balPCMDescNote :: Sentence
balPCMDescNote = foldlSent [
  (E (sy tempMeltP)) `sC` (E (sy time_final)) `sC` (E (sy tempInit)) `sC`
  (E (sy pcm_HTC)) `sC` (E (sy pcmMass)) `sC` (E (sy htCapSP)) `sC`
  (E (sy htCapSP)), S "form" +:+. sParen (makeRef2S eBalanceOnWtr),
  S "The input is constrained so that", (E (sy tempInit $< sy tempMeltP)),
  sParen (makeRef2S assumpPIS),
  (E (sy temp_PCM)) `sC` (E (0 $< sy time $< sy time_final)) `sC`
  (S "with initial conditions")
  `sC` (E (sy temp_W $= sy temp_PCM $= sy tempInit)) `sC`
  (S "FIXME t_w(0) = t_p(0)") `sC`
  makeRef2S assumpSITWP `sC` (S "and"), (E (sy temp_W)),
  S "from", (makeRef2S eBalanceOnWtr) `sC`
  S "such that the following governing ODE is satisfied.",
  S "The temperature remains constant at",
  (E (sy tempMeltP)) `sC`
  (S "even with the heating (or cool-ing), until the phase change has occurred for all of the material; that is as long as"),
  (E (0 $< sy meltFrac $< 1)), S "(from", makeRef2S dd4MeltFrac,
  S ") is determined as part of the heat energy in the PCM, as given in" +:+.
  sParen (makeRef2S heatEInPCM),
  -- Addition based on smiths manual version.
  (E $ (sy tauSP) $= ((sy pcmMass) * (sy htCapSP)) /
  ((sy pcm_HTC) * (sy pcmSA))), S "is a constant",
  sParen (unwrap $ getUnit tauSP) +:+.
  sParen (makeRef2S ddBalanceSolidPCM),
  
  (E $ (sy tauLP) $= ((sy pcmMass) * (sy htCapLP)) /
  ((sy pcm_HTC) * (sy pcmSA))), S "is a constant",
  sParen (unwrap $ getUnit tauLP),
  sParen (makeRef2S ddBalanceLiquidPCM)]

 ----------------------------------------------
--    Derivation of eBalanceOnPCM          --
----------------------------------------------
eBalanceOnPCMDeriv :: Derivation
eBalanceOnPCMDeriv =
  [S "Detailed derivation of the" +:+ phrase energy +:+ S "balance on the PCM during " +:+ 
    S "sensible heating phase:" ] ++
  (weave [eBalanceOnPCMDerivSentences, map E eBalanceOnPCM_deriv_eqns__im2])
  ++ (eBalanceOnPCMDerivDesc5 htCapSP htCapLP tauSP tauLP surface area melting vol assumpVCMPN)
  ++ (eBalanceOnPCMDerivDesc6 temp_PCM)
  ++ (eBalanceOnPCMDerivDesc7 boiling solid liquid assumpNGSP)

eBalanceOnPCMDerivSentences :: [Sentence]
eBalanceOnPCMDerivSentences = map foldlSentCol [
  eBalanceOnPCMDerivDesc1 rOfChng temp_PCM energy water vol pcmVol pcmMass heatCapSpec
  htCapSP htFlux htFluxP phaseChange pcmSA heat assumpNIHGBWP volHtGen,
  eBalanceOnPCMDerivDesc2 dd2HtFluxP htFluxP,
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
eq6 = [ch pcmMass, ch htCapSP]

eq7:: [Sentence]
eq7 = [ch tauSP, S "=", ch pcmMass, ch htCapSP, S "/", 
  ch pcm_HTC, ch pcmSA]
eq6_1 :: Expr
eq6_1 = (sy temp_PCM) $= (sy tempMeltP)
eq6_2 :: [Sentence]
eq6_2 = [S "d" +:+ ch temp_PCM +:+ S "/ d" +:+ ch time +:+ S "= 0"]


eBalanceOnPCM_Eqn1, eBalanceOnPCM_Eqn2, eBalanceOnPCM_Eqn3,
 eBalanceOnPCM_Eqn4 :: Expr

eBalanceOnPCM_Eqn1 = (sy pcmMass) * (sy htCapSP) * (deriv (sy temp_PCM) time) $= 
  (sy htFluxP) * (sy pcmSA)

eBalanceOnPCM_Eqn2 = (sy pcmMass) * (sy htCapSP) * (deriv (sy temp_PCM) time) $= 
  (sy pcm_HTC) * (sy pcmSA) *  ((sy temp_W) - (sy temp_PCM))

eBalanceOnPCM_Eqn3 = (deriv (sy temp_PCM) time) $= 
  (((sy pcm_HTC) * (sy pcmSA)) / ((sy pcmMass) * (sy htCapSP))) *  ((sy temp_W) - (sy temp_PCM))

eBalanceOnPCM_Eqn4 = 
  (deriv (sy temp_PCM) time) $= 
  (1 / sy tauSP) * ((sy temp_W) - (sy temp_PCM))

eBalanceOnPCM_deriv_eqns__im2 :: [Expr]
eBalanceOnPCM_deriv_eqns__im2 = [eBalanceOnPCM_Eqn1, eBalanceOnPCM_Eqn2,
 eBalanceOnPCM_Eqn3, eBalanceOnPCM_Eqn4]

---------
-- IM3 --
---------
heatEInWtr :: InstanceModel
heatEInWtr = im heatEInWtrRC [qw tempInit, qw wMass, qw htCap_W, qw wMass] 
  [] (qw w_E) [0 $< sy time $< sy time_final] [makeCite koothoor2013] [] "heatEInWtr"
  [htWtrDesc]

heatEInWtrRC :: RelationConcept
heatEInWtrRC = makeRC "heatEInWtrRC" (nounPhraseSP "Heat energy in the water")
  htWtrDesc htWtrRel -- heatEInWtrL

htWtrRel :: Relation
htWtrRel = (apply1 w_E time) $= (sy htCap_W) * (sy wMass) *
  ((apply1 temp_W time) - sy tempInit)

htWtrDesc :: Sentence
htWtrDesc = foldlSent [S "The above", phrase equation, S "is derived using" +:+. 
  makeRef2S sensHtE, ch w_E `isThe` phrase change, S "in", 
  phrase thermalEnergy, S "of the", phrase liquid, phrase water, 
  S "relative to the", phrase energy, S "at the initial", phrase temp, 
  sParen (ch tempInit) +:+. sParen (unwrap $ getUnit pcmInitMltE), 
  (ch htCap_W) `isThe` phrase heatCapSpec, S "of", phrase liquid, phrase water,
  sParen (unwrap $ getUnit htCapSP) `sAnd` (ch wMass) `isThe` phrase mass, 
  S "of the", phrase water +:+. sParen (unwrap $ getUnit wMass), S "The", 
  phrase change, S "in", phrase temp, S "is the difference between the", 
  phrase temp, S "at", phrase time, ch time, sParen (unwrap $ getUnit tInitMelt) `sC`
  (ch temp_W) `sAnd` S "the", phrase tempInit `sC` ch tempInit +:+.
  sParen (unwrap $ getUnit tempInit), S "This", phrase equation,
  S "applies as long as", (E $ real_interval temp_W (Bounded (Exc,0) (Exc,100)))
  :+: unwrap (getUnit temp_W), sParen $ makeRef2S assumpWAL `sC` makeRef2S assumpAPT]

---------
-- IM4 --
---------
heatEInPCM :: InstanceModel
heatEInPCM = imNoDeriv heatEInPCMRC [qw tempMeltP, qw time_final, qw tempInit, qw pcmSA,
 qw pcm_HTC, qw pcmMass, qw htCapSP, qw htCapLP, qw temp_PCM, qw htFusion, qw tInitMelt]
  [sy tempInit $< sy tempMeltP] (qw pcm_E)
  [0 $< sy time $< sy time_final] [makeCite koothoor2013]
  "heatEInPCM" [htPCMDesc]

heatEInPCMRC :: RelationConcept
heatEInPCMRC = makeRC "heatEInPCMRC" (nounPhraseSP "Heat energy in the PCM")
  htPCMDesc htPCMRel

htPCMRel :: Relation
htPCMRel = sy pcm_E $= case_ [case1, case2, case3, case4]
  where case1 = (sy htCapSP * sy pcmMass * ((apply1 temp_PCM time) -
          sy tempInit), real_interval temp_PCM (UpTo (Exc, sy tempMeltP)))

        case2 = (sy pcmInitMltE + (sy htFusion * sy pcmMass) +
          (sy htCapLP * sy pcmMass * ((apply1 temp_PCM time) -
          sy tempMeltP)), real_interval temp_PCM (UpFrom (Exc, sy tempMeltP)))

        case3 = (sy pcmInitMltE + (apply1 latentEP time),
          (sy temp_PCM) $= (sy tempMeltP))

        case4 = (sy pcmInitMltE + (apply1 latentEP time),
          real_interval meltFrac (Bounded (Exc,0) (Exc,1)))

htPCMDesc :: Sentence
htPCMDesc = foldlSent [S "The above", phrase equation,S "is derived using" +:+.
  (makeRef2S sensHtE `sAnd` makeRef2S latentHtE), ch pcm_E `isThe` phrase change,
  S "in", phrase thermalEnergy, S "of the", short phsChgMtrl, S "relative to the",
  phrase energy, S "at the", phrase tempInit, sParen (ch tempInit) +:+.
  unwrap (getUnit pcmInitMltE), ch pcm_E, S "for the", phrase solid,
  short phsChgMtrl, S "is found using", makeRef2S sensHtE, S "for", phrase sensHeat,
  S "ing, with", phrase heatCapSpec `ofThe` phrase solid, short phsChgMtrl `sC`
  ch htCapSP, sParen (unwrap $ getUnit htCapSP), S "and the", phrase change, S "in the",
  short phsChgMtrl, phrase temp, S "from the", phrase tempInit +:+.
  sParen (unwrap $ getUnit tempInit), ch pcm_E, S "for the melted", short phsChgMtrl,
  sParen (E (sy temp_PCM $> sy pcmInitMltE)), S "is found using", makeRef2S sensHtE,
  S "for", phrase sensHeat, S "of the" +:+. phrase liquid, short phsChgMtrl,
  S "plus the", phrase energy, S "when", phrase melting, S "starts, plus the", phrase energy,
  S "required to melt all of the", short phsChgMtrl, S "The", phrase energy, S "when",
  phrase melting, S "starts is", ch pcmInitMltE +:+. sParen (unwrap $ getUnit pcmInitMltE),
  S "The", phrase energy, S "required to melt all of the", short phsChgMtrl, S "is",
  E (sy htFusion * sy pcmMass), sParen (unwrap $ getUnit pcmInitMltE) +:+.
  sParen (makeRef2S dd3HtFusion), phrase heatCapSpec `ofThe` phrase liquid, short phsChgMtrl,
  S "is", ch htCapLP, sParen (unwrap $ getUnit htCapLP) `sAnd` S "the", phrase change,
  S "in", phrase temp, S "is", E (sy temp_PCM - sy tempMeltP) +:+.
  sParen (unwrap $ getUnit tempMeltP), ch pcm_E, S "during", phrase melting, S "of the",
  short phsChgMtrl, S "is found using the", phrase energy, S "required at", S "instant" +:+
  phrase melting `ofThe` short phsChgMtrl, S "begins" `sC` ch pcmInitMltE, S "plus the",
  phrase latentHeat, phrase energy, S "added to the", short phsChgMtrl `sC`
  ch latentEP, sParen (unwrap $ getUnit latentEP), S "since the", phrase time, S "when",
  phrase melting, S "began", ch tInitMelt +:+. sParen (unwrap $ getUnit tInitMelt),
  S "The", phrase heat, phrase energy, S "for", phrase boiling, S "of the", short phsChgMtrl,
  S "is not detailed" `sC` S "since the", short phsChgMtrl, S "is assumed to either be in a", 
  phrase solid, S "or", phrase liquid, S "state", sParen (makeRef2S assumpNGSP),
  sParen (makeRef2S assumpPIS)]

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
