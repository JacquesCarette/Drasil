module Drasil.SWHS.IMods (iMods, eBalanceOnWtr, eBalanceOnWtrDerivDesc1,
  eBalanceOnPCM, heatEInWtr, heatEInPCM, instModIntro) where

import Language.Drasil
import Theory.Drasil (DataDefinition, InstanceModel, im, imNoDeriv)
import Utils.Drasil
import Control.Lens((^.))

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
import Drasil.SWHS.Unitals (coilHTC, coilSA, eta, htFluxC, htFluxP, htCapLP, 
  htCapSP, htCapW, htFusion, latentEP, meltFrac, pcmE, pcmHTC, pcmInitMltE, 
  pcmMass, pcmSA, pcmVol, tInitMelt, tauLP, tauSP, tauW, tempC, tempInit, 
  tempMeltP, tempPCM, tempW, timeFinal, volHtGen, watE, wMass, wVol) 
import Drasil.SWHS.GenDefs (rocTempSimp)

iMods :: [InstanceModel]
iMods = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

---------
-- IM1 --
---------
eBalanceOnWtr :: InstanceModel
eBalanceOnWtr = im eBalanceOnWtrRC [qw wMass, qw htCapW, qw coilHTC, qw pcmSA,
  qw pcmHTC, qw coilSA, qw tempPCM, qw timeFinal, qw tempC, qw tempInit]
  [sy tempInit $< sy tempC] (qw tempW)
  [0 $<= sy time $<= sy timeFinal] [makeCite koothoor2013] eBalanceOnWtrDeriv
  "eBalanceOnWtr" balWtrDesc

eBalanceOnWtrRC :: RelationConcept
eBalanceOnWtrRC = makeRC "eBalanceOnWtrRC" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") (tempW ^. defn) balWtrRel 
  -- eBalanceOnWtrL

balWtrRel :: Relation
balWtrRel = deriv (sy tempW) time $= 1 / sy tauW *
  ((sy tempC - apply1 tempW time) +
  sy eta * (apply1 tempPCM time - apply1 tempW time))

balWtrDesc :: [Sentence]
balWtrDesc = map foldlSent [[E (sy tempPCM) `sIs` S "defined by", makeRef2S eBalanceOnPCM],
  [S "The", phrase input_, phrase constraint, E $ sy tempInit $<= sy tempC,
   S "comes from", makeRef2S assumpCTNOD],
  [E (sy tauW) `sIs` S "calculated from", S "FIXME: Missing DD Issue 1484"],
  [E (sy eta) `sIs` S "calculated from", S "FIXME: Missing DD Issue 1484"],
  [S "The initial", plural condition, S "for the", getAcc ode `sAre` 
   E (apply (sy tempW) [Int 0] $= apply (sy tempPCM) [Int 0] $= sy tempInit) `follows` assumpSITWP],
  [S "The", getAcc ode, S "applies as long as the", phrase water `sIs` EmptyS `sIn`
  phrase liquid, S "form" `sC` (E $ realInterval tempW (Bounded (Exc,0) (Exc,100))),
  sParen (unwrap $ getUnit tempW), S "where", E 0, sParen (unwrap $ getUnit tempW) `sAnd`
  E 100, sParen (unwrap $ getUnit tempW) `sAre` S "the", phrase melting `sAnd`
  plural boilPt `sOf` phrase water `sC` S "respectively",
  sParen (S "from" +:+ makeRef2S assumpWAL `sAnd` makeRef2S assumpAPT)]]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
-- type Derivation = [Sentence]
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv =
  S "Derivation" `sOf` S "the" +:+ phrase energy +:+ S "balance on water:" :
  weave [eBalanceOnWtrDerivSentences, map E eBalanceOnWtrDerivEqnsIM1]

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = map foldlSentCol [
  eBalanceOnWtrDerivDesc1 htTransEnd overAreas extraAssumps assumpNIHGBWP,
  eBalanceOnWtrDerivDesc2 dd1HtFluxC dd2HtFluxP,
  eBalanceOnWtrDerivDesc3 wMass htCapW,
  eBalanceOnWtrDerivDesc4 eq2,
  eBalanceOnWtrDerivDesc5,
  eBalanceOnWtrDerivDesc6 eq3 eq4,
  eBalanceOnWtrDerivDesc7 eq5]

eBalanceOnWtrDerivDesc1 :: Sentence -> Sentence-> Sentence -> ConceptInstance -> [Sentence]
eBalanceOnWtrDerivDesc1 htEnd oa ea htA = [S "To find the", phrase rOfChng `sOf` (E $ sy tempW) `sC`
  S "we look at the", phrase energy, S "balance on" +:+. phrase water, S "The",
  phrase vol, S "being considered" `isThe` (phrase vol `sOf` phrase water) `sIn` S "the",
  phrase tank, (E $ sy wVol) `sC` S "which has", phrase mass +:+. ((E $ sy wMass) `sAnd`
  phrase heatCapSpec `sC` (E $ sy htCapW)), atStart heatTrans, S "occurs in the",
  phrase water, S "from the", phrase coil, S "as", E $ sy htFluxC,
  sParen (makeRef2S dd1HtFluxC) :+: htEnd `sC` EmptyS +:+. oa, ea, S "No",
  phrase heatTrans, S "occurs to", S "outside" `ofThe` phrase tank `sC`
  S "since it has been assumed to be perfectly insulated" +:+. sParen (makeRef2S assumpPIT),
  S "Since the", phrase assumption, S "is made that no internal heat is generated" +:+.
  (sParen (makeRef2S htA) `sC` (E $ sy volHtGen $= 0)), S "Therefore" `sC` S "the",
  phrase equation, S "for", makeRef2S rocTempSimp, S "can be written as"]

htTransEnd :: Sentence
htTransEnd = foldlSent_ [S " " `sAnd` S "from the", phrase water, S "into the",
  getAcc phsChgMtrl, S "as", E $ sy htFluxP, sParen (makeRef2S dd2HtFluxP)]

overAreas :: Sentence
overAreas = S "over areas" +:+ ((E $ sy coilSA) `sAnd` (E $ sy pcmSA) `sC` S "respectively")

extraAssumps :: Sentence
extraAssumps = foldlSent [S "The thermal flux" `sIs` S "constant over", (E $ sy coilSA) `sC`
  S "since", phrase temp `ofThe` phrase coil `sIs` S "assumed to not vary along its length",
  sParen (makeRef2S assumpTHCCoL) `sC` EmptyS `andThe` S "thermal flux" `sIs` S "constant over",
  (E $ sy pcmSA) `sC` S "since", phrase temp `ofThe` getAcc phsChgMtrl `isThe`
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
  [S "Setting", E eq33, sParen (makeRef2S dd3HtFusion) `sAnd` E eq44,
  sParen (makeRef2S dd4MeltFrac) `sC` S "Equation (5) can be written as"]

eBalanceOnWtrDerivDesc7 :: Expr -> [Sentence]
eBalanceOnWtrDerivDesc7 eq55 = 
  [S "Finally" `sC` S "factoring out", E eq55, S ", we are left with the governing",
  getAcc ode, S "for", sParen (makeRef2S eBalanceOnWtr)]

eq2 :: [Sentence]
eq2 = [ch coilHTC, ch coilSA, S "/", ch coilHTC, ch coilSA]

eq3, eq4, eq5:: Expr
eq3 = sy tauW $= (sy wMass * sy htCapW) / (sy coilHTC * sy coilSA)
eq4 = sy eta $= (sy pcmHTC * sy pcmSA) / 
  (sy coilHTC * sy coilSA)
eq5 = 1 / sy tauW

eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3,
 eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7 :: Expr

eBalanceOnWtrDerivEqn1 = sy wMass * sy htCapW * deriv (sy tempW) time $= 
  sy htFluxC * sy coilSA - sy htFluxP * sy pcmSA

eBalanceOnWtrDerivEqn2 = sy wMass * sy htCapW * deriv (sy tempW) time $= 
  sy coilHTC * sy coilSA * (sy tempC - sy tempW) -
  sy pcmHTC * sy pcmSA * (sy tempW - sy tempPCM)

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $= 
  (sy coilHTC * sy coilSA / 
  (sy wMass * sy htCapW)) * (sy tempC - sy tempW) -
  (sy pcmHTC * sy pcmSA / 
  (sy wMass * sy htCapW)) * (sy tempW - sy tempPCM)

eBalanceOnWtrDerivEqn4 = 
  deriv (sy tempW) time $= 
  (sy coilHTC * sy coilSA / 
  (sy wMass * sy htCapW)) *  (sy tempC - sy tempW) +
  (sy coilHTC * sy coilSA / 
  (sy coilHTC * sy coilSA)) * (sy pcmHTC * sy pcmSA / 
  (sy wMass * sy htCapW)) * (sy tempPCM - sy tempW)

eBalanceOnWtrDerivEqn5 =  
  deriv (sy tempW) time $= 
  (sy coilHTC * sy coilSA / 
  (sy wMass * sy htCapW)) *  (sy tempC - sy tempW) +
  (sy pcmHTC * sy pcmSA / 
  (sy coilHTC * sy coilSA)) * (sy coilHTC * sy coilSA / 
  (sy wMass * sy htCapW)) * (sy tempPCM - sy tempW)


eBalanceOnWtrDerivEqn6 = deriv (sy tempW) time $= 
  1 / sy tauW * (sy tempC - sy tempW) + sy eta / sy tauW * (sy tempPCM - sy tempW)

eBalanceOnWtrDerivEqn7 = deriv (sy tempW) time $=
  1 / sy tauW * ((sy tempC - sy tempW) + sy eta * (sy tempPCM - sy tempW))

eBalanceOnWtrDerivEqnsIM1 :: [Expr]
eBalanceOnWtrDerivEqnsIM1 = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2,
 eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6,
 eBalanceOnWtrDerivEqn7]

---------
-- IM2 --
---------
eBalanceOnPCM :: InstanceModel
eBalanceOnPCM = im eBalanceOnPCMRC [qw tempMeltP, qw timeFinal, qw tempInit, qw pcmSA,
  qw pcmHTC, qw pcmMass, qw htCapSP, qw htCapLP]
  [sy tempInit $< sy tempMeltP] (qw tempPCM)
  [0 $<= sy time $<= sy timeFinal] [makeCite koothoor2013] eBalanceOnPCMDeriv 
  "eBalanceOnPCM" balPCMNotes

eBalanceOnPCMRC :: RelationConcept
eBalanceOnPCMRC = makeRC "eBalanceOnPCMRC" (nounPhraseSP
  "Energy Balance on PCM to find temperature of PCM")
  (tempPCM ^. defn) balPCMRel -- eBalanceOnPCML

balPCMRel :: Relation
balPCMRel = deriv (sy tempPCM) time $= case_ [case1, case2, case3]
  where case1 = ((1 / sy tauSP) * (apply1 tempW time -
          apply1 tempPCM time), realInterval tempPCM (UpTo (Exc,sy tempMeltP)))
        case2 = ((1 / sy tauLP) * (apply1 tempW time -
          apply1 tempPCM time), realInterval tempPCM (UpFrom (Exc,sy tempMeltP)))
        case3 = (0, sy tempPCM $= sy tempMeltP $&& realInterval meltFrac (Bounded (Exc,0) (Exc,1)))

balPCMNotes :: [Sentence]
balPCMNotes = map foldlSent [
  [E (sy tempW) `sIs` S "defined by", makeRef2S eBalanceOnWtr],
  [S "The", phrase input_, phrase constraint, E $ sy tempInit $<= sy tempMeltP,
   S "comes from", makeRef2S assumpPIS],
  [S "The", phrase temp, S "remains constant at", (E $ sy tempMeltP) `sC`
   S "even with the heating", sParen (S "or cooling") `sC` S "until the",
   phrase phaseChange, S "has occurred for all" `sOf` S "the material; that" `sIs`
   S "as long as" +:+. E (0 $< sy meltFrac $< 1), E $ sy meltFrac,
   sParen (S "from" +:+ makeRef2S dd4MeltFrac) `sIs`
   S "determined as part" `sOf` S "the", phrase heat, phrase energy `sIn`
   S "the", getAcc phsChgMtrl `sC` S "as given" `sIn` sParen (makeRef2S heatEInPCM)],
  [E (sy tauSP) `sIs` S "calculated" `sIn` makeRef2S ddBalanceSolidPCM],
  [E (sy tauLP) `sIs` S "calculated" `sIn` makeRef2S ddBalanceLiquidPCM],
  [S "The initial", plural condition, S "for the", getAcc ode `sAre` 
   E (apply (sy tempW) [Int 0] $= apply (sy tempPCM) [Int 0] $= sy tempInit) `follows` assumpSITWP]]

 ----------------------------------------------
--    Derivation of eBalanceOnPCM          --
----------------------------------------------
eBalanceOnPCMDeriv :: Derivation
eBalanceOnPCMDeriv = foldlSentCol [S "Detailed derivation of the", phrase energy,
  S "balance on the PCM during sensible heating phase"] :
  weave [eBalanceOnPCMDerivSentences, map E eBalanceOnPCMDerivEqnsIM2]
  ++ eBalanceOnPCMDerivDesc5 htCapSP htCapLP tauSP tauLP surface area melting vol assumpVCMPN
  ++ eBalanceOnPCMDerivDesc6 tempPCM
  ++ eBalanceOnPCMDerivDesc7 boiling solid liquid assumpNGSP

eBalanceOnPCMDerivSentences :: [Sentence]
eBalanceOnPCMDerivSentences = map foldlSentCol [
  eBalanceOnPCMDerivDesc1 rOfChng tempPCM energy water vol pcmVol pcmMass heatCapSpec
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
   S "initially for the solid PCM", S "The mass of phase change material is", (E $ sy pm) `andThe`
   phrase hcs `sOf` S "PCM as a solid is" +:+. (E $ sy hsp), S "The", phrase hf,
   S "into the PCM from", phrase wt `sIs` (E $ sy hfp), sParen (makeRef2S dd2HtFluxP),
   S "over", phrase pc, S "material surface area" +:+. (E $ sy ps),
   S "The thermal flux" `sIs` S "constant over", (E $ sy pcmSA) `sC` S "since",
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
   sParen (makeRef2S ass17)]

eBalanceOnPCMDerivDesc6 :: NamedIdea a => a -> [Sentence]
eBalanceOnPCMDerivDesc6 tp =
    [S "In the case where" +:+ E eq6_1 +:+ S "and not all of the PCM is melted" `sC`
   S "the" +:+ phrase tp +:+. S "does not change" +:+ S "Therefore" `sC` 
   S "in this case" +:+ foldlSent eq6_2]

eBalanceOnPCMDerivDesc7 :: ConceptChunk -> ConceptChunk -> ConceptChunk -> ConceptInstance -> [Sentence]
eBalanceOnPCMDerivDesc7 boil sld lqd assp18 =
   [S "This derivation does not consider the" +:+
   phrase boil +:+ S "of the PCM" `sC` S "as the PCM is assumed to either be in a" +:+ phrase sld +:+ S "state or a" +:+
   phrase lqd +:+ S "state" +:+. sParen (makeRef2S assp18)]
--(E $ ((deriv (sy temp_PCm) time) $= 0)
eq6:: [Sentence]
eq6 = [ch pcmMass, ch htCapSP]

eq7:: [Sentence]
eq7 = [ch tauSP, S "=", ch pcmMass, ch htCapSP, S "/", 
  ch pcmHTC, ch pcmSA]
eq6_1 :: Expr
eq6_1 = sy tempPCM $= sy tempMeltP
eq6_2 :: [Sentence]
eq6_2 = [S "d" +:+ ch tempPCM +:+ S "/ d" +:+ ch time +:+ S "= 0"]


eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3,
 eBalanceOnPCMEqn4 :: Expr

eBalanceOnPCMEqn1 = sy pcmMass * sy htCapSP * deriv (sy tempPCM) time $= 
  sy htFluxP * sy pcmSA

eBalanceOnPCMEqn2 = sy pcmMass * sy htCapSP * deriv (sy tempPCM) time $= 
  sy pcmHTC * sy pcmSA * (sy tempW - sy tempPCM)

eBalanceOnPCMEqn3 = deriv (sy tempPCM) time $= 
  ((sy pcmHTC * sy pcmSA) / (sy pcmMass * sy htCapSP)) *  (sy tempW - sy tempPCM)

eBalanceOnPCMEqn4 = deriv (sy tempPCM) time $= 
  (1 / sy tauSP) * (sy tempW - sy tempPCM)

eBalanceOnPCMDerivEqnsIM2 :: [Expr]
eBalanceOnPCMDerivEqnsIM2 = [eBalanceOnPCMEqn1, eBalanceOnPCMEqn2,
 eBalanceOnPCMEqn3, eBalanceOnPCMEqn4]

---------
-- IM3 --
---------
heatEInWtr :: InstanceModel
heatEInWtr = im heatEInWtrRC [qw tempInit, qw wMass, qw htCapW, qw wMass] 
  [] (qw watE) [0 $<= sy time $<= sy timeFinal] [makeCite koothoor2013] [] "heatEInWtr"
  htWtrNotes

heatEInWtrRC :: RelationConcept
heatEInWtrRC = makeRC "heatEInWtrRC" (nounPhraseSP "Heat energy in the water")
  (watE ^. defn) htWtrRel -- heatEInWtrL

htWtrRel :: Relation
htWtrRel = apply1 watE time $= sy htCapW * sy wMass *
  (apply1 tempW time - sy tempInit)

htWtrNotes :: [Sentence]
htWtrNotes = map foldlSent [
  [S "The above", phrase equation, S "is derived using", makeRef2S sensHtE],
  [S "The", phrase change `sIn` phrase temp `isThe` S "difference between the", 
   phrase temp, S "at", phrase time, ch time, sParen (unwrap $ getUnit tInitMelt) `sC`
  ch tempW `andThe` phrase tempInit `sC` ch tempInit, sParen (unwrap $ getUnit tempInit)],
  [S "This", phrase equation, S "applies as long as",
   E (realInterval tempW (Bounded (Exc,0) (Exc,100))) :+:
  unwrap (getUnit tempW), sParen $ makeRef2S assumpWAL `sC` makeRef2S assumpAPT]]

---------
-- IM4 --
---------
heatEInPCM :: InstanceModel
heatEInPCM = imNoDeriv heatEInPCMRC [qw tempMeltP, qw timeFinal, qw tempInit, qw pcmSA,
  qw pcmHTC, qw pcmMass, qw htCapSP, qw htCapLP, qw tempPCM, qw htFusion, qw tInitMelt]
  [sy tempInit $< sy tempMeltP] (qw pcmE)
  [0 $<= sy time $<= sy timeFinal] [makeCite koothoor2013]
  "heatEInPCM" htPCMNotes

heatEInPCMRC :: RelationConcept
heatEInPCMRC = makeRC "heatEInPCMRC" (nounPhraseSP "Heat energy in the PCM")
  (pcmE ^. defn) htPCMRel

htPCMRel :: Relation
htPCMRel = sy pcmE $= case_ [case1, case2, case3, case4]
  where case1 = (sy htCapSP * sy pcmMass * (apply1 tempPCM time -
          sy tempInit), realInterval tempPCM (UpTo (Exc, sy tempMeltP)))

        case2 = (sy pcmInitMltE + (sy htFusion * sy pcmMass) +
          (sy htCapLP * sy pcmMass * (apply1 tempPCM time -
          sy tempMeltP)), realInterval tempPCM (UpFrom (Exc, sy tempMeltP)))

        case3 = (sy pcmInitMltE + apply1 latentEP time,
          sy tempPCM $= sy tempMeltP)

        case4 = (sy pcmInitMltE + apply1 latentEP time,
          realInterval meltFrac (Bounded (Exc,0) (Exc,1)))

htPCMNotes :: [Sentence]
htPCMNotes = map foldlSent [
  [S "The above", phrase equation `sIs` S "derived using",
   makeRef2S sensHtE `sAnd` makeRef2S latentHtE],
  [ch pcmE, S "for the", phrase solid, short phsChgMtrl, S "is found using",
   makeRef2S sensHtE, S "for", phrase sensHeat :+: S "ing, with",
   phrase heatCapSpec `ofThe` phrase solid, short phsChgMtrl `sC` ch htCapSP,
   sParen (unwrap $ getUnit htCapSP) `andThe` phrase change, S "in the",
   short phsChgMtrl, phrase temp, S "from the", phrase tempInit, sParen (unwrap $ getUnit tempInit)],
  [ch pcmE, S "for the melted", short phsChgMtrl, sParen (E (sy tempPCM $> sy pcmInitMltE)),
   S "is found using", makeRef2S sensHtE, S "for", phrase sensHeat, S "of the", phrase liquid,
   short phsChgMtrl, S "plus the", phrase energy, S "when", phrase melting, S "starts" `sC`
   S "plus", (phrase energy +:+ S "required to melt all") `ofThe` short phsChgMtrl], 
  [S "The", phrase energy, S "required to melt all of the", short phsChgMtrl `sIs`
   E (sy htFusion * sy pcmMass), sParen (unwrap $ getUnit pcmInitMltE),
   sParen (S "from" +:+ makeRef2S dd3HtFusion)],
  [S "The", phrase change `sIn` phrase temp `sIs` E (sy tempPCM - sy tempMeltP),
   sParen (unwrap $ getUnit tempMeltP)],
  [ch pcmE, S "during", phrase melting, S "of the", short phsChgMtrl,
   S "is found using the", phrase energy, S "required at", S "instant" +:+
   phrase melting `ofThe` short phsChgMtrl, S "begins" `sC` ch pcmInitMltE, S "plus the",
   phrase latentHeat, phrase energy, S "added" `toThe` short phsChgMtrl `sC`
   ch latentEP, sParen (unwrap $ getUnit latentEP), S "since the", phrase time, S "when",
   phrase melting, S "began", ch tInitMelt, sParen (unwrap $ getUnit tInitMelt)],
  [S "The", phrase heat, phrase energy, S "for", phrase boiling, S "of the", short phsChgMtrl,
   S "is not detailed" `sC` S "since the", short phsChgMtrl, S "is assumed to either be in a", 
   phrase solid `sOr` phrase liquid, S "state", sParen (makeRef2S assumpNGSP),
   sParen (makeRef2S assumpPIS)]]

-----------
-- Intro --
-----------

instModIntro :: Sentence
instModIntro = foldlSent [S "The", plural goal, foldlList Comma List
  (map makeRef2S [waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS]) `sAre`
  S "solved by" +:+. foldlList Comma List (map makeRef2S
  [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]), S "The",
  plural solution, S "for", makeRef2S eBalanceOnWtr `sAnd`
  makeRef2S eBalanceOnPCM `sAre` S "coupled since the", plural solution,
  S "for", ch tempW `sAnd` ch tempPCM +:+. S "depend on one another",
  makeRef2S heatEInWtr, S "can be solved once", makeRef2S eBalanceOnWtr +:+.
  S "has been solved", S "The", plural solution `sOf` makeRef2S eBalanceOnPCM `sAnd`
  makeRef2S heatEInPCM `sAre` S "also coupled" `sC` S "since the",
  phrase tempPCM `andThe` phrase pcmE,S "depend on the", phrase phaseChange]
