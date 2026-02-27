module Drasil.SWHS.Assumptions where --all of this file is exported

import Control.Lens ((^.))

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (system, simulation, model,
  problem, assumpDom)

import Data.Drasil.Quantities.PhysicalProperties (vol)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Quantities.Thermodynamics (boilPt, meltPt, temp)

import Data.Drasil.Concepts.Thermodynamics as CT (heat, melting,
  lawConvCooling, heatTrans, thermalEnergy)
import Data.Drasil.Concepts.PhysicalProperties (solid, liquid, gaseous)
import Data.Drasil.Concepts.Math (change)
import Data.Drasil.Concepts.Physics (mechEnergy)

import Drasil.SWHS.Concepts (coil, tank, phsChgMtrl, water, perfectInsul,
  charging, discharging)
import Drasil.SWHS.Unitals (wVol, volHtGen, tempC, tempInit, tempW,
  tempPCM, htCapLP, htCapW, htCapSP, wDensity, pcmDensity, pcmVol)

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

assumptions :: [ConceptInstance]
assumptions = [assumpTEO, assumpHTCC, assumpCWTAT, assumpTPCAV, assumpDWPCoV, assumpSHECoV,
  assumpLCCCW, assumpTHCCoT, assumpTHCCoL, assumpLCCWP, assumpCTNOD, assumpSITWP,
  assumpPIS, assumpWAL, assumpPIT, assumpNIHGBWP, assumpVCMPN, assumpNGSP,
  assumpAPT, assumpVCN]

assumpTEO, assumpHTCC, assumpCWTAT, assumpTPCAV, assumpDWPCoV, assumpSHECoV,
  assumpLCCCW, assumpTHCCoT, assumpTHCCoL, assumpLCCWP, assumpCTNOD, assumpSITWP,
  assumpPIS, assumpWAL, assumpPIT, assumpNIHGBWP, assumpVCMPN, assumpNGSP,
  assumpAPT, assumpVCN :: ConceptInstance

assumpTEO = cic "assumpTEO"                  assumpS1                   "Thermal-Energy-Only"                       assumpDom
assumpHTCC = cic "assumpHTCC"                assumpS2                   "Heat-Transfer-Coeffs-Constant"             assumpDom
assumpCWTAT = cic "assumpCWTAT"              assumpS3                   "Constant-Water-Temp-Across-Tank"           assumpDom
assumpTPCAV = cic "assumpTPCAV"              assumpS4                   "Temp-PCM-Constant-Across-Volume"           assumpDom
assumpDWPCoV = cic "assumpDWPCoV"            assumpS5                   "Density-Water-PCM-Constant-over-Volume"    assumpDom
assumpSHECoV = cic "assumpSHECov"            assumpS6                   "Specific-Heat-Energy-Constant-over-Volume" assumpDom
assumpLCCCW = cic "assumpLCCCW"              assumpS7                   "Newton-Law-Convective-Cooling-Coil-Water"  assumpDom
assumpTHCCoT = cic "assumpTHCCoT"            assumpS8                   "Temp-Heating-Coil-Constant-over-Time"      assumpDom
assumpTHCCoL = cic "assumpTHCCoL"            assumpS9                   "Temp-Heating-Coil-Constant-over-Length"    assumpDom
assumpLCCWP = cic "assumpLCCWP"              assumpS10                  "Law-Convective-Cooling-Water-PCM"          assumpDom
assumpCTNOD = cic "assumpCTNOD"              assumpS11                  "Charging-Tank-No-Temp-Discharge"           assumpDom
assumpSITWP = cic "assumpSITWP"              assumpS12                  "Same-Initial-Temp-Water-PCM"               assumpDom
assumpPIS = cic "assumpPIS"                  assumpS13                  "PCM-Initially-Solid"                       assumpDom
assumpWAL = cic "assumpWAL"                  (assumpS14 $ phrase water) "Water-Always-Liquid"                       assumpDom
assumpPIT = cic "assumpPIT"                  assumpS15                  "Perfect-Insulation-Tank"                   assumpDom
assumpNIHGBWP = cic "assumpNIHGBWP"          assumpS16                  "No-Internal-Heat-Generation-By-Water-PCM"  assumpDom
assumpVCMPN = cic "assumpVCMPN"              assumpS17                  "Volume-Change-Melting-PCM-Negligible"      assumpDom
assumpNGSP = cic "assumpNGSP"                assumpS18                  "No-Gaseous-State-PCM"                      assumpDom
assumpAPT = cic "assumpAPT"                  assumpS19                  "Atmospheric-Pressure-Tank"                 assumpDom
assumpVCN = cic "assumpVCN"                  assumpS20                  "Volume-Coil-Negligible"                    assumpDom

assumpS1, assumpS2, assumpS3, assumpS4, assumpS5, assumpS6, assumpS7,
  assumpS8, assumpS9, assumpS10, assumpS11, assumpS12, assumpS13,
  assumpS15, assumpS16, assumpS17, assumpS18, assumpS19, assumpS20 :: Sentence

assumpS14 :: Sentence -> Sentence

assumpS1 = foldlSent [
  S "The only form" `S.of_` phrase energy, S "that" `S.is`
  S "relevant for this" +:+. (phrase problem `S.is` phrase CT.thermalEnergy),
  S "All other forms" `S.of_` phrase energy `sC` S "such as",
  phrase mechEnergy `sC` S "are assumed to be negligible"]
assumpS2 = foldlSent [S "All", phrase CT.heatTrans, S "coefficients" `S.are`
                      S "constant over", phrase time]
assumpS3 = foldlSent [
  D.toSent (atStartNP (NP.the (water `inThe` tank))),
  S "is fully mixed, so the", phrase tempW `S.isThe`
  S "same throughout the entire", phrase tank]
assumpS4 = foldlSent [
  D.toSent (atStartNP (the tempPCM)) `S.isThe` S "same throughout the", phrase pcmVol]
  --FIXME `sC` makeRefS likeChg1]
assumpS5 = foldlSent [
  D.toSent (atStartNP (NP.the (wDensity `and_` pcmDensity))),
  S "have no spatial variation; that is" `sC`
  S "they" `S.are` S "each constant over their entire", phrase vol]
assumpS6 = foldlSent [
  S "The", foldlList Comma List [phrase htCapW, phrase htCapSP,
  phrase htCapLP], S "have no spatial variation; that",
  S "is" `sC` S "they" `S.are` S "each constant over their entire",
  phrase vol]
assumpS7 = foldlSent [
  CT.lawConvCooling ^. defn, S "applies between the",
  phrase coil `S.andThe` phrase water]
assumpS8 = foldlSent [
  D.toSent (atStartNP (the tempC)) `S.is` S "constant over", phrase time]
assumpS9 = foldlSent [
  D.toSent (atStartNP (the tempC)), S "does not vary along its length"]
assumpS10 = foldlSent [
  CT.lawConvCooling ^. defn, S "applies between the",
  phrase water `S.andThe` short phsChgMtrl]
assumpS11 = foldlSent [
  D.toSent (atStartNP (the model)), S "only accounts for", (charging ^. defn) `sC`
  S "not" +:+. phrase discharging, D.toSent (atStartNP (NP.the (tempW `and_` tempPCM))),
  S "can only increase" `sC` S "or remain",
  S "constant; they do not decrease. This implies that the",
  phrase tempInit, refS assumpSITWP `S.is` S "less than (or equal)"
  `S.toThe` phrase tempC]
assumpS12 = foldlSent [D.toSent (atStartNP (tempInit `the_ofThe` water)) `S.andThe`
  short phsChgMtrl `S.isThe` S "same"]
assumpS13 = foldlSent [
  D.toSent (atStartNP (the simulation)), S "will start with the",
  short phsChgMtrl, S "in a", solid ^. defn]
assumpS14 mat = foldlSent [
  (S "operating" +:+ phrase temp +:+ S "range") `S.the_ofTheC` phrase system,
  S "is such that the", mat `S.is` S "always in" +:+. (liquid ^. defn), S "That is" `sC`
  D.toSent (phraseNP (the temp)), S "will not drop below the",
  D.toSent (phraseNP (meltPt `of_` water)) `sC` S "or rise above its",
  phrase boilPt]
assumpS15 = foldlSent [
  D.toSent (atStartNP (NP.the (tank `is` perfectInsul))),
  S "so that there is no", phrase CT.heat, S "loss from the",
  phrase tank]
assumpS16 = foldlSent [
  S "No internal", phrase CT.heat `S.is` S "generated by either the",
  phrase water `S.or_` S "the", short phsChgMtrl :+:
  S "; therefore" `sC` D.toSent (phraseNP (the volHtGen)) `S.is` S "zero"]
assumpS17 = foldlSent [
  (phrase vol +:+ phrase change) `S.the_ofTheC` short phsChgMtrl,
  S "due to", phrase CT.melting `S.is` S "negligible"]
assumpS18 = foldlSent [
  S "The", short phsChgMtrl `S.is` S "either in a", liquid ^. defn,
  S "or a", solid ^. defn, S "but not a", gaseous ^. defn]
assumpS19 = foldlSent [
  S "The pressure" `S.inThe` phrase tank `S.is` S "atmospheric" `sC` S "so the",
  D.toSent (phraseNP (meltPt `and_` boilPt)) `S.are` S (show (0 :: Integer)) :+:
  Sy (unit_symb temp) `S.and_` S (show (100 :: Integer)) :+:
  Sy (unit_symb temp) `sC` S "respectively"]
assumpS20 = foldlSent [
  S "When considering the", D.toSent (phraseNP (wVol `inThe` tank))
  `sC` D.toSent (phraseNP (vol `the_ofThe` coil)) `S.is` S "assumed to be negligible"]
  --FIXME , sSqBr $ makeRefS req2]

--- Again, list structure is same between all examples.
-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.
