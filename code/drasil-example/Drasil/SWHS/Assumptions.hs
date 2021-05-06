module Drasil.SWHS.Assumptions where --all of this file is exported

import Language.Drasil
import Control.Lens ((^.))
import Utils.Drasil

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
  S "The only form" `sOf` phrase energy, S "that" `sIs`
  S "relevant for this" +:+. (phrase problem `sIs` phrase CT.thermalEnergy),
  S "All other forms" `sOf` phrase energy `sC` S "such as",
  phrase mechEnergy `sC` S "are assumed to be negligible"]
assumpS2 = foldlSent [S "All", phrase CT.heatTrans, S "coefficients" `sAre`
                      S "constant over", phrase time]
assumpS3 = foldlSent [
  S "The", phrase water `sIn` S "the", phrase tank,
  S "is fully mixed, so the", phrase tempW `isThe` 
  S "same throughout the entire", phrase tank]
assumpS4 = foldlSent [
  S "The", phrase tempPCM `isThe` S "same throughout the", phrase pcmVol]
  --FIXME `sC` makeRefS likeChg1]
assumpS5 = foldlSent [
  S "The", phrase wDensity `sAnd` phrase pcmDensity,
  S "have no spatial variation; that is" `sC`
  S "they are each constant over their entire", phrase vol]
assumpS6 = foldlSent [
  S "The", foldlList Comma List [phrase htCapW, phrase htCapSP,
  phrase htCapLP], S "have no spatial variation; that",
  S "is" `sC` S "they are each constant over their entire",
  phrase vol]
assumpS7 = foldlSent [
  CT.lawConvCooling ^. defn, S "applies between the",
  phrase coil `andThe` phrase water]
assumpS8 = foldlSent [
  S "The", phrase tempC `sIs` S "constant over", phrase time]
assumpS9 = foldlSent [
  S "The", phrase tempC, S "does not vary along its length"]
assumpS10 = foldlSent [
  CT.lawConvCooling ^. defn, S "applies between the",
  phrase water `andThe` short phsChgMtrl]
assumpS11 = foldlSent [
  S "The", phrase model, S "only accounts for", (charging ^. defn) `sC`
  S "not" +:+. phrase discharging, S "The", phrase tempW `sAnd`
  phrase tempPCM, S "can only increase, or remain",
  S "constant; they do not decrease. This implies that the",
  phrase tempInit, Ref $ makeRef2 assumpSITWP, S "is less than (or equal)"
  `toThe` phrase tempC]
assumpS12 = foldlSent [phrase tempInit `the_ofThe'` phrase water `andThe`
  short phsChgMtrl `isThe` S "same"]
assumpS13 = foldlSent [
  S "The", phrase simulation, S "will start with the",
  short phsChgMtrl, S "in a", solid ^. defn]
assumpS14 mat = foldlSent [
  (S "operating" +:+ phrase temp +:+ S "range") `the_ofThe'` phrase system,
  S "is such that the", mat,
  S "is always in" +:+. (liquid ^. defn), S "That is" `sC`
  S "the", phrase temp, S "will not drop below the",
  phrase meltPt `sOf` phrase water `sC` S "or rise above its",
  phrase boilPt]
assumpS15 = foldlSent [
  S "The", phrase tank `sIs` phrase perfectInsul,
  S "so that there is no", phrase CT.heat, S "loss from the",
  phrase tank]
assumpS16 = foldlSent [
  S "No internal", phrase CT.heat, S "is generated by either the",
  phrase water `sOr` S "the", short phsChgMtrl :+:
  S "; therefore" `sC` S "the", phrase volHtGen, S "is zero"]
assumpS17 = foldlSent [
  (phrase vol +:+ phrase change) `the_ofThe'` short phsChgMtrl,
  S "due to", phrase CT.melting, S "is negligible"]
assumpS18 = foldlSent [
  S "The", short phsChgMtrl `sIs` S "either in a", liquid ^. defn,
  S "or a", solid ^. defn, S "but not a", gaseous ^. defn]
assumpS19 = foldlSent [
  S "The pressure in the", phrase tank, S "is atmospheric" `sC` S "so the",
  phrase meltPt `sAnd` phrase boilPt `sAre` S (show (0 :: Integer)) :+:
  Sy (unit_symb temp) `sAnd` S (show (100 :: Integer)) :+:
  Sy (unit_symb temp) `sC` S "respectively"]
assumpS20 = foldlSent [
  S "When considering the", phrase wVol `sIn` S "the",
  phrase tank `sC` (phrase vol `the_ofThe` phrase coil),
  S "is assumed to be negligible"]
  --FIXME , sSqBr $ makeRefS req2]

--- Again, list structure is same between all examples.
-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.
