module Drasil.SWHS.Changes where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (assumption, value, simulation,
  model, likeChgDom, unlikeChgDom)

import Drasil.SWHS.Concepts (tank, phsChgMtrl, water)
import Drasil.SWHS.Unitals (temp_init, temp_C, temp_PCM)
import Drasil.SWHS.Assumptions (newA4, newA8, newA9, newA11,
  newA12, newA14, newA15, newA16, newA18)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInPCM)

import Data.Drasil.Concepts.Thermodynamics as CT (heat,
  thermal_conductor)
import Data.Drasil.Quantities.Physics (energyUC)

import Data.Drasil.SentenceStructures (foldlSent, sAnd, ofThe)

--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

chgsStart :: (HasShortName x, Referable x) => x -> Sentence
chgsStart a = makeRefS a +:+ S "-"

likelyChgs :: [ConceptInstance]
likelyChgs = [likeChgUTP, likeChgTCVOD, likeChgTCVOL, likeChgDT, likeChgDITPW, likeChgTLH]

likeChgUTP, likeChgTCVOD, likeChgTCVOL, likeChgDT, likeChgDITPW, likeChgTLH :: ConceptInstance

likeChgUTP = cic "likeChgUTP" (
  foldlSent [chgsStart newA4, short phsChgMtrl, S "is actually a poor",
  phrase CT.thermal_conductor `sC` S "so the", phrase assumption,
  S "of uniform", phrase temp_PCM, S "is not likely"] ) "Uniform-Temperature-PCM"
  likeChgDom
--
likeChgTCVOD = cic "likeChgTCVOD" (
  foldlSent [chgsStart newA8, S "The", phrase temp_C, S "will change over",
  (S "course" `ofThe` S "day, depending"), S "on the", phrase energyUC,
  S "received from the sun"] ) "Temperature-Coil-Variable-Over-Day" likeChgDom
--
likeChgTCVOL = cic "likeChgTCVOL" (
  foldlSent [chgsStart newA9, S "The", phrase temp_C,
  S "will actually change along its length as the", phrase water,
  S "within it cools"] ) "Temperature-Coil-Variable-Over-Length" likeChgDom
--
likeChgDT = cic "likeChgDT" (
  foldlSent [chgsStart newA11, S "The", phrase model, S "currently only",
  S "accounts for charging of the tank. A more complete", phrase model,
  S "would also account for discharging of the tank"] ) "Discharging-Tank" likeChgDom
--
likeChgDITPW = cic "likeChgDITPW" (
  foldlSent [chgsStart newA12, S "To add more flexibility to the",
  phrase simulation `sC` (phrase temp_init `ofThe` phrase water) `sAnd`
  S "the", short phsChgMtrl, S "could be allowed to have different",
  plural value] ) "Different-Initial-Temps-PCM-Water" likeChgDom
--
likeChgTLH = cic "likeChgTLH" (
  foldlSent [chgsStart newA15, S "Any real", phrase tank, S "cannot",
  S "be perfectly insulated and will lose", phrase CT.heat] ) "Tank-Lose-Heat"
  likeChgDom

-- List structure same in all examples.

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [unlikeChgWPFS, unlikeChgNIHG, unlikeChgNGS]

unlikeChgWPFS, unlikeChgNIHG, unlikeChgNGS :: ConceptInstance
unlikeChgWPFS = cic "unlikeChgWPFS" (
  foldlSent [makeRefS newA14, S ", ", chgsStart newA18, S "It is unlikely for the change of",
  phrase water, S "from liquid to a solid or the state change of the", phrase phsChgMtrl,
  S "from a liquid to a gas to be considered"] ) "Water-PCM-Fixed-States" unlikeChgDom


unlikeChgNIHG = cic "unlikeChgNIHG" (
  foldlSent [chgsStart newA16, S "Is used for the derivations of",
  makeRefS eBalanceOnWtr, S "and", makeRefS eBalanceOnPCM] )
  "No-Internal-Heat-Generation" unlikeChgDom

unlikeChgNGS = cic "unlikeChgNGS" (
  foldlSent [chgsStart newA18, S "Is used for the derivation of", makeRefS eBalanceOnPCM,
  S "and for the equation given by", makeRefS heatEInPCM, S "to be valid"] )
  "No-Gaseous-State" unlikeChgDom
