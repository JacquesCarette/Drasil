module Drasil.SWHS.Changes (likelyChgs, likeChgTCVOD, likeChgTCVOL,
  likeChgTLH, unlikelyChgs) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (assumption, value, simulation,
  model, likeChgDom, unlikeChgDom)
import Drasil.Sentence.Combinators (chgsStart)

import Drasil.SWHS.Concepts (tank, phsChgMtrl, water)
import Drasil.SWHS.Unitals (tempInit, tempC, tempPCM)
import Drasil.SWHS.Assumptions (assumpTPCAV, assumpTHCCoT, assumpTHCCoL,
  assumpCTNOD, assumpSITWP, assumpWAL, assumpPIT, assumpNIHGBWP, assumpNGSP)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInPCM)

import Data.Drasil.Concepts.Thermodynamics as CT (heat,
  thermalConductor)
import Data.Drasil.Quantities.Physics (energy)

--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

likelyChgs :: [ConceptInstance]
likelyChgs = [likeChgUTP, likeChgTCVOD, likeChgTCVOL, likeChgDT, likeChgDITPW,
  likeChgTLH]

likeChgUTP, likeChgTCVOD, likeChgTCVOL, likeChgDT, likeChgDITPW,
  likeChgTLH :: ConceptInstance

likeChgUTP = cic "likeChgUTP" (
  foldlSent [chgsStart assumpTPCAV (short phsChgMtrl) `S.is` S "actually a poor",
  phrase CT.thermalConductor `sC` S "so the", phrase assumption,
  S "of uniform", phrase tempPCM `S.is` S "not likely"] ) "Uniform-Temperature-PCM"
  likeChgDom
--
likeChgTCVOD = cic "likeChgTCVOD" (
  foldlSent [chgsStart assumpTHCCoT (S "The"), phrase tempC, S "will change over",
  S "course" `S.the_ofThe` S "day, depending" `S.onThe` phrase energy,
  S "received from the sun"]) "Temperature-Coil-Variable-Over-Day" likeChgDom
--
likeChgTCVOL = cic "likeChgTCVOL" (
  foldlSent [chgsStart assumpTHCCoL (S "The"), phrase tempC,
  S "will actually change along its length as the", phrase water,
  S "within it cools"] ) "Temperature-Coil-Variable-Over-Length" likeChgDom
--
likeChgDT = cic "likeChgDT" (
  foldlSent [chgsStart assumpCTNOD (S "The"), phrase model, S "currently only",
  S "accounts" `S.for` S "charging" `S.ofThe` S "tank. A more complete", phrase model,
  S "would also account" `S.for` S "discharging" `S.ofThe` S "tank"] ) "Discharging-Tank" likeChgDom
--
likeChgDITPW = cic "likeChgDITPW" (
  foldlSent [chgsStart assumpSITWP (S "To add more flexibility to the"),
  phrase simulation `sC` D.toSent (phraseNP (tempInit `the_ofThe` water)) `S.andThe`
  short phsChgMtrl, S "could be allowed to have different",
  plural value] ) "Different-Initial-Temps-PCM-Water" likeChgDom
--
likeChgTLH = cic "likeChgTLH" (
  foldlSent [chgsStart assumpPIT (S "Any real"), phrase tank, S "cannot",
  S "be perfectly insulated and will lose", phrase CT.heat] ) "Tank-Lose-Heat"
  likeChgDom

-- List structure same in all examples.

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [unlikeChgWPFS, unlikeChgNIHG, unlikeChgNGS]

unlikeChgWPFS, unlikeChgNIHG, unlikeChgNGS :: ConceptInstance
unlikeChgWPFS = cic "unlikeChgWPFS" (
  foldlSent [refS assumpWAL `sC` chgsStart assumpNGSP (S "It is unlikely" `S.for` S "the change of"),
  phrase water, S "from liquid to a solid or the state change" `S.ofThe` phrase phsChgMtrl,
  S "from a liquid to a gas to be considered"] ) "Water-PCM-Fixed-States" unlikeChgDom

unlikeChgNIHG = cic "unlikeChgNIHG" (
  foldlSent [chgsStart assumpNIHGBWP (S "Is used" `S.for` S "the derivations of"),
  refS eBalanceOnWtr `S.and_` refS eBalanceOnPCM] )
  "No-Internal-Heat-Generation" unlikeChgDom

unlikeChgNGS = cic "unlikeChgNGS" (
  foldlSent [chgsStart assumpNGSP (S "Is used" `S.for` S "the derivation of"), refS eBalanceOnPCM,
  S "and for the equation given by", refS heatEInPCM, S "to be valid"] )
  "No-Gaseous-State" unlikeChgDom
