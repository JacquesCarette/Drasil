module Drasil.SWHS.Changes where

import Language.Drasil
import Drasil.DocLang (mkLklyChnk, mkUnLklyChnk)
import qualified Drasil.DocLang.SRS as SRS (unlikeChg)

import Data.Drasil.Concepts.Documentation (assumption, value, simulation,
  model)

import Drasil.SWHS.Concepts (tank, phsChgMtrl, water)
import Drasil.SWHS.Unitals (temp_init, temp_C, temp_PCM)
import Drasil.SWHS.Assumptions (newA4, newA8, newA9, newA11,
  newA12, newA14, newA15, newA16, newA18)

import Data.Drasil.Concepts.Thermodynamics as CT (heat,
  thermal_conductor)
import Data.Drasil.Quantities.Physics (energy)

import Data.Drasil.SentenceStructures (foldlSent, sAnd, ofThe)

--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

chgsStart :: (Referable l, HasShortName l) => l -> Sentence
chgsStart a = makeRef a +:+ S "-"

likeChg1, likeChg2, likeChg3, likeChg4, likeChg5, likeChg6 :: Contents

likeChg1 = mkLklyChnk "likeChg1" (
  foldlSent [chgsStart newA4, short phsChgMtrl, S "is actually a poor", 
  phrase CT.thermal_conductor `sC` S "so the", phrase assumption, 
  S "of uniform", phrase temp_PCM, S "is not likely"] ) "Uniform-Temperature-PCM"
--
likeChg2 = mkLklyChnk "likeChg2" (
  foldlSent [chgsStart newA8, S "The", phrase temp_C, S "will change over", 
  (S "course" `ofThe` S "day, depending"), S "on the", phrase energy, 
  S "received from the sun"] ) "Temperature-Coil-Variable-Over-Day"
--
likeChg3 = mkLklyChnk "likeChg3" (
  foldlSent [chgsStart newA9, S "The", phrase temp_C,
  S "will actually change along its length as the", phrase water,
  S "within it cools"] ) "Temperature-Coil-Variable-Over-Length"
--
likeChg4 = mkLklyChnk "likeChg4" (
  foldlSent [chgsStart newA11, S "The", phrase model, S "currently only", 
  S "accounts for charging of the tank. A more complete", phrase model, 
  S "would also account for discharging of the tank"] ) "Discharging-Tank"
--
likeChg5 = mkLklyChnk "likeChg5" (
  foldlSent [chgsStart newA12, S "To add more flexibility to the", 
  phrase simulation `sC` (phrase temp_init `ofThe` phrase water) `sAnd`
  S "the", short phsChgMtrl, S "could be allowed to have different", 
  plural value] ) "Different-Initial-Temps-PCM-Water"
--
likeChg6 = mkLklyChnk "likeChg6" (
  foldlSent [chgsStart newA15, S "Any real", phrase tank, S "cannot", 
  S "be perfectly insulated and will lose", phrase CT.heat] ) "Tank-Lose-Heat"

-- List structure same in all examples.

--add referencing to assumptions?

unlikelyChgs :: Section
unlikelyChgs = SRS.unlikeChg unlikelyChgsList []

unlikelyChgsList :: [Contents]
unlikelyChgsList = unlikeChgList

unlikeChgList :: [Contents]
unlikeChgList = [unlikeChg1, unlikeChg2]

unlikeChg1, unlikeChg2 :: Contents

unlikeChg1 = mkUnLklyChnk "unlikeChg1" ( 
  foldlSent [makeRef newA14, S ", ", chgsStart newA18, S "It is unlikely for the changeof", 
  phrase water, S "from liquid to a solid or the state change of the", phrase phsChgMtrl, 
  S "from a liquid to a gas to be considered"] ) "Water-PCM-Fixed-States"
--
unlikeChg2 = mkUnLklyChnk "unlikeChg1" (
  foldlSent [chgsStart newA16, S "Is used for the derivations of IM1 and IM2",
  S "(Hack: need Label to fix)"] ) "No-Internal-Heat-Generation"
--
{-
unlikeChg3 = mkUnLklyChnk "unlikeChg3" ( 
  foldlSent [chgsStart newA18, S "Is used for the derivation of IM2 and for the equation",
  S "given by IM4 to be valid", S "(Hack: need Label to fix)"] )
-}
