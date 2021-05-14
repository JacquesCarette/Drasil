module Drasil.NoPCM.Changes (likelyChgs, unlikelyChgs) where

import Language.Drasil
import Utils.Drasil
import Utils.Drasil.Sentence

import Data.Drasil.Concepts.Documentation (model, likeChgDom, unlikeChgDom)
import Data.Drasil.Concepts.Thermodynamics (temp)

import Drasil.NoPCM.Assumptions (assumpCTNTD, assumpNIHGBW, assumpWAL)
import Drasil.NoPCM.IMods (eBalanceOnWtr)
import Drasil.SWHS.Concepts (water)
--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

likelyChgs :: [ConceptInstance]
likelyChgs = [likeChgDT]

likeChgDT :: ConceptInstance
likeChgDT = cic "likeChgDT" (
  foldlSent [chgsStart assumpCTNTD (S "The"), phrase model,
  S "currently only accounts for charging of the tank. That is, increasing the",
  phrase temp, S "of the water to match the", phrase temp +:+. S "of the coil",
  S "A more complete", phrase model, S "would also account for discharging of the tank"]) 
  "Discharging-Tank" likeChgDom


unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [unlikeChgWFS, unlikeChgNIHG]

unlikeChgWFS :: ConceptInstance
unlikeChgWFS = cic "unlikeChgWFS" (
  foldlSent [chgsStart assumpWAL (S "It is unlikely for the change of"),
  phrase water, S "from liquid to a solid, or from liquid to gas to be considered"])
  "Water-Fixed-States" unlikeChgDom

unlikeChgNIHG :: ConceptInstance
unlikeChgNIHG = cic "unlikeChgNIHG" (
  foldlSent [chgsStart assumpNIHGBW (S "Is used for the derivations of"),
  makeRef2S eBalanceOnWtr] ) "No-Internal-Heat-Generation" unlikeChgDom
