{-# LANGUAGE PostfixOperators #-}
module Drasil.SWHSNoPCM.Changes (likelyChgs, unlikelyChgs) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (model, likeChgDom, unlikeChgDom)
import Data.Drasil.Concepts.Thermodynamics (temp)

import Drasil.SWHSNoPCM.Assumptions (assumpCTNTD, assumpNIHGBW, assumpWAL)
import Drasil.SWHSNoPCM.IMods (eBalanceOnWtr)
import Drasil.SWHS.Concepts (tank, water)
--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

likelyChgs :: [ConceptInstance]
likelyChgs = [likeChgDT]

likeChgDT :: ConceptInstance
likeChgDT = cic "likeChgDT" (
  foldlSent [chgsStart assumpCTNTD (S "The"), phrase model,
  S "currently only accounts for charging of the tank. That is, increasing the",
  phraseNP (temp `ofThe` water), S "to match the",(phrase temp `S.ofThe` S "coil" !.),
  S "A more complete", phrase model, S "would also account for discharging of", phraseNP (the tank)]) 
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
  refS eBalanceOnWtr] ) "No-Internal-Heat-Generation" unlikeChgDom
