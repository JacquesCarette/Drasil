module Drasil.OrganizationOfSRS (refineChain) where

import Language.Drasil
import Control.Lens ((^.))

-- Note: Order matters!
refineChain :: Concept c => [c] -> Sentence
refineChain (x:y:[]) = S "The" +:+ word x +:+ S "are refined to the" +:+ word y
refineChain (x:y:xs) = refineChain [x,y] `sC` rc ([y] ++ xs)
refineChain _ = error "refineChain encountered an unexpected empty list"

word :: Concept c => c -> Sentence
word w = addS (sLower (w ^. defn))

rc :: Concept c => [c] -> Sentence
rc (x:y:[]) = S "and the" +:+ addS (sLower (x ^. defn)) +:+ S "to the" +:+. 
  addS (sLower (y ^. defn))
rc (x:y:xs) = S "the" +:+ word x +:+ S "to the" +:+ word y `sC` rc ([y] ++ xs)
rc _ = error "refineChain helper encountered an unexpected empty list"