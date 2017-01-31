module Drasil.OrganizationOfSRS (refineChain, orgSec, orgSecWTS) where

import Language.Drasil
import Control.Lens ((^.))
import Data.Drasil.Concepts.Documentation

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

--
orgSec :: Concept c => Sentence -> c -> Section -> Section
orgSec = \i b s ->
  Section (S "Organization of Document") (map Con (orgIntro i b s Nothing))

--Do we have extra information at the end (post-refine chain)?
orgSecWTS :: Concept c => Sentence -> c -> Section -> Sentence -> Section
orgSecWTS = \i b s t ->
  Section (S "Organization of Document") (map Con (orgIntro i b s (Just t)))
  
-- Intro -> Bottom (for bottom up approach) -> Section that contains bottom ->
--    trailing sentences -> [Contents]
orgIntro :: Concept c => Sentence -> c -> Section -> Maybe Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence= [ Paragraph $
  intro +:+ S "The presentation follows the standard pattern of presenting" +:+
  (foldl1 sC (map S ["goals", "theories", "definitions"])) `sC` 
  S "and assumptions. For readers that would like a more bottom up approach" `sC`
  S "they can start reading the" +:+ addS (sLower (bottom ^. defn)) +:+ 
  S "in" +:+ (makeRef bottomSec) +:+ S "and trace back to any additional" +:+.
  S "information they require",
  Paragraph $ lastS trailingSentence ]
  where lastS Nothing = refineChain [goalStmt, thModel, inModel]
        lastS (Just t) = lastS Nothing +:+. t