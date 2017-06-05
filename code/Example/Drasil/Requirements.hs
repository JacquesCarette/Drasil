module Drasil.Requirements
  (reqF, nonFuncReqF) where

import Language.Drasil
import Control.Lens ((^.))
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.SentenceStructures
import qualified Drasil.SRS as SRS

-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

--generalized requirements introduction
reqIntro :: Contents
reqIntro = Paragraph $ foldlSent
        [S "This", (phrase $ section_ ^. term), S "provides the",
        (plural $ functionalRequirement ^. term) `sC` S "the business tasks that the",
        (phrase $ software ^. term), S "is expected to complete, and the", 
        (plural $ nonfunctionalRequirement ^. term) `sC` S "the qualities that the",
        (phrase $ software ^. term), S "is expected to exhibit"]

-- wrapper for nonfuncReq
nonFuncReqF :: (Concept c) => [c] -> [c] -> Sentence -> Sentence -> Section
nonFuncReqF noPriority priority_ reason_ explanation_ = SRS.nonfuncReq
  [nonFuncReq (map (\x -> phrase $ x ^. term) noPriority) (map (\x -> phrase $ x ^. term) priority_) reason_ explanation_] []
        
-- generalized non-functional requirements paragraph: list of non-priority requirements, list of priority requirements,
-- reason for initial priority choice, explanation for how priority choice can be achieved.
nonFuncReq :: [Sentence] -> [Sentence] -> Sentence -> Sentence -> Contents
nonFuncReq noPriority priority_ reason_ explanation_ = Paragraph $ reason_ `sC` (listO explanation_ noPriority priority_)

listO :: Sentence -> [Sentence] -> [Sentence] -> Sentence
listO explanation_ [] [] = S "so there are no" +:+ (plural $ priority ^. term) +:+ explanation_
listO explanation_ [] priority_ = S "so" +:+ head priority_ +:+ S "is a high" +:+. (phrase $ priority ^. term) +:+ explanation_ +:+ S "The other" +:+. listT (tail priority_)
listO explanation_ [s] priority_ = S "so" +:+ s +:+ S "is not a" +:+. (phrase $ priority ^. term) +:+ explanation_ +:+ S "Rather than" +:+ s `sC` S "the" +:+. listT priority_
listO explanation_ s priority_ = S "so" +:+ foldlList s +:+ S "are not" +:+. (plural $ priority ^. term) +:+ explanation_ +:+ S "Rather, the" +:+. listT priority_
listT :: [Sentence] -> Sentence
listT [] = (phrase $ program ^. term) +:+ S "does not possess a" +:+ (phrase $ priority ^. term) +:+ (phrase $ nonfunctionalRequirement ^. term)
listT [s] = (phrase $ nonfunctionalRequirement ^. term) +:+ (phrase $ priority ^. term) +:+ S "is" +:+ s
listT s = (phrase $ nonfunctionalRequirement ^. term) +:+ (phrase $ priority ^. term) +:+ S "are" +:+ foldlList s