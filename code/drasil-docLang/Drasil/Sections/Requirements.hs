module Drasil.Sections.Requirements
  (fReqF, reqF, nonFuncReqF, funcReqDom) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (priority, software, requirement, nonfunctionalRequirement,
  functionalRequirement, section_)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.SentenceStructures (foldlSP, foldlList, foldlSent, SepType(Comma), FoldType(List))

import qualified Drasil.DocLang.SRS as SRS

import Control.Lens ((^.))

-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

fReqF :: [Contents] -> Section
fReqF listOfReqs = SRS.funcReq (listOfReqs) []

--generalized requirements introduction
reqIntroS :: Sentence
reqIntroS = foldlSent
        [S "This", (phrase section_), S "provides the",
        (plural functionalRequirement) `sC` S "the business tasks that the",
        (phrase software), S "is expected to complete" `sC` S "and the", 
        (plural nonfunctionalRequirement) `sC` S "the qualities that the",
        (phrase software), S "is expected to exhibit"]

reqIntro :: Contents
reqIntro = mkParagraph reqIntroS

-- Requirements Domains
reqDom :: ConceptChunk
reqDom = ccs (nc "reqDom" $ requirement ^. term) reqIntroS [SRS.srsDom]

funcReqDom :: ConceptChunk
funcReqDom = ccs (nc "funcReqDom" $ functionalRequirement ^. term) EmptyS [reqDom]

-- wrapper for nonfuncReq
nonFuncReqF :: (Concept c) => [c] -> [c] -> Sentence -> Sentence -> Section
nonFuncReqF noPriority priority_ reason_ explanation_ = SRS.nonfuncReq
  [nonFuncReq (map phrase noPriority) (map phrase priority_) reason_ explanation_] []
        
-- generalized non-functional requirements paragraph: list of non-priority requirements, list of priority requirements,
-- reason for initial priority choice, explanation for how priority choice can be achieved.
nonFuncReq :: [Sentence] -> [Sentence] -> Sentence -> Sentence -> Contents
nonFuncReq noPriority priority_ reason_ explanation_ = mkParagraph $ reason_ `sC` (listO explanation_ noPriority priority_)

listO :: Sentence -> [Sentence] -> [Sentence] -> Sentence
listO explanation_ [] [] = S "so there are no" +:+ (plural priority) +:+ explanation_
listO explanation_ [] priority_ = S "so" +:+ head priority_ +:+ S "is a high" +:+. (phrase priority) +:+ explanation_ +:+ S "The other" +:+. listT (tail priority_)
listO explanation_ [s] priority_ = S "so" +:+ s +:+ S "is not a" +:+. phrase priority +:+ explanation_ +:+ S "Rather than" +:+ s `sC` S "the" +:+. listT priority_
listO explanation_ s priority_ = S "so" +:+ foldlList Comma List s +:+ S "are not" +:+. (plural priority) +:+ explanation_ +:+ S "Rather, the" +:+. listT priority_

listT :: [Sentence] -> Sentence
listT [] = (phrase program) +:+ S "does not possess a" +:+ (phrase priority) +:+ (phrase nonfunctionalRequirement)
listT [s] = (phrase nonfunctionalRequirement) +:+ (phrase priority) +:+ S "is" +:+ s
listT s = (phrase nonfunctionalRequirement) +:+ (plural priority) +:+ S "are" +:+ foldlList Comma List s
