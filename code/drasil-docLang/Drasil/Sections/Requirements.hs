module Drasil.Sections.Requirements
  (fReqF, reqF, nonFuncReqF, nonFuncReqF') where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (priority, software, nonfunctionalRequirement,
  functionalRequirement, section_)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.SentenceStructures (foldlList, foldlSent, SepType(Comma), FoldType(List))

import qualified Drasil.DocLang.SRS as SRS

-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

fReqF :: [Contents] -> Section
fReqF listOfReqs = SRS.funcReq listOfReqs []

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

-- wrapper for nonfuncReq
nonFuncReqF :: (Concept c) => [c] -> [c] -> Sentence -> Sentence -> Section
nonFuncReqF noPriority priority_ reason_ explanation_ = SRS.nonfuncReq
  [nonFuncReq (map phrase noPriority) (map phrase priority_) reason_ explanation_] []

nonFuncReqF' :: (Concept c) => [c] -> [Contents] -> Sentence -> Sentence -> Section
nonFuncReqF' noPriority nfrs reason_ explanation_ = SRS.nonfuncReq
  ((nonFuncReq' (map phrase noPriority) nfrs reason_ explanation_) : nfrs) []
        
-- generalized non-functional requirements paragraph: list of non-priority requirements, list of priority requirements,
-- reason for initial priority choice, explanation for how priority choice can be achieved.
nonFuncReq :: [Sentence] -> [Sentence] -> Sentence -> Sentence -> Contents
nonFuncReq noPriority priority_ reason_ explanation_ = mkParagraph $ reason_ `sC` (listO explanation_ noPriority priority_)

nonFuncReq' :: [Sentence] -> [Contents] -> Sentence -> Sentence -> Contents
nonFuncReq' noPriority priority_ reason_ explanation_ = mkParagraph $ reason_ `sC` (listO' explanation_ noPriority priority_)

listO :: Sentence -> [Sentence] -> [Sentence] -> Sentence
listO explanation_ [] [] = S "so there are no" +:+ (plural priority) +:+ explanation_
listO explanation_ [] priority_ = S "so" +:+ head priority_ +:+ S "is a high" +:+. (phrase priority) +:+ explanation_ +:+ S "The other" +:+. listT (tail priority_)
listO explanation_ [s] priority_ = S "so" +:+ s +:+ S "is not a" +:+. phrase priority +:+ explanation_ +:+ S "Rather than" +:+ s `sC` S "the" +:+. listT priority_
listO explanation_ s priority_ = S "so" +:+ foldlList Comma List s +:+ S "are not" +:+. (plural priority) +:+ explanation_ +:+ S "Rather, the" +:+. listT priority_

listO' :: Sentence -> [Sentence] -> [Contents] -> Sentence
listO' explanation_ [] [] = S "so there are no" +:+ (plural priority) +:+ explanation_
listO' explanation_ []  priority_ = S "so all" +:+ (plural nonfunctionalRequirement) +:+ S "are given equal" +:+ phrase priority +:+ explanation_ +:+ S "The" +:+. listT' priority_
listO' explanation_ [s] priority_ = S "so" +:+ s +:+ S "is not a" +:+. phrase priority +:+ explanation_ +:+ S "Rather than" +:+ s `sC` S "the" +:+. listT' priority_
listO' explanation_ s priority_ = S "so" +:+ foldlList Comma List s +:+ S "are not" +:+. (plural priority) +:+ explanation_ +:+ S "Rather, the" +:+. listT' priority_

listT :: [Sentence] -> Sentence
listT [] = (phrase program) +:+ S "does not possess a" +:+ (phrase priority) +:+ (phrase nonfunctionalRequirement)
listT [s] = (phrase nonfunctionalRequirement) +:+ (phrase priority) +:+ S "is" +:+ s
listT s = (phrase nonfunctionalRequirement) +:+ (plural priority) +:+ S "are" +:+ foldlList Comma List s

listT' :: [Contents] -> Sentence
listT' [] = (phrase program) +:+ S "does not possess a" +:+ (phrase priority) +:+ (phrase nonfunctionalRequirement)
listT' [_] = (phrase nonfunctionalRequirement) +:+ (phrase priority) +:+ S "is:"
listT' _ = (phrase nonfunctionalRequirement) +:+ (plural priority) +:+ S "are:"
