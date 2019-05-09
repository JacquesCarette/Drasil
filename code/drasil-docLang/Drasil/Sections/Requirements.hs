module Drasil.Sections.Requirements
  (fReqF, reqF, nfReqF, nonFuncReqF) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (priority, software, nonfunctionalRequirement,
  functionalRequirement, section_)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.SentenceStructures (foldlList, foldlSent_, sAnd, SepType(Comma), FoldType(List))

import qualified Drasil.DocLang.SRS as SRS

-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

fReqF :: [Contents] -> Section
fReqF listOfFReqs = SRS.funcReq (fReqIntro : listOfFReqs) []

nfReqF :: (Concept c) => [c] -> Int -> Sentence -> Sentence -> [Contents] -> Section
nfReqF no num r e nfrs = SRS.nonfuncReq
  (nfReqIntro : ((nonFuncReq' (map phrase no) num r e) : nfrs)) []

--helpers for requirements intros
reqIntroStart :: Sentence
reqIntroStart = foldlSent_ [S "This", (phrase section_), S "provides"]

frReqIntroBody :: Sentence
frReqIntroBody = foldlSent_
        [S "the", (plural functionalRequirement) `sC` S "the tasks and behaviours that the",
        (phrase software), S "is expected to complete"]

nfrReqIntroBody :: Sentence
nfrReqIntroBody = foldlSent_
        [S "the", (plural nonfunctionalRequirement) `sC` S "the qualities that the",
        (phrase software), S "is expected to exhibit"]

--generalized requirements introduction
reqIntroS :: Sentence
reqIntroS = reqIntroStart +:+. ((frReqIntroBody :+: S ",") `sAnd` nfrReqIntroBody) -- FIXME: comma hack?

reqIntro :: Contents
reqIntro = mkParagraph reqIntroS

--generalized functional requirements introduction
fReqIntroS :: Sentence
fReqIntroS = reqIntroStart +:+. frReqIntroBody

nfReqIntro :: Contents
nfReqIntro = mkParagraph nfReqIntroS

--generalized nonfunctional requirements introduction
nfReqIntroS :: Sentence
nfReqIntroS = reqIntroStart +:+. nfrReqIntroBody

fReqIntro :: Contents
fReqIntro = mkParagraph fReqIntroS

-- wrapper for nonfuncReq
nonFuncReqF :: (Concept c) => [c] -> [c] -> Sentence -> Sentence -> Section
nonFuncReqF noPriority priority_ reason_ explanation_ = SRS.nonfuncReq
  [nonFuncReq (map phrase noPriority) (map phrase priority_) reason_ explanation_] []
        
-- generalized non-functional requirements paragraph: list of non-priority requirements, list of priority requirements,
-- reason for initial priority choice, explanation for how priority choice can be achieved.
nonFuncReq :: [Sentence] -> [Sentence] -> Sentence -> Sentence -> Contents
nonFuncReq noPriority priority_ reason_ explanation_ = mkParagraph $ reason_ `sC` (listO explanation_ noPriority priority_)

nonFuncReq' :: [Sentence] -> Int -> Sentence -> Sentence -> Contents
nonFuncReq' noPriority num reason_ explanation_ = mkParagraph $ reason_ `sC` (listO' explanation_ noPriority num)

listO :: Sentence -> [Sentence] -> [Sentence] -> Sentence
listO explanation_ [] [] = S "so there are no" +:+ (plural priority) +:+ explanation_
listO explanation_ [] priority_ = S "so" +:+ head priority_ +:+ S "is a high" +:+. (phrase priority) +:+ explanation_ +:+ S "The other" +:+. listT (tail priority_)
listO explanation_ [s] priority_ = S "so" +:+ s +:+ S "is not a" +:+. phrase priority +:+ explanation_ +:+ S "Rather than" +:+ s `sC` S "the" +:+. listT priority_
listO explanation_ s priority_ = S "so" +:+ foldlList Comma List s +:+ S "are not" +:+. (plural priority) +:+ explanation_ +:+ S "Rather, the" +:+. listT priority_

listO' :: Sentence -> [Sentence] -> Int -> Sentence
listO' explanation_ [] 0 = S "so there are no" +:+ (plural priority) +:+ explanation_
listO' explanation_ []  num = S "so all" +:+ (plural nonfunctionalRequirement) +:+ S "are given equal" +:+ phrase priority +:+ explanation_ +:+ S "The" +:+ listT' num
listO' explanation_ [s] num = S "so" +:+ s +:+ S "is not a" +:+. phrase priority +:+ explanation_ +:+ S "Rather than" +:+ s `sC` S "the" +:+ listT' num
listO' explanation_ s num = S "so" +:+ foldlList Comma List s +:+ S "are not" +:+. (plural priority) +:+ explanation_ +:+ S "Rather, the" +:+ listT' num

listT :: [Sentence] -> Sentence
listT [] = (phrase program) +:+ S "does not possess a" +:+ (phrase priority) +:+ (phrase nonfunctionalRequirement)
listT [s] = (phrase nonfunctionalRequirement) +:+ (phrase priority) +:+ S "is" +:+ s
listT s = (phrase nonfunctionalRequirement) +:+ (plural priority) +:+ S "are" +:+ foldlList Comma List s

listT' :: Int -> Sentence
listT' 0 = (phrase program) +:+ S "does not possess a" +:+ (phrase priority) +:+. (phrase nonfunctionalRequirement)
listT' 1 = (phrase nonfunctionalRequirement) +:+ (phrase priority) +: S "is"
listT' _ = (phrase nonfunctionalRequirement) +:+ (plural priority) +: S "are"
