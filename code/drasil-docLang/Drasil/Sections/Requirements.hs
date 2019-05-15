module Drasil.Sections.Requirements (fReqF, reqF, nfReqF) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (software, nonfunctionalRequirement,
  functionalRequirement, section_)
import Data.Drasil.SentenceStructures (foldlSent_, sAnd)

import qualified Drasil.DocLang.SRS as SRS

-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

fReqF :: [Contents] -> Section
fReqF listOfFReqs = SRS.funcReq (fReqIntro : listOfFReqs) []

nfReqF :: [Contents] -> Section
nfReqF nfrs = SRS.nonfuncReq (nfReqIntro : nfrs) []

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
