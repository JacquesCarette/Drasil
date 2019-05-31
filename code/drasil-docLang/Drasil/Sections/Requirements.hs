module Drasil.Sections.Requirements (fReqF, mkInputPropsTable, reqF, reqIntro, nfReqF) where

import Language.Drasil
import Language.Drasil.Utils (sortBySymbol)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (description, functionalRequirement,
  input_, nonfunctionalRequirement, section_, software, symbol_)
import Data.Drasil.Concepts.Math (unit_)
import Data.Drasil.SentenceStructures (foldlSent_, follows)

import qualified Drasil.DocLang.SRS as SRS
import Drasil.DocumentLanguage.Units (toSentence)

-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

fReqF :: [Contents] -> Section
fReqF listOfFReqs = SRS.funcReq (fReqIntro : listOfFReqs) []

nfReqF :: [Contents] -> Section
nfReqF nfrs = SRS.nonfuncReq (nfReqIntro : nfrs) []

--helpers for requirements intros
reqIntroStart :: Sentence
reqIntroStart = foldlSent_ [S "This", phrase section_, S "provides"]

frReqIntroBody :: Sentence
frReqIntroBody = foldlSent_
        [S "the", plural functionalRequirement `sC` S "the tasks and behaviours that the",
        phrase software, S "is expected to complete"]

nfrReqIntroBody :: Sentence
nfrReqIntroBody = foldlSent_
        [S "the", plural nonfunctionalRequirement `sC` S "the qualities that the",
        phrase software, S "is expected to exhibit"]

--generalized requirements introduction
reqIntroS :: Sentence
reqIntroS = reqIntroStart +:+. (frReqIntroBody `sC` EmptyS `sAnd` nfrReqIntroBody)

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

-- | takes a list of wrapped variables and creates an Input Data Table for uses in Functional Requirments
mkInputPropsTable :: (Quantity i, MayHaveUnit i, HasShortName r, Referable r) => 
                          [i] -> r -> LabelledContent
mkInputPropsTable reqInputs req = llcc (makeTabRef "InputPropsReqInputs") $ 
  Table [at_start symbol_, at_start description, at_start' unit_]
  (mkTable [ch, at_start, toSentence] $ sortBySymbol reqInputs)
  (S "Required" +:+ titleize' input_ `follows` req) True
