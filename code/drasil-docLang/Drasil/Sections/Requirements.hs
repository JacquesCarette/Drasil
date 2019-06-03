module Drasil.Sections.Requirements (fReqF, mkInputPropsTable, mkQRTuple,
  mkQRTupleRef, mkValsSourceTable, nfReqF, reqF) where

import Language.Drasil
import Language.Drasil.Utils (sortBySymbol, sortBySymbolTuple)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (description, functionalRequirement,
  input_, nonfunctionalRequirement, section_, software, symbol_)
import Data.Drasil.Concepts.Math (unit_)
import Data.Drasil.SentenceStructures (follows)

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
reqIntro :: Contents
reqIntro = mkParagraph $ reqIntroStart +:+. (frReqIntroBody `sC` EmptyS `sAnd` nfrReqIntroBody)

--generalized functional requirements introduction
fReqIntro :: Contents
fReqIntro = mkParagraph $ reqIntroStart +:+. frReqIntroBody

--generalized nonfunctional requirements introduction
nfReqIntro :: Contents
nfReqIntro = mkParagraph $ reqIntroStart +:+. nfrReqIntroBody

-- | takes a list of wrapped variables and creates an Input Data Table for uses in Functional Requirments
mkInputPropsTable :: (Quantity i, MayHaveUnit i, HasShortName r, Referable r) => 
                          [i] -> r -> LabelledContent
mkInputPropsTable reqInputs req = llcc (makeTabRef "ReqInputs") $ 
  Table [atStart symbol_, atStart description, atStart' unit_]
  (mkTable [ch, atStart, toSentence] $ sortBySymbol reqInputs)
  (S "Required" +:+ titleize' input_ `follows` req) True

-- | takes a list of tuples of variables and sources and creates an table for uses in Functional Requirments
mkValsSourceTable :: (Quantity i, MayHaveUnit i) => 
                          [(i, Sentence)] -> String -> Sentence -> LabelledContent
mkValsSourceTable vals label cap = llcc (makeTabRef label) $ 
  Table [atStart symbol_, atStart description, S "Source", atStart' unit_]
  (mkTable [ch . fst, atStart . fst, snd, toSentence . fst] $ sortBySymbolTuple vals) cap True

mkQRTuple :: (Quantity i, MayHaveUnit i, HasShortName i, Referable i) => [i] -> [(QuantityDict, Sentence)]
mkQRTuple = map (\c -> (qw c, makeRef2S c))

mkQRTupleRef :: (Quantity i, MayHaveUnit i, HasShortName r, Referable r) => [i] -> [r] -> [(QuantityDict, Sentence)]
mkQRTupleRef qs rs = map (\(c, r) -> (qw c, makeRef2S r)) $ zip qs rs
