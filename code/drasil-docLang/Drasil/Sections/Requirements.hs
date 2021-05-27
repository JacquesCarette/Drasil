module Drasil.Sections.Requirements (fReqF, fullReqs, fullTables, inReq, inTable,
  mkInputPropsTable, mkQRTuple, mkQRTupleRef, mkValsSourceTable, nfReqF, reqF) where

import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (description, funcReqDom,
  functionalRequirement, input_, nonfunctionalRequirement, {-output_,-} section_,
  software, symbol_, value)
import Data.Drasil.Concepts.Math (unit_)

import qualified Drasil.DocLang.SRS as SRS
import Drasil.DocumentLanguage.Units (toSentence)

import Data.Bifunctor (bimap)

-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

fullReqs :: (Quantity i, MayHaveUnit i) => [i] -> Sentence -> [ConceptInstance] -> [ConceptInstance]
fullReqs i d r = inReq (inReqDesc (inTable i) d) : r-- ++ [outReq (outReqDesc outTable)]

fullTables :: (Quantity i, MayHaveUnit i) => [i] -> [LabelledContent] -> [LabelledContent]
fullTables i t = inTable i : t

inTable :: (Quantity i, MayHaveUnit i) => [i] -> LabelledContent
inTable i = mkInputPropsTable i (inReq EmptyS) -- passes empty Sentence to make stub of inReq
--outTable    = mkValsSourceTable o "ReqOutputs" (S "Required" +:+ titleize' output_ `follows` (outReq EmptyS))
                                                -- passes empty Sentence to make stub of outReq

inReqDesc :: (HasShortName r, Referable r) => r -> Sentence -> Sentence 
inReqDesc  t desc = foldlSent [atStart input_,  S "the", plural value, S "from", end]
  where end = case desc of EmptyS -> makeRef2S t
                           sent   -> makeRef2S t `sC` S "which define" +:+ sent
--outReqDesc t = foldlSent [atStart output_, S "the", plural value, S "from", makeRef2S t]

inReq :: Sentence -> ConceptInstance
inReq  s = cic "inputValues"  s "Input-Values"  funcReqDom
--outReq s = cic "inputValues" s "Output-Values" funcReqDom

fReqF :: [Contents] -> Section
fReqF listOfFReqs = SRS.funcReq (fReqIntro : listOfFReqs) []

nfReqF :: [Contents] -> Section
nfReqF nfrs = SRS.nonfuncReq (nfReqIntro : nfrs) []

--helpers for requirements intros
reqIntroStart :: Sentence
reqIntroStart = foldlSent_ [S "This", phrase section_, S "provides"]

frReqIntroBody :: Sentence
frReqIntroBody = foldlSent_ [S "the", plural functionalRequirement `sC`
  S "the tasks and behaviours that the", phrase software, S "is expected to complete"]

nfrReqIntroBody :: Sentence
nfrReqIntroBody = foldlSent_ [S "the", plural nonfunctionalRequirement `sC`
  S "the qualities that the", phrase software, S "is expected to exhibit"]

--generalized requirements introduction
reqIntro :: Contents
reqIntro = mkParagraph $ reqIntroStart +:+. (frReqIntroBody `sC` EmptyS `S.and_` nfrReqIntroBody)

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
mkQRTupleRef = zipWith (curry (bimap qw makeRef2S))
