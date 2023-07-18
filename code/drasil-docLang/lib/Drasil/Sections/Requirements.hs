-- | Defines functions used in the Requirements section.
module Drasil.Sections.Requirements (
  -- * Requirements
  reqF, reqInputsRef,
  -- * Functional Requirements
  fReqF,
  -- ** Input Requirements
  fullReqs, fullTables, inReq, inTable,
  mkInputPropsTable, mkQRTuple, mkQRTupleRef, mkValsSourceTable,
  -- * Non-functional Requirements
  nfReqF
  ) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.Sections.ReferenceMaterial(emptySectSentPlu)

import Data.Drasil.Concepts.Documentation (description, funcReqDom,
  functionalRequirement, input_, nonfunctionalRequirement, {-output_,-} section_,
  software, symbol_, value, reqInput)
import Data.Drasil.Concepts.Math (unit_)

import qualified Drasil.DocLang.SRS as SRS
import Drasil.DocumentLanguage.Units (toSentence)
import Data.List (nub)

import Control.Lens ((^.))
import Data.Bifunctor (bimap)

-- | Wrapper for 'reqIntro'.
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

-- | Prepends a 'ConceptInstance' referencing an input-value table to a list of other 'ConceptInstance's.
-- For listing input requirements.
fullReqs :: (Quantity i, MayHaveUnit i) => [i] -> Sentence -> [ConceptInstance] -> [ConceptInstance]
fullReqs [] _ _ = []
fullReqs i d r = nub $ inReq (inReqDesc (inTable i) d) : r-- ++ [outReq (outReqDesc outTable)]

-- | Prepends given LabelledContent to an input-value table.
fullTables :: (Quantity i, MayHaveUnit i) => [i] -> [LabelledContent] -> [LabelledContent]
fullTables [] _ = []
fullTables i t = inTable i : t

-- | Creates a generalized input-value table for the Requirements section.
inTable :: (Quantity i, MayHaveUnit i) => [i] -> LabelledContent
inTable i = mkInputPropsTable i (inReq EmptyS) -- passes empty Sentence to make stub of inReq
--outTable    = mkValsSourceTable o "ReqOutputs" (S "Required" +:+ titleize' output_ `follows` (outReq EmptyS))
                                                -- passes empty Sentence to make stub of outReq

-- | Creates a Sentence from a Referable and possible description. Output is of the form
-- "Inputs the values from @reference@, which define @description@". If no description is given,
-- there will be nothing after the word "@reference@".
inReqDesc :: (HasShortName r, Referable r) => r -> Sentence -> Sentence 
inReqDesc  t desc = foldlSent [atStart input_,  S "the", plural value, S "from", end]
  where end = case desc of EmptyS -> refS t
                           sent   -> refS t `sC` S "which define" +:+ sent
--outReqDesc t = foldlSent [atStart output_, S "the", plural value, S "from", refS t]

-- | Creates a 'ConceptInstance' of input values.
inReq :: Sentence -> ConceptInstance
inReq  s = cic "inputValues"  s "Input-Values"  funcReqDom
--outReq s = cic "inputValues" s "Output-Values" funcReqDom

-- | Adds a generalized introduction for a Non-Fucntional Requirements section. Takes in the contents of that section.
fReqF :: [Contents] -> Section
fReqF listOfFReqs = SRS.funcReq (fReqIntro listOfFReqs : listOfFReqs) []

-- | Adds a generalized introduction for a Non-Fucntional Requirements section. Takes in the contents of that section.
nfReqF :: [Contents] -> Section
nfReqF nfrs = SRS.nonfuncReq (nfReqIntro nfrs : nfrs) []

-- | General 'Sentence' for use in the Requirements section introduction.
reqIntroStart :: Sentence
reqIntroStart = foldlSent_ [S "This", phrase section_, S "provides"]

-- | General 'Sentence' for use in the Functional Requirements subsection introduction.
frReqIntroBody :: Sentence
frReqIntroBody = foldlSent_ [pluralNP (the functionalRequirement) `sC`
  S "the tasks and behaviours that the", phrase software, S "is expected to complete"]

-- | General 'Sentence' for use in the Non-Functional Requirements subsection introduction.
nfrReqIntroBody :: Sentence
nfrReqIntroBody = foldlSent_ [pluralNP (the nonfunctionalRequirement) `sC`
  S "the qualities that the", phrase software, S "is expected to exhibit"]

-- | Generalized Requirements section introduction.
reqIntro :: Contents
reqIntro = mkParagraph $ reqIntroStart +:+. (frReqIntroBody `sC` EmptyS `S.and_` nfrReqIntroBody)

-- | Generalized Functional Requirements subsection introduction.
fReqIntro :: [Contents] -> Contents
fReqIntro [] = mkParagraph $ emptySectSentPlu [functionalRequirement]
fReqIntro _  = mkParagraph $ reqIntroStart +:+. frReqIntroBody

-- | Generalized Non-Functional Requirements subsection introduction.
nfReqIntro :: [Contents] -> Contents
nfReqIntro [] = mkParagraph $ emptySectSentPlu [nonfunctionalRequirement]
nfReqIntro _  = mkParagraph $ reqIntroStart +:+. nfrReqIntroBody

-- | Creates an Input Data Table for use in the Functional Requirments section. Takes a list of wrapped variables and something that is 'Referable'.
mkInputPropsTable :: (Quantity i, MayHaveUnit i, HasShortName r, Referable r) => 
                          [i] -> r -> LabelledContent
mkInputPropsTable []        _   = llcc reqInputsRef $ Paragraph EmptyS
mkInputPropsTable reqInputs req = llcc reqInputsRef $ 
  Table [atStart symbol_, atStart description, atStart' unit_]
  (mkTable [ch, atStart, toSentence] $ sortBySymbol reqInputs)
  (titleize' reqInput `follows` req) True

-- | Reference for the Required Inputs table.
reqInputsRef :: Reference
reqInputsRef = makeTabRef' (reqInput ^. uid)

-- | Creates a table for use in the Functional Requirments section. Takes a list of tuples containing variables and sources, a label, and a caption. 
mkValsSourceTable :: (Quantity i, MayHaveUnit i) => 
                          [(i, Sentence)] -> String -> Sentence -> LabelledContent
mkValsSourceTable vals labl cap = llcc (makeTabRef labl) $ 
  Table [atStart symbol_, atStart description, S "Source", atStart' unit_]
  (mkTable [ch . fst, atStart . fst, snd, toSentence . fst] $ sortBySymbolTuple vals) cap True

mkQRTuple :: (Quantity i, MayHaveUnit i, HasShortName i, Referable i) => [i] -> [(QuantityDict, Sentence)]
mkQRTuple = map (\c -> (qw c, refS c))

mkQRTupleRef :: (Quantity i, MayHaveUnit i, HasShortName r, Referable r) => [i] -> [r] -> [(QuantityDict, Sentence)]
mkQRTupleRef = zipWith (curry (bimap qw refS))
