-- | Defines functions used in the Requirements section.
module Drasil.Sections.Requirements (
  -- * Requirements
  ReqType(..), reqF, reqInputsRef,
  -- * Functional Requirements
  fReqF,
  -- ** Input Requirements
  fullReqs, fullTables, inReq, inTable, mkInputPropsTable, mkQRTuple,
  mkQRTupleRef, mkValsSourceTable,
  -- * Non-functional Requirements
  nfReqF
  ) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)

import Data.Drasil.Concepts.Documentation (description, funcReqDom,
  functionalRequirement, input_, nonfunctionalRequirement, output_, reqInput,
  section_, software, symbol_, value)
import Data.Drasil.Concepts.Math (unit_)

import qualified Drasil.DocLang.SRS as SRS
import Drasil.DocumentLanguage.Units (toSentence)

import Control.Lens ((^.))
import Data.Bifunctor (bimap)
import Data.List (nub)

-- | Types of requirements that may be generated.
data ReqType = InputReq Sentence
             | VerifyInputReq
             | CalculateReq
             | VerifyOutputReq
             | OutputReq

instance Eq ReqType where
  InputReq _      == InputReq _      = True
  VerifyInputReq  == VerifyInputReq  = True
  CalculateReq    == CalculateReq    = True
  VerifyOutputReq == VerifyOutputReq = True
  OutputReq       == OutputReq       = True
  _               == _               = False

instance Ord ReqType where
  compare (InputReq _)    (InputReq _)    = EQ
  compare (InputReq _)    _               = LT
  compare VerifyInputReq  VerifyInputReq  = EQ
  compare VerifyInputReq  _               = LT
  compare CalculateReq    CalculateReq    = EQ
  compare CalculateReq    _               = LT
  compare VerifyOutputReq VerifyOutputReq = EQ
  compare VerifyOutputReq _               = LT
  compare OutputReq       OutputReq       = EQ
  compare OutputReq       _               = GT

-- | Wrapper for 'reqIntro'.
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

-- | Prepends an input value requirement and appends an output value
-- requirement to a list of the other requirements.
fullReqs :: (Quantity i, MayHaveUnit i, Quantity j, MayHaveUnit j) =>
  [ReqType] -> [i] -> [(j, Sentence)] -> [ConceptInstance] -> [ConceptInstance]
fullReqs []                _ _ r = r
fullReqs ((InputReq s):rs) i o r = nub $ inReq (inReqDesc (inTable i) s) : fullReqs rs i o r
fullReqs [OutputReq]       _ o r = nub $ r ++ [outReq (outReqDesc (outTable o))]
fullReqs _                 _ _ _ = error "ReqTypes not fully implemented"

-- | Prepends given LabelledContent to an input-value table.
fullTables :: (Quantity i, MayHaveUnit i, Quantity j, MayHaveUnit j) =>
  [ReqType] -> [i] -> [(j, Sentence)] -> [LabelledContent] -> [LabelledContent]
fullTables []                _ _ t = t
fullTables ((InputReq _):rs) i o t = inTable i : fullTables rs i o t
fullTables [OutputReq]       _ o t = t ++ [outTable o]
fullTables _                 _ _ _ = error "ReqTypes not fully implemented"

-- | Creates a generalized input-value table for the Requirements section.
inTable :: (Quantity i, MayHaveUnit i) => [i] -> LabelledContent
inTable i = mkInputPropsTable i (inReq EmptyS) -- passes empty Sentence to make stub of inReq

-- | Creates a generalized output-value table for the Requirements section.
outTable :: (Quantity j, MayHaveUnit j) => [(j, Sentence)] -> LabelledContent
outTable o = mkValsSourceTable o "ReqOutputs" (S "Required" +:+ titleize' output_ `follows` outReq EmptyS)
                                                -- passes empty Sentence to make stub of outReq

-- | Creates a Sentence from a Referable and possible description. Output is of the form
-- "Inputs the values from @reference@, which define @description@". If no description is given,
-- there will be nothing after the word "@reference@".
inReqDesc :: (HasShortName r, Referable r) => r -> Sentence -> Sentence
inReqDesc  t desc = foldlSent [atStart input_,  S "the", plural value, S "from", end]
  where end = case desc of EmptyS -> refS t
                           sent   -> refS t `sC` S "which define" +:+ sent

-- TODO: document
outReqDesc :: (HasShortName r, Referable r) => r -> Sentence
outReqDesc t = foldlSent [atStart output_, S "the", plural value, S "from", refS t]

-- | Creates a 'ConceptInstance' of input values.
inReq, outReq :: Sentence -> ConceptInstance
inReq  s = cic "inputValues"  s "Input-Values"  funcReqDom
outReq s = cic "outputValues" s "Output-Values" funcReqDom

-- | Adds a generalized introduction for the Non-Functional Requirements
-- section. Takes in the contents of that section.
fReqF :: [Contents] -> Section
fReqF listOfFReqs = SRS.funcReq (fReqIntro listOfFReqs : listOfFReqs) []

-- | Adds a generalized introduction for the Non-Functional Requirements
-- section. Takes in the contents of that section.
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

-- | Creates an Input Data Table for use in the Functional Requirements
-- section. Takes a list of wrapped variables and something that is 'Referable'.
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

-- | Creates a table for use in the Functional Requirements section. Takes a
-- list of tuples containing variables and sources, a label, and a caption. 
mkValsSourceTable :: (Quantity j, MayHaveUnit j) =>
                          [(j, Sentence)] -> String -> Sentence -> LabelledContent
mkValsSourceTable vals labl cap = llcc (makeTabRef labl) $
  Table [atStart symbol_, atStart description, S "Source", atStart' unit_]
  (mkTable [ch . fst, atStart . fst, snd, toSentence . fst] $ sortBySymbolTuple vals) cap True

mkQRTuple :: (Quantity i, MayHaveUnit i, HasShortName i, Referable i) => [i] -> [(QuantityDict, Sentence)]
mkQRTuple = map (\c -> (qw c, refS c))

mkQRTupleRef :: (Quantity i, MayHaveUnit i, HasShortName r, Referable r) => [i] -> [r] -> [(QuantityDict, Sentence)]
mkQRTupleRef = zipWith (curry (bimap qw refS))
