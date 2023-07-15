-- | Defines functions used in the Requirements section.
module Drasil.Sections.Requirements (
  -- * Requirements
  ReqType(..), reqF, reqInputsRef,
  -- * Functional Requirements
  fReqF,
  -- ** Input Requirements
  fullReqs, fullTables, inReq, inTable, mkInputPropsTable, mkQRTuple,
  mkQRTupleRef, mkValsSourceTable, mkTupleValsSourceTable,
  -- * Non-functional Requirements
  nfReqF
  ) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)
import Theory.Drasil (HasOutput(output))

import Data.Drasil.Concepts.Documentation (description, funcReqDom,
  functionalRequirement, input_, nonfunctionalRequirement, output_, reqInput,
  section_, software, symbol_, value)
import Data.Drasil.Concepts.Math (unit_)

import qualified Drasil.DocLang.SRS as SRS
import Drasil.DocumentLanguage.Units (toSentence)

import Control.Lens ((^.))
import Data.Bifunctor (bimap)

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

-- | Where to add a given requirement (before the manually created ones or after)
data ReqLocation = Before | After deriving Eq

reqSection :: ReqLocation -> [(ReqLocation, b)] -> [b]
reqSection l = map snd . filter (\x -> fst x == l)

-- | Creates a Requirement based on its type, along with its location in the list
genReq :: (Quantity i, MayHaveUnit i, HasOutput j, HasShortName j, Referable j)
  => ReqType -> [i] -> [j] -> (ReqLocation, ConceptInstance)
genReq (InputReq _) [] _  = error "Input requirement cannot be generated without inputs"
genReq (InputReq s) i  _  = (Before, inReq (inReqDesc i s))
genReq OutputReq    _  [] = error "Output requirement cannot be generated without outputs"
genReq OutputReq    _  o  = (After, outReq (outReqDesc o))
genReq _            _  _  = error "ReqTypes not fully implemented"


-- | Prepends generated requirements that should come first to the list of the
-- other requirements and appends the rest to it.
fullReqs :: (Quantity i, MayHaveUnit i, HasOutput j, HasShortName j, Referable j)
  => [ReqType] -> [i] -> [j] -> [ConceptInstance] -> [ConceptInstance]
fullReqs r i o c = reqSection Before genReqs ++ c ++ reqSection After genReqs
  where
    genReqs = map (\x -> genReq x i o) r

-- | Creates a table for a given Requirmement type, along with its location in
-- the list
genReqTable :: (Quantity i, MayHaveUnit i, HasOutput j, HasShortName j, Referable j)
  => ReqType -> [i] -> [j] -> (ReqLocation, LabelledContent)
genReqTable (InputReq _) i _ = (Before, inTable i)
genReqTable OutputReq    _ o = (After, outTable o)
genReqTable _            _ _ = error "ReqTypes not fully implemented"

-- | Prepends generated requirement tables that should come first to the list
-- of the other requirement tables and appends the rest to it.
-- This function looks very similar to 'fullReqs', but @samm82 is pretty sure
-- it will change to be different in the future
fullTables :: (Quantity i, MayHaveUnit i, HasOutput j, HasShortName j, Referable j)
  => [ReqType] -> [i] -> [j] -> [LabelledContent] -> [LabelledContent]
fullTables r i o c = reqSection Before genReqs ++ c ++ reqSection After genReqs
  where
    genReqs = map (\x -> genReqTable x i o) r

-- | Creates a generalized input-value table for the Requirements section.
inTable :: (Quantity i, MayHaveUnit i) => [i] -> LabelledContent
inTable i = mkInputPropsTable i (inReq EmptyS) -- passes empty Sentence to make stub of inReq

singleValCaptionHelper :: NamedIdea n => [a] -> n -> Sentence
singleValCaptionHelper [_] = titleize
singleValCaptionHelper _   = titleize'

-- | Creates a generalized output-value table for the Requirements section.
outTable :: (HasOutput j, HasShortName j, Referable j)
  => [j] -> LabelledContent
outTable o = mkValsSourceTable o "ReqOutputs" (S "Required" +:+ singleValCaptionHelper o output_ `follows` outReq EmptyS)
                                                -- passes empty Sentence to make stub of outReq

singleValHelper :: NamedIdea n => [a] -> n -> Sentence
singleValHelper [_] = phrase
singleValHelper _   = plural

-- | Creates a Sentence from a Referable and possible description. Output is of the form
-- "Inputs the values from @reference@, which define @description@." If no description is given,
-- there will be nothing after the word "@reference@".
inReqDesc :: (Quantity i, MayHaveUnit i) => [i] -> Sentence -> Sentence
inReqDesc i desc = foldlSent [atStart input_,  S "the", singleValHelper i value, S "from", end]
  where
    t = inTable i
    end = case desc of EmptyS -> refS t
                       sent   -> refS t `sC` S "which define" +:+ sent

-- | Creates a Sentence from a Referable. Output is of the form "Outputs the
-- values from @reference@."
outReqDesc :: (HasOutput j, HasShortName j, Referable j)
  => [j] -> Sentence
outReqDesc o = foldlSent [atStart output_, S "the", singleValHelper o value,
  S "from", refS (outTable o)]

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
mkInputPropsTable [] _   = llcc reqInputsRef $ Paragraph EmptyS
mkInputPropsTable i  req = llcc reqInputsRef $
  Table [atStart symbol_, atStart description, atStart' unit_]
  (mkTable [ch, atStart, toSentence] $ sortBySymbol i)
  (singleValCaptionHelper i reqInput `follows` req) True

-- | Reference for the Required Inputs table.
reqInputsRef :: Reference
reqInputsRef = makeTabRef' (reqInput ^. uid)

-- | Creates a table for use in the Functional Requirments section. Takes a
-- list of tuples containing variables and sources, a label, and a caption. 
mkTupleValsSourceTable :: (Quantity i, MayHaveUnit i) => 
                          [(i, Sentence)] -> String -> Sentence -> LabelledContent
mkTupleValsSourceTable vals labl cap = llcc (makeTabRef labl) $ 
  Table [atStart symbol_, atStart description, S "Source", atStart' unit_]
  (mkTable [ch . fst, atStart . fst, snd, toSentence . fst] $ sortBySymbolTuple vals) cap True

-- | Creates a table for use in the Functional Requirements section. Takes a
-- list of values with quantities and sources, a label, and a caption. 
mkValsSourceTable :: (HasOutput j, HasShortName j, Referable j) =>
                          [j] -> String -> Sentence -> LabelledContent
mkValsSourceTable vals = mkTupleValsSourceTable (mkQRTuple vals)

-- | Pulls out the 'QuantityDict' and reference 'Sentence' into a tuple for
-- each item in a list with both.
mkQRTuple :: (HasOutput i, HasShortName i, Referable i) => [i] -> [(QuantityDict, Sentence)]
mkQRTuple = map (\c -> (c ^. output, refS c))

-- | Zips a list of items with 'QuantityDict's with a list of items with
-- reference 'Sentence's.
mkQRTupleRef :: (Quantity i, MayHaveUnit i, HasShortName r, Referable r) => [i]
  -> [r] -> [(QuantityDict, Sentence)]
mkQRTupleRef = zipWith (curry (bimap qw refS))
