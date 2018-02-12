{-# Language GADTs, Rank2Types #-}

module Drasil.DocumentLanguage.RefHelpers 
  ( refA, refR, refChng
  )where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (assumption)

import Control.Lens ((^.), Simple, Lens)
import Prelude hiding (id)

-- | Internal lookup function for the number associated with an assumption,
-- requirement, etc. and returns it as a Sentence.
-- Takes a reference database, a lens to the appropriate table, a lookup function,
-- and a chunk of the appropriate type to look up. 
-- For example, looking up an assumption number would require:
-- a reference database, the assumpRefTable lens, the assumpLookup function, and
-- the assumption chunk being looked up.
numLookup :: Chunk c => ReferenceDB -> Simple Lens ReferenceDB t -> 
  (c -> t -> (ct, Int)) -> c -> Sentence
numLookup db tableLens lookupFun chunk = 
  S $ show $ snd $ lookupFun chunk (db ^. tableLens)

-- | Reference Assumptions by Number  
refA :: ReferenceDB -> AssumpChunk -> Sentence
refA adb a = Ref (rType a) (refAdd a) (short assumption :+:
  numLookup adb assumpRefTable assumpLookup a)

-- | Reference Requirements by Number
refR :: ReferenceDB -> ReqChunk -> Sentence
refR rdb r = Ref (rType r) (refAdd r) (S (show (reqType r)) :+:
  numLookup rdb reqRefTable reqLookup r)

-- | Reference Changes by Number
refChng :: ReferenceDB -> Change -> Sentence
refChng chdb c = Ref (rType c) (refAdd c) (S (show (chngType c)) :+:
  numLookup chdb changeRefTable changeLookup c)
