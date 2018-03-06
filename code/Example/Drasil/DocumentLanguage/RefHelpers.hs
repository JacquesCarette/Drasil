{-# Language GADTs, Rank2Types #-}

module Drasil.DocumentLanguage.RefHelpers
  ( refA, refR, refChng, cite
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

-- | Verifies that a chunk exists within our referencing database before we
-- attempt to make a reference to it.
chunkLookup :: Chunk c => ReferenceDB -> Simple Lens ReferenceDB t ->
  (c -> t -> (ct, Int)) -> c -> ct
chunkLookup db tableLens lookupFun chunk =
  fst $ lookupFun chunk (db ^. tableLens)

-- | Smart constructors for assumption referencing by name or by number.
refA, refAByNum :: ReferenceDB -> AssumpChunk -> Sentence
refA rdb = refACustom rdb ByName
refAByNum rdb = refACustom rdb ByNum

-- | Reference Assumptions by Name or by Number where applicable
refACustom :: ReferenceDB -> RefBy -> AssumpChunk -> Sentence
refACustom adb ByNum a = customRef a (short assumption :+:
  numLookup adb assumpRefTable assumpLookup a)
refACustom adb ByName a = makeRef (chunkLookup adb assumpRefTable assumpLookup a)

-- | Reference Requirements by Name or by Number where applicable
refRCustom :: ReferenceDB -> RefBy -> ReqChunk -> Sentence
refRCustom rdb ByNum r = customRef r (S (show (reqType r)) :+:
  numLookup rdb reqRefTable reqLookup r)

-- | Reference Changes by Name or by Number where applicable
refChngCustom :: ReferenceDB -> RefBy -> Change -> Sentence
refChngCustom chdb ByNum c = customRef c (S (show (chngType c)) :+:
  numLookup chdb changeRefTable changeLookup c)

citeCustom :: ReferenceDB -> RefBy -> Citation -> Sentence
citeCustom _ _ _ = undefined
