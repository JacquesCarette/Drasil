{-# Language GADTs, Rank2Types #-}

module Drasil.DocumentLanguage.RefHelpers
  ( refA, refR, refChng, cite
  , refAByNum, refRByNum, refChngByNum, citeByNum
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
refA rfdb = refACustom rfdb ByName
refAByNum rfdb = refACustom rfdb ByNum

-- | Reference Assumptions by Name or by Number where applicable
refACustom :: ReferenceDB -> RefBy -> AssumpChunk -> Sentence
refACustom rfdb ByNum  a = customRef a (short assumption :+:
  numLookup rfdb assumpRefTable assumpLookup a)
refACustom rfdb ByName a =
  makeRef (chunkLookup rfdb assumpRefTable assumpLookup a)

-- | Smart constructors for requirement referencing by name or by number.
refR, refRByNum :: ReferenceDB -> ReqChunk -> Sentence
refR rfdb = refRCustom rfdb ByName
refRByNum rfdb = refRCustom rfdb ByNum

-- | Reference Requirements by Name or by Number where applicable
refRCustom :: ReferenceDB -> RefBy -> ReqChunk -> Sentence
refRCustom rfdb ByNum  r = customRef r (S (show (reqType r)) :+:
  numLookup rfdb reqRefTable reqLookup r)
refRCustom rfdb ByName r = makeRef (chunkLookup rfdb reqRefTable reqLookup r)

-- | Smart constructors for likely/unlikely change referencing by name or by number.
refChng, refChngByNum :: ReferenceDB -> Change -> Sentence
refChng rfdb = refChngCustom rfdb ByName
refChngByNum rfdb = refChngCustom rfdb ByNum

-- | Reference Changes by Name or by Number where applicable
refChngCustom :: ReferenceDB -> RefBy -> Change -> Sentence
refChngCustom chdb ByNum  c = customRef c (S (show (chngType c)) :+:
  numLookup chdb changeRefTable changeLookup c)
refChngCustom chdb ByName c =
  makeRef (chunkLookup chdb changeRefTable changeLookup c)

-- | Smart constructors for citation referencing by name or by number.
cite, citeByNum :: ReferenceDB -> Citation -> Sentence
cite rfdb = citeCustom rfdb ByName
citeByNum rfdb = citeCustom rfdb ByNum

-- | Reference Changes by Name or by Number where applicable
citeCustom :: ReferenceDB -> RefBy -> Citation -> Sentence
citeCustom rfdb ByNum  c = customRef c
  (S "[" :+: numLookup rfdb citationRefTable citeLookup c :+: S "]")
citeCustom rfdb ByName c =
  makeRef (chunkLookup rfdb citationRefTable citeLookup c)
