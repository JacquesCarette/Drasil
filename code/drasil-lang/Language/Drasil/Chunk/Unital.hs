{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Unital 
  ( UnitalChunk(..)
  , makeUCWDS
  , ucFromDQD
  , uc
  , uc'
  , ucs
  , ucs'
  , ucsWS
  ) where

import Control.Lens (makeLenses, view, (^.))
import Language.Drasil.Chunk.Concept (dcc, dccWDS,cw)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd)
import Language.Drasil.Chunk.Quantity (Quantity(..),HasSpace(typ))
import Language.Drasil.Chunk.Unitary (Unitary(..))
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasSymbol(symbol),
  IsUnit)
import Language.Drasil.Development.Unit (MayHaveUnit(unitOpt), UnitDefn, unitWrapper)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space(..))
import Language.Drasil.Spec (Sentence)

import Language.Drasil.NounPhrase (NP)

-- | UnitalChunks are Unitary
data UnitalChunk = UC { _defq' :: DefinedQuantityDict
                      , _uni :: UnitDefn
                      }
makeLenses ''UnitalChunk

instance HasUID        UnitalChunk where uid = defq' . uid
instance NamedIdea     UnitalChunk where term = defq' . term
instance Idea          UnitalChunk where getA (UC qc _) = getA qc
instance Definition    UnitalChunk where defn = defq' . defn
instance ConceptDomain UnitalChunk where cdom = defq' . cdom
instance Concept       UnitalChunk where
instance HasSpace      UnitalChunk where typ = defq' . typ
instance HasSymbol     UnitalChunk where symbol c st = symbol (c^.defq') st
instance Quantity      UnitalChunk where getUnit defq = Just (unit defq)
instance Unitary       UnitalChunk where unit = view uni
instance MayHaveUnit   UnitalChunk where unitOpt u = Just (unit u)

--{BEGIN HELPER FUNCTIONS}--

-- | Used to create a UnitalChunk from a 'Concept', 'Symbol', and 'Unit'.
-- Assumes the 'Space' is Real
uc :: (Concept c, IsUnit u, ConceptDomain u) =>
  c -> Symbol -> u -> UnitalChunk
uc a b c = ucs' a b c Real

ucs' :: (Concept c, IsUnit u, ConceptDomain u) =>
  c -> Symbol -> u -> Space -> UnitalChunk
ucs' a sym c space = UC (dqd (cw a) sym space (Just un)) un
 where un = unitWrapper c

-- | Same as 'uc', except it builds the Concept portion of the UnitalChunk
-- from a given uid, term, and defn. Those are the first three arguments
uc' :: (IsUnit u, ConceptDomain u) => String -> NP -> String -> Symbol ->
  u -> UnitalChunk
uc' i t d s u = UC (dqd (dcc i t d) s Real (Just un)) un
 where un = unitWrapper u

-- | Same as 'uc'', but does not assume the 'Space'
ucs :: (IsUnit u, ConceptDomain u) => String -> NP ->
  String -> Symbol -> u -> Space -> UnitalChunk
ucs nam trm desc sym un space = UC (dqd (dcc nam trm desc) sym space (Just uu)) uu
  where uu = unitWrapper un

-- ucs With a Sentence for desc
ucsWS :: (IsUnit u, ConceptDomain u) => String -> NP ->
  Sentence -> Symbol -> u -> Space -> UnitalChunk
ucsWS nam trm desc sym un space = UC (dqd (dccWDS nam trm desc) sym space (Just uu)) uu
  where uu = unitWrapper un

--Better names will come later.
-- | Create a UnitalChunk in the same way as 'uc'', but with a 'Sentence' for
-- the definition instead of a String
makeUCWDS :: (IsUnit u, ConceptDomain u) => String -> NP -> Sentence -> Symbol ->
  u -> UnitalChunk
makeUCWDS nam trm desc sym un = UC (dqd (dccWDS nam trm desc) sym Real (Just uu)) uu
  where uu = unitWrapper un

-- | Create a UnitalChunk from a 'DefinedQuantityDict' which has a unit already.
-- fail otherwise
ucFromDQD :: DefinedQuantityDict -> UnitalChunk
ucFromDQD defq = UC defq (unitWrapper uu) 
  where uu = maybe (error "ucFromDQD must be called on a DQD with a unit") id (getUnit defq)
