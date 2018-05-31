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
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom, DOM), Concept, HasSymbol(symbol),
  IsUnit, HasAttributes(attributes))
import Language.Drasil.Chunk.Concept (ConceptChunk,dcc, dccWDS,cw)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd)
import Language.Drasil.Chunk.Quantity (Quantity(..),HasSpace(typ))
import Language.Drasil.Chunk.Unitary (Unitary(..))
import Language.Drasil.Unit (UnitDefn, unitWrapper)
import Language.Drasil.Symbol
import Language.Drasil.Space
import Language.Drasil.Spec (Sentence)

import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Chunk.Attribute.Core (Attributes)

-- | UnitalChunks are Unitary
data UnitalChunk = UC { _defq' :: DefinedQuantityDict
                      , _uni :: UnitDefn
                      }
makeLenses ''UnitalChunk

instance HasUID        UnitalChunk where uid = defq' . uid
instance NamedIdea     UnitalChunk where term = defq' . term
instance Idea          UnitalChunk where getA (UC qc _) = getA qc
instance Definition    UnitalChunk where defn = defq' . defn
instance ConceptDomain UnitalChunk where
  type DOM UnitalChunk = ConceptChunk
  cdom = defq' . cdom
instance Concept       UnitalChunk where
instance HasSpace      UnitalChunk where typ = defq' . typ
instance HasSymbol     UnitalChunk where symbol c st = symbol (c^.defq') st
instance Quantity      UnitalChunk where getUnit defq = Just (unit defq)
instance Unitary       UnitalChunk where unit = view uni
instance HasAttributes UnitalChunk where attributes = defq' . attributes

--{BEGIN HELPER FUNCTIONS}--

-- | Used to create a UnitalChunk from a 'Concept', 'Symbol', and 'Unit'.
-- Assumes the 'Space' is Real
uc :: (Concept c, IsUnit u, DOM c ~ ConceptChunk, DOM u ~ ConceptChunk) =>
  c -> Symbol -> u -> {-Attributes ->-} UnitalChunk
uc a b c {-atts-} = ucs' a b c Real {-[]atts-}

ucs' :: (Concept c, IsUnit u, DOM c ~ ConceptChunk, DOM u ~ ConceptChunk) =>
  c -> Symbol -> u -> Space -> {-Attributes -> -} UnitalChunk
ucs' a sym c space {-atts-} = UC (dqd (cw a) sym space (Just un) [] {-atts-}) un
 where un = unitWrapper c

-- | Same as 'uc', except it builds the Concept portion of the UnitalChunk
-- from a given uid, term, and defn. Those are the first three arguments
uc' :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> String -> Symbol ->
  u -> {-Attributes -> -} UnitalChunk
uc' i t d s u {-atts-} = UC (dqd (dcc i t d) s Real (Just un) []{-atts-}) un
 where un = unitWrapper u

-- | Same as 'uc'', but does not assume the 'Space'
ucs :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP ->
  String -> Symbol -> u -> Space -> {-Attributes -> -} UnitalChunk
ucs nam trm desc sym un space {-atts-} = UC (dqd (dcc nam trm desc) sym space (Just uu) []{-atts-}) uu
  where uu = unitWrapper un

-- ucs With a Sentence for desc
ucsWS :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> 
  Sentence -> Symbol -> u -> Space -> Attributes -> UnitalChunk
ucsWS nam trm desc sym un space atts = UC (dqd (dccWDS nam trm desc) sym space (Just uu) atts) uu
  where uu = unitWrapper un

--Better names will come later.
-- | Create a UnitalChunk in the same way as 'uc'', but with a 'Sentence' for
-- the definition instead of a String
makeUCWDS :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> Sentence -> Symbol ->
  u -> Attributes -> UnitalChunk
makeUCWDS nam trm desc sym un atts = UC (dqd (dccWDS nam trm desc) sym Real (Just uu) atts) uu
  where uu = unitWrapper un

-- | Create a UnitalChunk from a 'DefinedQuantityDict' which has a unit already.
-- fail otherwise
ucFromDQD :: DefinedQuantityDict -> UnitalChunk
ucFromDQD defq = UC defq (unitWrapper uu) 
  where uu = maybe (error "ucFromDQD must be called on a DQD with a unit") id (getUnit defq)
