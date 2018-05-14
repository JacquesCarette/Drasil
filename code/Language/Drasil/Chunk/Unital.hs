{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Unital 
  ( UnitalChunk(..)
  , makeUCWDS
  , ucFromCV
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
import Language.Drasil.Chunk.Quantity (Quantity(..),HasSpace(typ))
import Language.Drasil.Chunk.Unitary (Unitary(..))
import Language.Drasil.Unit (UnitDefn,unitWrapper)
import Language.Drasil.Symbol
import Language.Drasil.Space
import Language.Drasil.Spec (Sentence)

import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.DefinedQuantity as D

-- | UnitalChunks are Unitary
data UnitalChunk = UC { _dqd :: D.DefinedQuantityDict
                      , _uni :: UnitDefn
                      , _attribs :: Attributes
                      }
makeLenses ''UnitalChunk

instance HasUID        UnitalChunk where uid = dqd . uid
instance NamedIdea     UnitalChunk where term = dqd . term
instance Idea          UnitalChunk where getA (UC qc _ _) = getA qc
instance Definition    UnitalChunk where defn = dqd . defn
instance ConceptDomain UnitalChunk where
  type DOM UnitalChunk = ConceptChunk
  cdom = dqd . cdom
instance Concept       UnitalChunk where
instance HasSpace      UnitalChunk where typ = dqd . typ
instance HasSymbol     UnitalChunk where symbol c st = symbol (c^.con) st
instance Quantity      UnitalChunk where getUnit = Just . unit
instance Unitary       UnitalChunk where unit = view uni
instance HasAttributes UnitalChunk where attributes = attribs

--{BEGIN HELPER FUNCTIONS}--

-- | Used to create a UnitalChunk from a 'Concept', 'Symbol', and 'Unit'.
-- Assumes the 'Space' is Real
uc :: (Concept c, IsUnit u, DOM c ~ ConceptChunk, DOM u ~ ConceptChunk) =>
  c -> Symbol -> u -> Attributes -> UnitalChunk
uc a b c atts = ucs' a b c Real atts

ucs' :: (Concept c, IsUnit u, DOM c ~ ConceptChunk, DOM u ~ ConceptChunk) =>
  c -> Symbol -> u -> Space -> Attributes -> UnitalChunk
ucs' a sym c space atts = UC (cqs (cw a) sym space) (unitWrapper c) atts

-- | Same as 'uc', except it builds the Concept portion of the UnitalChunk
-- from a given uid, term, and defn. Those are the first three arguments
uc' :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> String -> Symbol ->
  u -> Attributes -> UnitalChunk
uc' i t d s u atts = UC (cqs (dcc i t d) s Real) (unitWrapper u) atts

-- | Same as 'uc'', but does not assume the 'Space'
ucs :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP ->
  String -> Symbol -> u -> Space -> Attributes -> UnitalChunk
ucs nam trm desc sym un space atts = UC (cqs (dcc nam trm desc) sym space) (unitWrapper un) atts

-- ucs With a Sentence for desc
ucsWS :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> 
  Sentence -> Symbol -> u -> Space -> Attributes -> UnitalChunk
ucsWS nam trm desc sym un space atts = UC (cqs (dccWDS nam trm desc) sym space) (unitWrapper un) atts

--Better names will come later.
-- | Create a UnitalChunk in the same way as 'uc'', but with a 'Sentence' for
-- the definition instead of a String
makeUCWDS :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> Sentence -> Symbol ->
  u -> Attributes -> UnitalChunk
makeUCWDS nam trm desc sym un atts = UC (cqs (dccWDS nam trm desc) sym Real) (unitWrapper un) atts

-- | Create a UnitalChunk from a 'ConVar' by supplying the additional 'Unit'
ucFromCV :: (IsUnit u, DOM u ~ ConceptChunk) => DefinedQuantityDict -> u -> Attributes -> UnitalChunk
ucFromCV dqd un atts = UC dqd (unitWrapper un) atts
