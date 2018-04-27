{-# LANGUAGE TemplateHaskell #-}
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
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..),Idea(..))
import Language.Drasil.Chunk.Concept (Concept, dcc, dccWDS,Definition(..),ConceptDomain(..), cw)
import Language.Drasil.Chunk.ConVar (ConVar (..), cv)
import Language.Drasil.Chunk.Quantity (Quantity(..),HasSpace(typ))
import Language.Drasil.Chunk.Unitary (Unitary(..))
import Language.Drasil.Chunk.SymbolForm (HasSymbol(symbol))
import Language.Drasil.Unit (UnitDefn,IsUnit,unitWrapper)
import Language.Drasil.Symbol
import Language.Drasil.Space
import Language.Drasil.Spec (Sentence)

import Language.Drasil.NounPhrase (NP)

-- | UnitalChunks are Unitary
data UnitalChunk = UC { _con :: ConVar, _uni :: UnitDefn }
makeLenses ''UnitalChunk

instance Chunk         UnitalChunk where uid = con . uid
instance NamedIdea     UnitalChunk where term = con . term
instance Idea          UnitalChunk where getA (UC qc _) = getA qc
instance Definition    UnitalChunk where defn = con . defn
instance ConceptDomain UnitalChunk where cdom = con . cdom
instance Concept       UnitalChunk where
instance HasSpace      UnitalChunk where typ = con . typ
instance HasSymbol     UnitalChunk where symbol c st = symbol (c^.con) st
instance Quantity      UnitalChunk where getUnit = Just . unit
instance Unitary       UnitalChunk where unit = view uni
  
--{BEGIN HELPER FUNCTIONS}--

-- | Used to create a UnitalChunk from a 'Concept', 'Symbol', and 'Unit'.
-- Assumes the 'Space' is Real
uc :: (Concept c, IsUnit u) => c -> Symbol -> u -> UnitalChunk
uc a b c = UC (cv (cw a) b Real) (unitWrapper c)

ucs' :: (Concept c, IsUnit u) => c -> Symbol -> u -> Space -> UnitalChunk
ucs' a b c p = UC (cv (cw a) b p) (unitWrapper c)

-- | Same as 'uc', except it builds the Concept portion of the UnitalChunk
-- from a given uid, term, and defn. Those are the first three arguments
uc' :: (IsUnit u) => String -> NP -> String -> Symbol -> u -> UnitalChunk
uc' i t d s u = UC (cv (dcc i t d) s Real) (unitWrapper u)

-- | Same as 'uc'', but does not assume the 'Space'
ucs :: (IsUnit u) => String -> NP -> String -> Symbol -> u -> Space -> UnitalChunk
ucs nam trm desc sym un space = UC (cv (dcc nam trm desc) sym space) (unitWrapper un)

-- ucs With a Sentence for desc
ucsWS :: IsUnit u => String -> NP -> Sentence -> Symbol -> u -> Space -> UnitalChunk
ucsWS nam trm desc sym un space = UC (cv (dccWDS nam trm desc) sym space) (unitWrapper un)

--Better names will come later.
-- | Create a UnitalChunk in the same way as 'uc'', but with a 'Sentence' for
-- the definition instead of a String
makeUCWDS :: IsUnit u => String -> NP -> Sentence -> Symbol -> u -> UnitalChunk
makeUCWDS nam trm desc sym un = UC (cv (dccWDS nam trm desc) sym Real) (unitWrapper un)

-- | Create a UnitalChunk from a 'ConVar' by supplying the additional 'Unit'
ucFromCV :: IsUnit u => ConVar -> u -> UnitalChunk
ucFromCV conv un = UC conv (unitWrapper un)
