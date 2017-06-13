{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Unitary
  ( UnitaryChunk
  , unitary'
  , Unitary(..)) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), nc)
import Language.Drasil.Chunk.SymbolForm (SymbolForm(..), SF(..))
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Unit (Unit(..), UnitDefn(..))
import Language.Drasil.Symbol
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP)

-- | A Unitary is a 'Quantity' that __must__ have a unit, but not
-- necessarily a 'Symbol'
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

-- | UnitaryChunks are 'Unitary's with 'Symbols'
data UnitaryChunk where
  UC :: (NamedIdea c, Unit u) => c -> Symbol -> u -> Space -> UnitaryChunk
instance Chunk UnitaryChunk where
  id = nl id
instance NamedIdea UnitaryChunk where
  term = nl term
  getA (UC qc _ _ _) = getA qc
instance Quantity UnitaryChunk where
  typ f (UC named s u t) = fmap (\x -> UC named s u x) (f t)
  getSymb = Just . SF
  getUnit = Just . unit
instance Unitary UnitaryChunk where
  unit (UC _ _ u _) = UU u
instance SymbolForm UnitaryChunk where
  symbol f (UC n s u t) = fmap (\x -> UC n x u t) (f s)
  
nl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens UnitaryChunk a
nl l f (UC qc s u t) = fmap (\x -> UC (set l x qc) s u t) (f (qc ^. l))

-- FIXME: Temporarily hacking in the space for Unitary chunks, these can be fixed
-- with the use of other constructors.

-- | Same as 'unitary', except it builds the NamedIdea portion of the UnitaryChunk
-- from a given id and term. Those are the first two arguments
unitary' :: (Unit u) => String -> NP -> Symbol -> u -> UnitaryChunk
unitary' i t s u = UC (nc i t) s u Rational
