{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Unitary
  ( UnitaryChunk
  , unitary
  , Unitary(..)) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), nc, Idea(..))
import Language.Drasil.Chunk.SymbolForm (SF(..), StagedSymbolChunk, ssc'
  , getSymbForStage)
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Unit (Unit(..), UnitDefn(..))
import Language.Drasil.Symbol
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP)

-- | A Unitary is a 'Quantity' that __must__ have a unit
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

-- | UnitaryChunks are 'Unitary's with 'Symbols'
data UnitaryChunk where
  UC :: (Idea c, Unit u) => 
    c -> StagedSymbolChunk -> u -> Space -> UnitaryChunk
instance Chunk UnitaryChunk where
  id = nl id
instance NamedIdea UnitaryChunk where
  term = nl term
instance Idea UnitaryChunk where
  getA (UC qc _ _ _) = getA qc
instance Quantity UnitaryChunk where
  typ f (UC named s u t) = fmap (\x -> UC named s u x) (f t)
  getSymb st (UC _ s _ _) = SF $ getSymbForStage st s
  getUnit = Just . unit
  getStagedS (UC _ s _ _) = s
instance Unitary UnitaryChunk where
  unit (UC _ _ u _) = UU u
  
nl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens UnitaryChunk a
nl l f (UC qc s u t) = fmap (\x -> UC (set l x qc) s u t) (f (qc ^. l))

-- Builds the NamedIdea portion of the UnitaryChunk
-- from a given id and term. Those are the first two arguments
unitary :: (Unit u) => String -> NP -> Symbol -> u -> Space -> UnitaryChunk
unitary i t s u space = UC (nc i t) (ssc' i s) u space
