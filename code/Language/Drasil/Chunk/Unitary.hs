{-# LANGUAGE GADTs, Rank2Types, TemplateHaskell #-}
module Language.Drasil.Chunk.Unitary
  ( UnitaryChunk
  , unitary
  , Unitary(..)) where

import Control.Lens ((^.), makeLenses)
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), Idea(..))
import Language.Drasil.Chunk.Quantity (Quantity(..), QuantityDict, mkQuant)
import Language.Drasil.Unit (Unit, UnitDefn, unitWrapper)
import Language.Drasil.Symbol
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP)

-- | A Unitary is a 'Quantity' that __must__ have a unit
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

-- | UnitaryChunks are 'Unitary's with 'Symbols'
data UnitaryChunk = UC { _quant :: QuantityDict, _un :: UnitDefn }
makeLenses ''UnitaryChunk

instance Chunk UnitaryChunk where
  id = quant . id
instance NamedIdea UnitaryChunk where
  term = quant . term
instance Idea UnitaryChunk where
  getA uc = getA $ uc ^. quant
instance Quantity UnitaryChunk where
  typ = quant . typ
  getSymb st (UC s _) = getSymb st s
  getUnit = Just . _un
  getStagedS (UC s _) = getStagedS s
instance Unitary UnitaryChunk where
  unit x = x ^. un
  
-- Builds the NamedIdea portion of the UnitaryChunk
-- from a given id and term. Those are the first two arguments
unitary :: Unit u => String -> NP -> Symbol -> u -> Space -> UnitaryChunk
unitary i t s u space = UC (mkQuant i t s space (Just uu)) uu
  where uu = unitWrapper u
