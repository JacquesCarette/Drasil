{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Unitary
  ( UnitaryChunk
  , unitary, mkUnitary
  , Unitary(..)) where

import Control.Lens ((^.), makeLenses)
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), Idea(..))
import Language.Drasil.Chunk.Quantity (Quantity(..), QuantityDict, mkQuant, qw, 
  HasSpace(typ))
import Language.Drasil.Chunk.SymbolForm (HasSymbol(symbol))
import Language.Drasil.Unit (IsUnit, UnitDefn, unitWrapper)
import Language.Drasil.Symbol
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP)

-- | A Unitary is a 'Quantity' that __must__ have a unit
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

-- | UnitaryChunks are 'Unitary's with 'Symbols'
data UnitaryChunk = UC { _quant :: QuantityDict, _un :: UnitDefn }
makeLenses ''UnitaryChunk

instance Chunk     UnitaryChunk where id = quant . id
instance NamedIdea UnitaryChunk where term = quant . term
instance Idea      UnitaryChunk where getA uc = getA $ uc ^. quant
instance HasSpace  UnitaryChunk where typ = quant . typ
instance HasSymbol UnitaryChunk where symbol st (UC s _) = symbol st s
instance Quantity  UnitaryChunk where getUnit = Just . _un
instance Unitary   UnitaryChunk where unit x = x ^. un
  
-- Builds the Quantity part from the id, term, symbol and space.
-- assumes there's no abbreviation.
unitary :: IsUnit u => String -> NP -> Symbol -> u -> Space -> UnitaryChunk
unitary i t s u space = UC (mkQuant i t s space (Just uu) Nothing) uu
  where uu = unitWrapper u

mkUnitary :: Unitary u => u -> UnitaryChunk
mkUnitary u = UC (qw u) (unit u)
