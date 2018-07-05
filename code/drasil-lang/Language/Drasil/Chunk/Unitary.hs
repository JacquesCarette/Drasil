{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Unitary
  ( UnitaryChunk
  , unitary, mkUnitary
  , Unitary(..)) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  ConceptDomain, HasSymbol(symbol), IsUnit)
import Language.Drasil.Chunk.Quantity (Quantity(..), QuantityDict, mkQuant, qw, 
  HasSpace(typ))
import Language.Drasil.Development.Unit (UnitDefn, unitWrapper)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space)
import Language.Drasil.NounPhrase (NP)

import Control.Lens ((^.), makeLenses)

-- | A Unitary is a 'Quantity' that __must__ have a unit
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

-- | UnitaryChunks are 'Unitary's with 'Symbols'
data UnitaryChunk = UC { _quant :: QuantityDict
                       , _un :: UnitDefn
                       }
makeLenses ''UnitaryChunk

instance HasUID        UnitaryChunk where uid = quant . uid
instance NamedIdea     UnitaryChunk where term = quant . term
instance Idea          UnitaryChunk where getA uc = getA $ uc ^. quant
instance HasSpace      UnitaryChunk where typ = quant . typ
instance HasSymbol     UnitaryChunk where symbol u st = symbol (u^.quant) st
instance Quantity      UnitaryChunk where getUnit = Just . _un
instance Unitary       UnitaryChunk where unit x = x ^. un

-- Builds the Quantity part from the uid, term, symbol and space.
-- assumes there's no abbreviation.
unitary :: (IsUnit u, ConceptDomain u) =>
  String -> NP -> Symbol -> u -> Space -> UnitaryChunk
unitary i t s u space = UC (mkQuant i t s space (Just uu) Nothing) uu -- Unit doesn't have a unitDefn, so [] is passed in
  where uu = unitWrapper u

mkUnitary :: (Unitary u) => u -> UnitaryChunk
mkUnitary u = UC (qw u) (unit u)
