{-# LANGUAGE TemplateHaskell #-}
-- | Defines chunks to add units to a quantity. Similar to 'UnitalChunk'.
module Language.Drasil.Chunk.Unitary (
  -- * Chunk Types
  Unitary(..), UnitaryChunk,
  -- * Constructors
  mkUnitary, unitary, unitary', unit_symb) where

import Language.Drasil.Symbol
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  IsUnit, usymb, Quantity)
import Language.Drasil.Chunk.Quantity (QuantityDict, mkQuant, mkQuant', qw)
import Language.Drasil.UnitLang (USymb)
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit), UnitDefn, unitWrapper)
import Language.Drasil.Space (Space, HasSpace(..))
import Language.Drasil.Stages (Stage)
import Language.Drasil.NounPhrase.Core (NP)
import Drasil.Database.UID (HasUID(..))

import Control.Lens ((^.), makeLenses)

-- | A Unitary is a 'Quantity' that __must__ have a unit.
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

-- | UnitaryChunks are for ideas with quantities that must have units. Contains a 'QuantityDict' and a 'UnitDefn'.
--
-- Ex. A pendulum arm is an idea associated with a symbol (l) and units (cm, m, etc.).
data UnitaryChunk = UC { _quant :: QuantityDict
                       , _un :: UnitDefn
                       }
makeLenses ''UnitaryChunk

-- | Finds 'UID' of the 'QuantityDict' used to make the 'UnitaryChunk'.
instance HasUID        UnitaryChunk where uid = quant . uid
-- | Finds term ('NP') of the 'QuantityDict' used to make the 'UnitaryChunk'.
instance NamedIdea     UnitaryChunk where term = quant . term
-- | Finds the idea contained in the 'QuantityDict' used to make the 'UnitaryChunk'.
instance Idea          UnitaryChunk where getA uc = getA $ uc ^. quant
-- | Finds the 'Space' of the 'QuantityDict' used to make the 'UnitaryChunk'.
instance HasSpace      UnitaryChunk where typ = quant . typ
-- | Finds the 'Symbol' of the 'QuantityDict' used to make the 'UnitaryChunk'.
instance HasSymbol     UnitaryChunk where symbol u = symbol (u^.quant)
-- | 'UnitaryChunk's have a 'Quantity'.
instance Quantity      UnitaryChunk where
-- | Finds the unit definition of a 'UnitaryChunk'.
instance Unitary       UnitaryChunk where unit x = x ^. un
-- | Finds the units of the 'QuantityDict' used to make the 'UnitaryChunk'.
instance MayHaveUnit   UnitaryChunk where getUnit u = Just $ u ^. un

-- | Builds the 'QuantityDict' part from the 'UID', term ('NP'), 'Symbol', and 'Space'.
-- Assumes there's no abbreviation.
unitary :: (IsUnit u) => String -> NP -> Symbol -> u -> Space -> UnitaryChunk
unitary i t s u space = UC (mkQuant i t s space (Just uu) Nothing) uu -- Unit doesn't have a unitDefn, so [] is passed in
  where uu = unitWrapper u

-- | Same as 'unitary' but with a 'Symbol' that changes based on the 'Stage'.
unitary' :: (IsUnit u) => String -> NP -> (Stage -> Symbol) -> u -> Space -> UnitaryChunk
unitary' i t s u space = UC (mkQuant' i t Nothing space s (Just uu)) uu -- Unit doesn't have a unitDefn, so [] is passed in
  where uu = unitWrapper u

-- | Makes a 'UnitaryChunk' from a quantity with a unit.
mkUnitary :: (Unitary u, MayHaveUnit u) => u -> UnitaryChunk
mkUnitary u = UC (qw u) (unit u)

-- | Helper for getting the unit's 'Symbol' from a chunk, 
-- as opposed to the symbols of the chunk itself.
unit_symb :: (Unitary c) => c -> USymb
unit_symb c = usymb $ unit c

