{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Unitary
  ( UnitaryChunk
  , unitary, mkUnitary
  , Unitary(..)) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  ConceptDomain(DOM), HasSymbol(symbol), IsUnit, HasAttributes(attributes))
import Language.Drasil.Chunk.Quantity (Quantity(..), QuantityDict, mkQuant, qw, 
  HasSpace(typ))
import Language.Drasil.Unit (UnitDefn, unitWrapper)
import Language.Drasil.Symbol
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Space
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Chunk.Concept (ConceptChunk)

import Control.Lens ((^.), makeLenses)

-- | A Unitary is a 'Quantity' that __must__ have a unit
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

-- | UnitaryChunks are 'Unitary's with 'Symbols'
data UnitaryChunk = UC { _quant :: QuantityDict
                       , _un :: UnitDefn
                       , _attribs :: Attributes -- FIXME: Attributes included for consistency,
                                                -- since every chunk should eventually have the
                                                -- capability for attributes.
                       }
makeLenses ''UnitaryChunk

instance HasUID    UnitaryChunk where uid = quant . uid
instance NamedIdea UnitaryChunk where term = quant . term
instance Idea      UnitaryChunk where getA uc = getA $ uc ^. quant
instance HasSpace  UnitaryChunk where typ = quant . typ
instance HasSymbol UnitaryChunk where symbol u st = symbol (u^.quant) st
instance Quantity  UnitaryChunk where getUnit = Just . _un
instance Unitary   UnitaryChunk where unit x = x ^. un
instance HasAttributes UnitaryChunk where attributes = attribs

-- Builds the Quantity part from the uid, term, symbol and space.
-- assumes there's no abbreviation.
unitary :: (IsUnit u, DOM u ~ ConceptChunk) => 
  String -> NP -> Symbol -> u -> Space -> Attributes -> UnitaryChunk
unitary i t s u space atts = UC (mkQuant i t s space (Just uu) Nothing atts) uu atts --FIXME:atts used twice?
  where uu = unitWrapper u

mkUnitary :: (HasAttributes u, Unitary u) => u -> Attributes -> UnitaryChunk
mkUnitary u atts = UC (qw u) (unit u) atts
