module Language.Drasil.Chunk.Quantity 
  ( Quantity(..), QuantityDict, qw, mkQuant, HasSpace(typ)
  ) where

import Control.Lens

import Language.Drasil.Space
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.NounPhrase

import Prelude hiding (id)

import Language.Drasil.Chunk.SymbolForm (Stage(..),HasSymbol(..), eqSymb)
import Language.Drasil.Unit(UnitDefn)

-- | A Quantity is an 'Idea' with a 'Space' and a symbol and 
-- may have units
class (Idea c, HasSpace c, HasSymbol c) => Quantity c where
  -- | Provides the units a quantity is measured in, if any, otherwise returns
  -- 'Nothing'
  getUnit  :: c -> Maybe UnitDefn

data QuantityDict = QD { _id :: IdeaDict, _typ :: Space,
  _symb :: Stage -> Symbol, _unit :: Maybe UnitDefn}

instance Chunk     QuantityDict where id = qlens . id
instance NamedIdea QuantityDict where term = qlens . term
instance Idea      QuantityDict where getA  qd = getA (qd ^. qlens)
instance HasSpace  QuantityDict where typ f qd = fmap (\x -> qd {_typ = x}) (f (_typ qd))
instance HasSymbol QuantityDict where symbol s qd = _symb qd s
instance Quantity  QuantityDict where getUnit qd = _unit qd
instance Eq        QuantityDict where a == b = (a ^. id) == (b ^. id)

-- FIXME: Ordering hack. Should be context-dependent
instance Ord QuantityDict where compare a b = compare (eqSymb a) (eqSymb b)

qlens :: Simple Lens QuantityDict IdeaDict
qlens f qd = fmap (\x -> qd {_id = x}) (f (_id qd))

qw :: Quantity q => q -> QuantityDict
qw q = QD (nw q) (q^.typ) (\stg -> symbol stg q) (getUnit q)

mkQuant :: String -> NP -> Symbol -> Space -> Maybe UnitDefn -> QuantityDict
mkQuant i t s sp u = QD (mkIdea i t Nothing) sp (\_ -> s) u
