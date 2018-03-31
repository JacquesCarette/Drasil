{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Quantity 
  ( Quantity(..), QuantityDict, qw, mkQuant, HasSpace(typ)
  ) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil.Space
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.NounPhrase

import Language.Drasil.Chunk.SymbolForm (Stage(..),HasSymbol(..), eqSymb)
import Language.Drasil.Unit(UnitDefn)

-- | A Quantity is an 'Idea' with a 'Space' and a symbol and 
-- may have units
class (Idea c, HasSpace c, HasSymbol c) => Quantity c where
  -- | Provides the units a quantity is measured in, if any, otherwise returns
  -- 'Nothing'
  getUnit  :: c -> Maybe UnitDefn

data QuantityDict = QD { _id' :: IdeaDict, _typ' :: Space,
  _symb' :: Stage -> Symbol, _unit' :: Maybe UnitDefn}
makeLenses ''QuantityDict

instance Chunk     QuantityDict where uid = id' . uid
instance NamedIdea QuantityDict where term = id' . term
instance Idea      QuantityDict where getA  qd = getA (qd ^. id')
instance HasSpace  QuantityDict where typ = typ'
instance HasSymbol QuantityDict where symbol st x = view symb' x st
instance Quantity  QuantityDict where getUnit = view unit'
instance Eq        QuantityDict where a == b = (a ^. uid) == (b ^. uid)

-- FIXME: Ordering hack. Should be context-dependent
instance Ord QuantityDict where compare a b = compare (eqSymb a) (eqSymb b)

qw :: Quantity q => q -> QuantityDict
qw q = QD (nw q) (q^.typ) (\stg -> symbol stg q) (getUnit q)

mkQuant :: String -> NP -> Symbol -> Space -> Maybe UnitDefn -> Maybe String -> QuantityDict
mkQuant i t s sp u ab = QD (mkIdea i t ab) sp (\_ -> s) u
