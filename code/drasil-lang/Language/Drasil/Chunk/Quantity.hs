{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Quantity 
  ( Quantity(..), QuantityDict, qw, mkQuant, mkQuant', HasSpace(typ)) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil.Chunk.NamedIdea (IdeaDict,nw,mkIdea)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), HasSpace(typ))
import Language.Drasil.Development.Unit(UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Symbol (Symbol, Stage)

-- | A Quantity is an 'Idea' with a 'Space' and a symbol and 
-- may have units
class (Idea c, HasSpace c, HasSymbol c, MayHaveUnit c) => Quantity c where
  -- | Provides the units a quantity is measured in, if any, otherwise returns
  -- 'Nothing'

data QuantityDict = QD { _id' :: IdeaDict
                       , _typ' :: Space
                       , _symb' :: Stage -> Symbol
                       , _unit' :: Maybe UnitDefn
                       }
makeLenses ''QuantityDict

instance HasUID        QuantityDict where uid = id' . uid
instance NamedIdea     QuantityDict where term = id' . term
instance Idea          QuantityDict where getA  qd = getA (qd ^. id')
instance HasSpace      QuantityDict where typ = typ'
instance HasSymbol     QuantityDict where symbol = view symb'
instance Quantity      QuantityDict where 
instance Eq            QuantityDict where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit   QuantityDict where getUnit = view unit'

qw :: (Quantity q) => q -> QuantityDict
qw q = QD (nw q) (q^.typ) (symbol q) (getUnit q)

-- For when the symbol is constant through stages
mkQuant :: String -> NP -> Symbol -> Space -> Maybe UnitDefn -> Maybe String -> QuantityDict
mkQuant i t s sp u ab = QD (mkIdea i t ab) sp (\_ -> s) u

-- For when the symbol changes depending on the stage
mkQuant' :: String -> NP -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> Maybe String -> QuantityDict
mkQuant' i t symbs sp u ab = QD (mkIdea i t ab) sp symbs u
