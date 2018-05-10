{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Quantity 
  ( Quantity(..), QuantityDict, qw, mkQuant, HasSpace(typ)
  ) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), HasSpace(typ), HasAttributes(attributes))
import Language.Drasil.Chunk.NamedIdea (IdeaDict,nw,mkIdea)
import Language.Drasil.Symbol (Symbol,Stage)
import Language.Drasil.Space (Space)
import Language.Drasil.NounPhrase
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Unit(UnitDefn)

-- | A Quantity is an 'Idea' with a 'Space' and a symbol and 
-- may have units
class (Idea c, HasSpace c, HasSymbol c) => Quantity c where
  -- | Provides the units a quantity is measured in, if any, otherwise returns
  -- 'Nothing'
  getUnit  :: c -> Maybe UnitDefn

data QuantityDict = QD { _id' :: IdeaDict
                       , _typ' :: Space
                       , _symb' :: Stage -> Symbol
                       , _unit' :: Maybe UnitDefn
                       , _attribs :: Attributes -- FIXME: Attributes included for consistency,
                                                -- since every chunk should eventually have the
                                                -- capability for attributes.
                       }
makeLenses ''QuantityDict

instance HasUID    QuantityDict where uid = id' . uid
instance NamedIdea QuantityDict where term = id' . term
instance Idea      QuantityDict where getA  qd = getA (qd ^. id')
instance HasSpace  QuantityDict where typ = typ'
instance HasSymbol QuantityDict where symbol = view symb'
instance Quantity  QuantityDict where getUnit = view unit'
instance Eq        QuantityDict where a == b = (a ^. uid) == (b ^. uid)
instance HasAttributes QuantityDict where attributes = attribs

qw :: (HasAttributes q, Quantity q) => q -> QuantityDict
qw q = QD (nw q) (q^.typ) (symbol q) (getUnit q) (q ^. attributes)

mkQuant :: String -> NP -> Symbol -> Space -> Maybe UnitDefn -> Maybe String -> Attributes -> QuantityDict
mkQuant i t s sp u ab atts = QD (mkIdea i t ab) sp (\_ -> s) u atts
