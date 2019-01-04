{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Quantity 
  (QuantityDict, qw, mkQuant, mkQuant', implVar,codeVC,vc,vcSt,vc'') where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), HasSpace(typ), Quantity)
import Language.Drasil.Chunk.NamedIdea (IdeaDict,nw,mkIdea,nc)
import Language.Drasil.Chunk.UnitDefn(UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Stages (Stage(..))
import Language.Drasil.Symbol (Symbol(Empty))

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

qw :: (Quantity q, MayHaveUnit q) => q -> QuantityDict
qw q = QD (nw q) (q^.typ) (symbol q) (getUnit q)

-- For when the symbol is constant through stages
mkQuant :: String -> NP -> Symbol -> Space -> Maybe UnitDefn -> Maybe String -> QuantityDict
mkQuant i t s sp u ab = QD (mkIdea i t ab) sp (\_ -> s) u

-- For when the symbol changes depending on the stage
mkQuant' :: String -> NP -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> Maybe String -> QuantityDict
mkQuant' i t symbs sp u ab = QD (mkIdea i t ab) sp symbs u

-- | implVar makes an variable that is implementation-only
implVar :: String -> NP -> Symbol -> Space -> QuantityDict
implVar i des sym ty = vcSt i des f ty
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Creates a Quantity from an uid, term, symbol, and space
vc :: String -> NP -> Symbol -> Space -> QuantityDict
vc i des sym space = QD (nw $ nc i des) space (\_ -> sym) Nothing

-- | Like cv, but creates a QuantityDict from something that knows about stages
vcSt :: String -> NP -> (Stage -> Symbol) -> Space -> QuantityDict
vcSt i des sym space = QD (nw $ nc i des) space sym Nothing

codeVC :: Idea c => c -> Symbol -> Space -> QuantityDict
codeVC n s t = QD (nw n) t f Nothing
  where
    f :: Stage -> Symbol
    f Implementation = s
    f Equational = Empty

-- | Creates a QuantityDict from an 'Idea', symbol and space
vc'' :: Idea c => c -> Symbol -> Space -> QuantityDict
vc'' n sy space = vc (n ^. uid) (n ^. term) sy space
