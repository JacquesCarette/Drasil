{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Quantity (QuantityDict, codeVC, implVar, mkQuant,
  mkQuant', qw, vc, vc'', vcSt, vcUnit) where

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

mkQuant :: String -> NP -> Symbol -> Space -> Maybe UnitDefn -> Maybe String -> QuantityDict
mkQuant i t s sp u ab = QD (mkIdea i t ab) sp (const s) u

mkQuant' :: String -> NP -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> Maybe String -> QuantityDict
mkQuant' i t s sp u ab = QD (mkIdea i t ab) sp s u

-- | implVar makes an variable that is implementation-only
implVar :: String -> NP -> Symbol -> Space -> QuantityDict
implVar i des sym = vcSt i des f
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Creates a Quantity from an uid, term, symbol, and space
vc :: String -> NP -> Symbol -> Space -> QuantityDict
vc i des sym space = QD (nw $ nc i des) space (const sym) Nothing

-- | Creates a Quantity from an uid, term, symbol, space, and unit
vcUnit :: String -> NP -> Symbol -> Space -> UnitDefn -> QuantityDict
vcUnit i des sym space u = QD (nw $ nc i des) space (const sym) (Just u)

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
vc'' n = vc (n ^. uid) (n ^. term)
