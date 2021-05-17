{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Quantity (QuantityDict, codeVC, implVar, implVar', 
  mkQuant, mkQuant', qw, vc, vc'', vcSt, vcUnit) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), HasSpace(typ), Quantity)
import Language.Drasil.Chunk.NamedIdea (IdeaDict,nw,mkIdea,nc)
import Language.Drasil.Chunk.UnitDefn(UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Stages (Stage(..))
import Language.Drasil.Symbol (Symbol(Empty))

-- | A datatype that contains an 'IdeaDict', 'Space', a function from 
-- 'Stage' -> 'Symbol', and 'Maybe' a 'UnitDefn'
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

-- | A constructor for a 'QuantityDict' from another 'Quantity' with units
qw :: (Quantity q, MayHaveUnit q) => q -> QuantityDict
qw q = QD (nw q) (q^.typ) (symbol q) (getUnit q)

-- | Make a QuantityDict from a 'UID', 'NP', 'Symbol', 'Space', 
-- 'Maybe' 'UnitDefn', and an abbreviation ('Maybe' 'String')
mkQuant :: String -> NP -> Symbol -> Space -> Maybe UnitDefn -> Maybe String -> 
  QuantityDict
mkQuant i t s sp u ab = QD (mkIdea i t ab) sp (const s) u

-- | Similar to 'mkQuant', but the abbreviation is moved to 
-- the third argument ('Maybe' 'String'), and the 'Symbol' is no longer 'const'
mkQuant' :: String -> NP -> Maybe String -> Space -> (Stage -> Symbol) -> 
  Maybe UnitDefn -> QuantityDict
mkQuant' i t ab = QD (mkIdea i t ab)

-- | implVar makes a variable that is implementation-only
implVar :: String -> NP -> Space -> Symbol -> QuantityDict
implVar i des sp sym = vcSt i des f sp
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Like implVar but allows specification of abbreviation and unit 
implVar' :: String -> NP -> Maybe String -> Space -> Symbol -> 
  Maybe UnitDefn -> QuantityDict
implVar' s np a t sym = mkQuant' s np a t f
  where f :: Stage -> Symbol
        f Implementation = sym
        f Equational = Empty

-- | Creates a 'Quantity' from a 'UID', term ('NP'), 'Symbol', and 'Space'
vc :: String -> NP -> Symbol -> Space -> QuantityDict
vc i des sym space = QD (nw $ nc i des) space (const sym) Nothing

-- | Creates a 'Quantity' from a 'UID', term ('NP'), 'Symbol', 'Space', and unit ('UnitDefn')
vcUnit :: String -> NP -> Symbol -> Space -> UnitDefn -> QuantityDict
vcUnit i des sym space u = QD (nw $ nc i des) space (const sym) (Just u)

-- | Similar to vc, but creates a 'QuantityDict' from something that knows about stages
vcSt :: String -> NP -> (Stage -> Symbol) -> Space -> QuantityDict
vcSt i des sym space = QD (nw $ nc i des) space sym Nothing

-- | Makes a QuantityDict from an 'Idea', 'Symbol', and 'Space'. 'Symbol' is implementation-only
codeVC :: Idea c => c -> Symbol -> Space -> QuantityDict
codeVC n s t = QD (nw n) t f Nothing
  where
    f :: Stage -> Symbol
    f Implementation = s
    f Equational = Empty

-- | Creates a 'QuantityDict' from an 'Idea', 'Symbol', and 'Space'
vc'' :: Idea c => c -> Symbol -> Space -> QuantityDict
vc'' n = vc (n ^. uid) (n ^. term)
