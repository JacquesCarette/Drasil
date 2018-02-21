{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition(..), fromEqn, fromEqn', fromEqn'', equat, getVC
  , ec, ec', aqd) where

import Control.Lens ((^.), makeLenses)
import Prelude hiding (id)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), Idea(..))
import Language.Drasil.Chunk.Quantity (Quantity(getUnit),HasSpace(typ), QuantityDict,
  mkQuant, qw)
import Language.Drasil.Chunk.ExprRelat
import Language.Drasil.Chunk.VarChunk (VarChunk, vcSt)
import Language.Drasil.Chunk.SymbolForm (HasSymbol(symbol))
import Language.Drasil.Unit (Unit(..), unitWrapper)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Spec

-- | A QDefinition is a 'Quantity' with a defining equation.
data QDefinition = EC { _qua :: QuantityDict, _equat :: Expr, _att :: Attributes}
makeLenses ''QDefinition

-- this works because UnitalChunk is a Chunk
instance Chunk QDefinition where id = qua . id
instance NamedIdea QDefinition where term = qua . term
instance Idea QDefinition where getA c = getA $ c ^. qua
instance HasSpace QDefinition where typ = qua . typ
instance HasSymbol QDefinition where symbol s (EC a _ _)  = symbol s a
instance Quantity QDefinition where getUnit (EC a _ _)   = getUnit a
  
instance ExprRelat QDefinition where relat = equat
instance HasAttributes QDefinition where attributes = att
instance Eq QDefinition where a == b = (a ^. id) == (b ^. id)
  
-- useful: to be used for equations with units
--FIXME: Space hack

-- | Create a 'QDefinition' with an id, noun phrase, term, symbol,
-- unit, and defining equation.  And it ignores the term...
fromEqn :: Unit u => String -> NP -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc _ symb un eqn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) eqn []

-- and without
--FIXME: Space hack
-- | Same as fromEqn, but has no units.
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn' nm desc _ symb eqn = EC (mkQuant nm desc symb Real Nothing Nothing) eqn []

-- | Create a 'QDefinition' with an id, noun phrase (term), symbol,
-- abbreviation, unit, and defining equation.
fromEqn'' :: (Unit u) => String -> NP -> Sentence -> Symbol -> String -> Maybe u -> Expr -> QDefinition
fromEqn'' nm desc _ symb abbr u eqn = 
  EC (mkQuant nm desc symb Real (fmap unitWrapper u) (Just abbr)) eqn []

-- | Smart constructor for QDefinitions. Requires a quantity, its defining 
-- equation, and a list of attributes
ec :: Quantity c => c -> Expr -> Attributes -> QDefinition
ec c = EC (qw c)

-- | Smart constructor for QDefinitions. Requires a quantity and its defining
-- equation. Assumes no attributes.
ec' :: Quantity c => c -> Expr -> QDefinition
ec' e c = ec e c []
  
-- | Returns a 'VarChunk' from a 'QDefinition'.
-- Currently only used in example /Modules/ which are being reworked.
getVC :: QDefinition -> VarChunk
getVC qd = vcSt (qd ^. id) (qd ^. term) (\s -> symbol s qd) (qd ^. typ)

-- | For testing ONLY. Once all the chunks are updated for attributes this
-- should be removed and the other constructors should be updated to include
-- attributes
aqd :: QDefinition -> Attributes -> QDefinition
aqd (EC a b _) d = ec a b d
