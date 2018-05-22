{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition, fromEqn, fromEqn', fromEqn'', equat, getVC
  , ec) where

import Control.Lens ((^.), makeLenses)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Classes (HasUID(uid),NamedIdea(term), Idea(getA), DOM,
  HasSymbol(symbol), IsUnit, HasAttributes(attributes), ExprRelat(relat), HasReference)
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.Attribute.References (References)
import Language.Drasil.Chunk.Concept (ConceptChunk)
import Language.Drasil.Chunk.Quantity (Quantity(getUnit), HasSpace(typ), QuantityDict,
  mkQuant, qw)
import Language.Drasil.Chunk.VarChunk (VarChunk, vcSt)
import Language.Drasil.Unit (unitWrapper)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Spec (Sentence)

-- | A QDefinition is a 'Quantity' with a defining equation.
data QDefinition = EC
          { _qua :: QuantityDict
          , _equat :: Expr
          }
makeLenses ''QDefinition

-- this works because UnitalChunk is a Chunk
instance HasUID        QDefinition where uid = qua . uid
instance NamedIdea     QDefinition where term = qua . term
instance Idea          QDefinition where getA c = getA $ c ^. qua
instance HasSpace      QDefinition where typ = qua . typ
instance HasSymbol     QDefinition where symbol e st = symbol (e^.qua) st
instance Quantity      QDefinition where getUnit (EC a _)   = getUnit a
instance ExprRelat     QDefinition where relat = equat
instance HasAttributes QDefinition where attributes = qua . attributes
instance HasReference  QDefinition where
instance Eq            QDefinition where a == b = (a ^. uid) == (b ^. uid)
  
-- | Create a 'QDefinition' with an uid, noun phrase (term), definition, symbol,
-- unit, and defining equation.  And it ignores the definition...
--FIXME: Space hack
fromEqn :: (IsUnit u, DOM u ~ ConceptChunk) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> Attributes -> QDefinition
fromEqn nm desc _ symb un eqn atts = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing atts) eqn

-- | Same as fromEqn, but has no units.
--FIXME: Space hack
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> Attributes -> QDefinition
fromEqn' nm desc _ symb eqn atts = EC (mkQuant nm desc symb Real Nothing Nothing atts) eqn

-- | Create a 'QDefinition' with an uid, noun phrase (term), symbol,
-- abbreviation, unit, and defining equation.
fromEqn'' :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> Sentence ->
 Symbol -> String -> Maybe u -> Expr -> Attributes -> QDefinition
fromEqn'' nm desc _ symb abbr u eqn atts = 
  EC (mkQuant nm desc symb Real (fmap unitWrapper u) (Just abbr) atts) eqn

-- | Smart constructor for QDefinitions. Requires a quantity, its defining 
-- equation, and a list of attributes
ec :: (HasAttributes c, Quantity c) => c -> Expr -> QDefinition
ec c eqn = EC (qw c) eqn

-- | Returns a 'VarChunk' from a 'QDefinition'.
-- Currently only used in example /Modules/ which are being reworked.
getVC :: QDefinition -> VarChunk
getVC qd = vcSt (qd ^. uid) (qd ^. term) (symbol qd) (qd ^. typ) (qd ^. attributes)