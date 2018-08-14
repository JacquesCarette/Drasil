{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition, fromEqn, fromEqn', fromEqn'', equat, getVC
  , ec, qua, fromEqn''', fromEqn'''') where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), IsUnit, ExprRelat(relat),
  ConceptDomain)
import Language.Drasil.Chunk.Quantity (HasSpace(typ), QuantityDict,
  mkQuant, qw, Quantity)
import Language.Drasil.Chunk.VarChunk (VarChunk, vcSt)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space(Real))

import Language.Drasil.Development.Unit(unitWrapper, MayHaveUnit(getUnit))

import Language.Drasil.Expr (Expr)
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
instance Quantity      QDefinition where 
instance ExprRelat     QDefinition where relat = equat
instance Eq            QDefinition where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit   QDefinition where getUnit = getUnit . view qua

-- | Create a 'QDefinition' with a uid, noun phrase (term), definition, symbol,
-- unit, and defining equation.  And it ignores the definition...
--FIXME: Space hack
fromEqn :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc _ symb un eqn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) eqn

-- | Same as fromEqn, but has no units.
--FIXME: Space hack
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn' nm desc _ symb eqn = EC (mkQuant nm desc symb Real Nothing Nothing) eqn

-- | Create a 'QDefinition' with an uid, noun phrase (term), symbol,
-- abbreviation, unit, and defining equation.
fromEqn'' :: (IsUnit u, ConceptDomain u) => String -> NP -> Sentence ->
 Symbol -> String -> Maybe u -> Expr -> QDefinition
fromEqn'' nm desc _ symb abbr u eqn = 
  EC (mkQuant nm desc symb Real (fmap unitWrapper u) (Just abbr)) eqn

fromEqn''' :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn''' nm desc _ symb un eqn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) eqn

fromEqn'''' :: String -> NP -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn'''' nm desc _ symb eqn = EC (mkQuant nm desc symb Real Nothing Nothing) eqn

-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation
ec :: (Quantity c) => c -> Expr -> QDefinition
ec c eqn = EC (qw c) eqn

-- | Returns a 'VarChunk' from a 'QDefinition'.
-- Currently only used in example /Modules/ which are being reworked.
getVC :: QDefinition -> VarChunk
getVC qd = vcSt (qd ^. uid) (qd ^. term) (symbol qd) (qd ^. typ)
