{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition, fromEqn, fromEqn', equat
  , ec, qua, fromEqn''', fromEqn'''') where

import Control.Lens ((^.), makeLenses, view)
import Language.Drasil.Development.Unit (unitWrapper, MayHaveUnit(getUnit))

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), IsUnit, DefiningExpr(defnExpr), Definition(defn),
  ConceptDomain)
import Language.Drasil.Chunk.Quantity (HasSpace(typ), Quantity, QuantityDict, 
  mkQuant, qw)

import Language.Drasil.Expr (Expr)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space(Real))
import Language.Drasil.Spec (Sentence(EmptyS))
import Language.Drasil.Symbol (Symbol)

-- | A QDefinition is a 'Quantity' with a defining expression, and a definition
data QDefinition = EC { _qua :: QuantityDict , _defn' :: Sentence, _equat :: Expr }
makeLenses ''QDefinition

-- this works because UnitalChunk is a Chunk
instance HasUID        QDefinition where uid = qua . uid
instance NamedIdea     QDefinition where term = qua . term
instance Idea          QDefinition where getA c = getA $ c ^. qua
instance HasSpace      QDefinition where typ = qua . typ
instance HasSymbol     QDefinition where symbol e st = symbol (e^.qua) st
instance Definition    QDefinition where defn = defn'
instance Quantity      QDefinition where 
instance DefiningExpr  QDefinition where defnExpr = equat
instance Eq            QDefinition where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit   QDefinition where getUnit = getUnit . view qua

-- | Create a 'QDefinition' with a uid, noun phrase (term), definition, symbol,
-- unit, and defining equation.
--FIXME: Space hack
fromEqn :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc def symb un eqn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) def eqn

-- | Same as fromEqn, but has no units.
--FIXME: Space hack
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn' nm desc def symb eqn = EC (mkQuant nm desc symb Real Nothing Nothing) def eqn

-- The next two variants are only used in Chunk.DataDefinitions.
fromEqn''' :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn''' nm desc def symb un eqn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) def eqn

fromEqn'''' :: String -> NP -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn'''' nm desc def symb eqn = EC (mkQuant nm desc symb Real Nothing Nothing) def eqn

-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation. HACK - makes the definition EmptyS !!! FIXME
ec :: (Quantity c) => c -> Expr -> QDefinition
ec c eqn = EC (qw c) EmptyS eqn
