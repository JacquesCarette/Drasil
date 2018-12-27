{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', equat, ec) where

import Control.Lens ((^.), makeLenses, view)
import Language.Drasil.Development.Unit (unitWrapper, MayHaveUnit(getUnit))

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  IsUnit, DefiningExpr(defnExpr), Definition(defn), Quantity, HasSpace(typ))
import Language.Drasil.Chunk.Quantity (QuantityDict, mkQuant, qw)

import Language.Drasil.Expr (Expr)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space(Real))
import Language.Drasil.Sentence (Sentence(EmptyS))
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
fromEqn :: (IsUnit u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc def symb un eqn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) def eqn

-- | Same as fromEqn, but has no units.
--FIXME: Space hack
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn' nm desc def symb eqn = EC (mkQuant nm desc symb Real Nothing Nothing) def eqn

-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation. HACK - makes the definition EmptyS !!! FIXME
ec :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
ec c eqn = EC (qw c) EmptyS eqn
