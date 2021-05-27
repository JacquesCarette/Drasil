{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', fromEqnSt,
  fromEqnSt', equat, mkQuantDef, mkQuantDef', ec) where

import Control.Lens ((^.), makeLenses, view)
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit))

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  IsUnit, DefiningExpr(defnExpr), Definition(defn), Quantity, HasSpace(typ), ExprRelat(relat), ConceptDomain(cdom))
import Language.Drasil.Chunk.Quantity (QuantityDict, mkQuant, mkQuant', qw)

import Language.Drasil.Expr (Expr, ($=))
import Language.Drasil.Expr.Math (sy)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UID (UID)

-- | A QDefinition is a 'QuantityDict' with a defining expression ('Expr'), a definition ('Sentence'), and a domain (['UID']).
data QDefinition = EC
  { _qua :: QuantityDict
  , _defn' :: Sentence
  , _equat :: Expr
  , cd :: [UID]
  }
makeLenses ''QDefinition

-- this works because UnitalChunk is a Chunk
instance HasUID        QDefinition where uid = qua . uid
-- ^ Finds the 'UID' of the 'QuantityDict' used to make the 'QDefinition'.
instance NamedIdea     QDefinition where term = qua . term
-- ^ Finds the term ('NP') of the 'QuantityDict' used to make the 'QDefinition'.
instance Idea          QDefinition where getA c = getA $ c ^. qua
-- ^ Finds the idea contained in the 'QuantityDict' used to make the 'QDefinition'.
instance HasSpace      QDefinition where typ = qua . typ
-- ^ Finds the 'Space' of the 'QuantityDict' used to make the 'QDefinition'.
instance HasSymbol     QDefinition where symbol e = symbol (e^.qua)
-- ^ Finds the 'Symbol' of the 'QuantityDict' used to make the 'QDefinition'.
instance Definition    QDefinition where defn = defn'
-- ^ Finds the definition of 'QDefinition'.
instance Quantity      QDefinition where
-- ^ 'QDefinition's have a 'Quantity'.
instance DefiningExpr  QDefinition where defnExpr = equat
-- ^ Finds the defining expression of 'QDefinition'.
instance Eq            QDefinition where a == b = (a ^. uid) == (b ^. uid)
-- ^ Equal if 'UID's are equal.
instance MayHaveUnit   QDefinition where getUnit = getUnit . view qua
-- ^ Finds the units of the 'QuantityDict' used to make the 'QDefinition'.
instance ExprRelat     QDefinition where relat z = sy z $= z ^. equat
-- ^ Finds the relation given by the expression in 'QDefinition'.
instance ConceptDomain QDefinition where cdom = cd
-- ^ Finds the domain of 'QDefinition'.

-- | Create a 'QDefinition' with a 'UID', term ('NP'), definition ('Sentence'), 'Symbol',
-- 'Space', unit, and defining expression.
fromEqn :: (IsUnit u) => String -> NP -> Sentence -> Symbol -> Space -> u -> Expr -> QDefinition
fromEqn nm desc def symb sp un expr =
  EC (mkQuant nm desc symb sp (Just $ unitWrapper un) Nothing) def expr []

-- | Same as 'fromEqn', but has no units.
fromEqn' :: String -> NP -> Sentence -> Symbol -> Space -> Expr -> QDefinition
fromEqn' nm desc def symb sp expr =
  EC (mkQuant nm desc symb sp Nothing Nothing) def expr []

-- | Same as 'fromEqn', but symbol depends on stage.
fromEqnSt :: (IsUnit u) => String -> NP -> Sentence -> (Stage -> Symbol) ->
  Space -> u -> Expr -> QDefinition
fromEqnSt nm desc def symb sp un expr =
  EC (mkQuant' nm desc Nothing sp symb (Just $ unitWrapper un)) def expr []

-- | Same as 'fromEqn', but symbol depends on stage and has no units.
fromEqnSt' :: String -> NP -> Sentence -> (Stage -> Symbol) -> Space -> Expr ->
  QDefinition
fromEqnSt' nm desc def symb sp expr =
  EC (mkQuant' nm desc Nothing sp symb Nothing) def expr []

-- | Used to help make 'Qdefinition's when 'UID', term, and 'Symbol' come from the same source.
mkQuantDef :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
mkQuantDef c e = datadef $ getUnit c
  where datadef (Just a) = fromEqnSt  (c ^. uid) (c ^. term) EmptyS (symbol c) (c ^. typ) a e
        datadef Nothing  = fromEqnSt' (c ^. uid) (c ^. term) EmptyS (symbol c) (c ^. typ) e

-- | Used to help make 'Qdefinition's when 'UID' and 'Symbol' come from the same source, with the term separate.
mkQuantDef' :: (Quantity c, MayHaveUnit c) => c -> NP -> Expr -> QDefinition
mkQuantDef' c t e = datadef $ getUnit c
  where datadef (Just a) = fromEqnSt  (c ^. uid) t EmptyS (symbol c) (c ^. typ) a e
        datadef Nothing  = fromEqnSt' (c ^. uid) t EmptyS (symbol c) (c ^. typ) e

-- HACK - makes the definition EmptyS !!! FIXME
-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation. 
ec :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
ec c eqn = EC (qw c) EmptyS eqn []
