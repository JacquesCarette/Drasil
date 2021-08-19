{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Contains chunks related to adding an expression to a quantitative concept. 
module Language.Drasil.Chunk.Eq (
  -- * Chunk Type
  QDefinition,
  -- * Constructor
  fromEqn, fromEqn', fromEqnSt,
  fromEqnSt', mkQDefSt, mkQuantDef, mkQuantDef', ec,
  mkFuncDef, mkFuncDef', mkFuncDefByQ) where

import Control.Lens ((^.), makeLenses, view)
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit), UnitDefn)

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  IsUnit, DefiningExpr(defnExpr), Definition(defn), Quantity, HasSpace(typ),
  ConceptDomain(cdom), Express(express))
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqd')
import Language.Drasil.Chunk.Concept (cc')
import Language.Drasil.Chunk.NamedIdea (mkIdea, nw)

import Language.Drasil.ModelExpr.Math (defines)
import Language.Drasil.Expr (Expr(FCall, C))
import Language.Drasil.Expr.Math (sy)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Space (mkFunction, Space)
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UID (UID(..))

-- | A QDefinition is a 'QuantityDict' with a defining expression ('Expr'), a definition ('Sentence'), and a domain (['UID']).
-- This chunk now contains almost enough information to generate code, definitions, and models.
-- Used for high-level quantities with equations and inputs.
data QDefinition = EC
  { _qua    :: DefinedQuantityDict
  , _inputs :: [UID]
  , _equat  :: Expr
  }
makeLenses ''QDefinition

-- | Finds the 'UID' of the 'QuantityDict' used to make the 'QDefinition'.
instance HasUID        QDefinition where uid = qua . uid
-- | Finds the term ('NP') of the 'QuantityDict' used to make the 'QDefinition'.
instance NamedIdea     QDefinition where term = qua . term
-- | Finds the idea contained in the 'QuantityDict' used to make the 'QDefinition'.
instance Idea          QDefinition where getA = getA . (^. qua)
-- | Finds the 'Space' of the 'QuantityDict' used to make the 'QDefinition'.
instance HasSpace      QDefinition where typ = qua . typ
-- | Finds the 'Symbol' of the 'QuantityDict' used to make the 'QDefinition'.
instance HasSymbol     QDefinition where symbol = symbol . (^. qua)
-- | Finds the definition of 'QDefinition'.
instance Definition    QDefinition where defn = qua . defn
-- | 'QDefinition's have a 'Quantity'.
instance Quantity      QDefinition where
-- | Finds the defining expression of 'QDefinition'.
instance DefiningExpr  QDefinition where defnExpr = equat
-- | Equal if 'UID's are equal.
instance Eq            QDefinition where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the units of the 'QuantityDict' used to make the 'QDefinition'.
instance MayHaveUnit   QDefinition where getUnit = getUnit . view qua
-- | Displays the relation given by the expression in 'QDefinition'.
instance Express       QDefinition where
  express q = f (q ^. defnExpr)
    where
      f = case q ^. inputs of
        [] -> defines (sy q)
        is -> defines (FCall (q ^. uid) (map C is) [])
-- | Finds the domain of a 'QDefinition'.
instance ConceptDomain QDefinition where cdom = cdom . view qua

-- | Create a 'QDefinition' with a 'UID' (as a 'String'), term ('NP'), definition ('Sentence'), 'Symbol',
-- 'Space', unit, and defining expression.
fromEqn :: IsUnit u => String -> NP -> Sentence -> Symbol -> Space -> u -> Expr -> QDefinition
fromEqn nm desc def symb sp un =
  EC (dqd (cc' (mkIdea nm desc Nothing) def) symb sp un) []

-- | Same as 'fromEqn', but has no units.
fromEqn' :: String -> NP -> Sentence -> Symbol -> Space -> Expr -> QDefinition
fromEqn' nm desc def symb sp =
  EC (dqd' (cc' (mkIdea nm desc Nothing) def) (const symb) sp Nothing) []

-- | Same as 'fromEqn', but symbol depends on stage.
fromEqnSt :: IsUnit u => String -> NP -> Sentence -> (Stage -> Symbol) ->
  Space -> u -> Expr -> QDefinition
fromEqnSt nm desc def symb sp un =
  EC (dqd' (cc' (mkIdea nm desc Nothing) def) symb sp (Just $ unitWrapper un)) []

-- | Same as 'fromEqn', but symbol depends on stage and has no units.
fromEqnSt' :: String -> NP -> Sentence -> (Stage -> Symbol) -> Space -> Expr ->
  QDefinition
fromEqnSt' nm desc def symb sp =
  EC (dqd' (cc' (mkIdea nm desc Nothing) def) symb sp Nothing) []

-- | Wrapper for fromEqnSt and fromEqnSt'
mkQDefSt :: String -> NP -> Sentence -> (Stage -> Symbol) -> Space ->
  Maybe UnitDefn -> Expr -> QDefinition
mkQDefSt u n s symb sp (Just ud) e = fromEqnSt u n s symb sp ud e
mkQDefSt u n s symb sp Nothing   e = fromEqnSt' u n s symb sp e

-- | Used to help make 'QDefinition's when 'UID', term, and 'Symbol' come from the same source.
mkQuantDef :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
mkQuantDef c = mkQDefSt (uidToStr $ c ^. uid) (c ^. term) EmptyS (symbol c) (c ^. typ) (getUnit c)

-- FIXME: See #2788.
-- | Used to help make 'QDefinition's when 'UID' and 'Symbol' come from the same source, with the term separate.
mkQuantDef' :: (Quantity c, MayHaveUnit c) => c -> NP -> Expr -> QDefinition
mkQuantDef' c t = mkQDefSt (uidToStr $ c ^. uid) t EmptyS (symbol c) (c ^. typ) (getUnit c)

-- HACK - makes the definition EmptyS !!! FIXME
-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation. 
ec :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
ec c = EC (dqd' (cc' (nw c) EmptyS) (symbol c) (c ^. typ) (getUnit c)) []

-- FIXME: uidToStr shouldn't be used here.
-- | Factored version of 'QDefinition' functions.
mkFuncDef0 :: (HasUID f, HasSymbol f, HasSpace f,
                HasUID i, HasSymbol i, HasSpace i) =>
  f -> NP -> Sentence -> Maybe UnitDefn -> [i] -> Expr -> QDefinition
mkFuncDef0 f n s u is =
  EC (dqd' (cc' (mkIdea (uidToStr $ f ^. uid) n Nothing) s) (symbol f)
    (mkFunction (map (^. typ) is) (f ^. typ)) u) (map (^. uid) is)

-- | Create a 'QDefinition' function with a symbol, name, term, list of inputs, resultant units, and a defining Expr
mkFuncDef :: (HasUID f, HasSymbol f, HasSpace f,
               HasUID i, HasSymbol i, HasSpace i,
               IsUnit u) =>
  f -> NP -> Sentence -> u -> [i] -> Expr -> QDefinition
mkFuncDef f n s u = mkFuncDef0 f n s (Just $ unitWrapper u)

-- | Create a 'QDefinition' function with a symbol, name, term, list of inputs, and a defining Expr
mkFuncDef' :: (HasUID f, HasSymbol f, HasSpace f,
                HasUID i, HasSymbol i, HasSpace i) =>
  f -> NP -> Sentence -> [i] -> Expr -> QDefinition
mkFuncDef' f n s = mkFuncDef0 f n s Nothing

-- | Create a 'QDefinition' functions using a symbol, list of inputs, and a defining Expr
mkFuncDefByQ :: (Quantity c, MayHaveUnit c, HasSpace c,
                  Quantity i, HasSpace i) =>
  c -> [i] -> Expr -> QDefinition
mkFuncDefByQ f = case getUnit f of
  Just u  -> mkFuncDef  f (f ^. term) EmptyS u
  Nothing -> mkFuncDef' f (f ^. term) EmptyS

