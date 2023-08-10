{-# LANGUAGE RankNTypes, FlexibleInstances, GADTs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Contains chunks related to adding an expression to a quantitative concept. 
module Language.Drasil.Chunk.Eq (
  -- * Types
  QDefinition,
  -- * Constructors
  fromEqn, fromEqn', fromEqnSt,
  fromEqnSt', fromEqnSt'', mkQDefSt, mkQuantDef, mkQuantDef', ec,
  mkFuncDef, mkFuncDef', mkFuncDefByQ
) where

import Control.Lens ((^.), view, lens, Lens', to)
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit), UnitDefn)

import Language.Drasil.Symbol (HasSymbol(symbol), Symbol)
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  IsUnit, DefiningExpr(defnExpr), Definition(defn), Quantity,
  ConceptDomain(cdom), Express(express))
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqd')
import Language.Drasil.Chunk.Concept (cc')
import Language.Drasil.Chunk.NamedIdea (ncUID, mkIdea, nw)
import Language.Drasil.Chunk.Quantity (DefinesQuantity(defLhs), qw)

import Language.Drasil.Expr.Lang (Expr)
import qualified Language.Drasil.Expr.Lang as E (Expr(C))
import Language.Drasil.Expr.Class (ExprC(apply, sy, ($=)))
import Language.Drasil.ModelExpr.Class (ModelExprC(defines))
import qualified Language.Drasil.ModelExpr.Lang as M (ModelExpr(C))
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Space (Space(..), HasSpace(..))
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.Stages (Stage)
import Language.Drasil.UID (UID, HasUID(..))
import Language.Drasil.WellTyped (RequiresChecking(..))

data QDefinition e where
  QD :: DefinedQuantityDict -> [UID] -> e -> QDefinition e

qdQua :: Lens' (QDefinition e) DefinedQuantityDict
qdQua = lens (\(QD qua _ _) -> qua) (\(QD _ ins e) qua' -> QD qua' ins e)

qdInputs :: Lens' (QDefinition e) [UID]
qdInputs = lens (\(QD _ ins _) -> ins) (\(QD qua _ e) ins' -> QD qua ins' e)

qdExpr :: Lens' (QDefinition e) e
qdExpr = lens (\(QD _ _ e) -> e) (\(QD qua ins _) e' -> QD qua ins e')

instance HasUID          (QDefinition e) where uid = qdQua . uid
instance NamedIdea       (QDefinition e) where term = qdQua . term
instance Idea            (QDefinition e) where getA = getA . (^. qdQua)
instance DefinesQuantity (QDefinition e) where defLhs = qdQua . to qw
instance HasSpace        (QDefinition e) where typ = qdQua . typ
instance HasSymbol       (QDefinition e) where symbol = symbol . (^. qdQua)
instance Definition      (QDefinition e) where defn = qdQua . defn
instance Quantity        (QDefinition e) where
instance Eq              (QDefinition e) where a == b = a ^. uid == b ^. uid
instance MayHaveUnit     (QDefinition e) where getUnit = getUnit . view qdQua
instance DefiningExpr     QDefinition    where defnExpr = qdExpr
instance Express e => Express (QDefinition e) where
  express q = f $ express $ q ^. defnExpr
    where
      f = case q ^. qdInputs of
        [] -> defines (sy q)
        is -> defines $ apply q (map M.C is)
        -- FIXME: The fact that we have to manually use `C` here is because our
        -- UID references don't carry enough information. This feels hacky at
        -- the moment, and should eventually be fixed.
instance ConceptDomain (QDefinition e) where cdom = cdom . view qdQua

instance RequiresChecking (QDefinition Expr) Expr Space where
  -- FIXME: Here, we are type-checking QDefinitions by building it as a relation
  -- and running the relation through the type-checker. We do this because the
  -- "normal" way does not work for Functions because it leaves function input
  -- parameters left unchecked. It's probably preferred to be doing type
  -- checking at time of chunk creation rather than here, really.
  requiredChecks (QD q is e) = pure (apply q (map E.C is) $= e, Boolean)

-- | Create a 'QDefinition' with a 'UID' (as a 'String'), term ('NP'), definition ('Sentence'), 'Symbol',
-- 'Space', unit, and defining expression.
fromEqn :: IsUnit u => String -> NP -> Sentence -> Symbol -> Space -> u -> e -> QDefinition e
fromEqn nm desc def symb sp un =
  QD (dqd (cc' (mkIdea nm desc Nothing) def) symb sp un) []

-- | Same as 'fromEqn', but has no units.
fromEqn' :: String -> NP -> Sentence -> Symbol -> Space -> e -> QDefinition e
fromEqn' nm desc def symb sp =
  QD (dqd' (cc' (mkIdea nm desc Nothing) def) (const symb) sp Nothing) []

-- | Same as 'fromEqn', but symbol depends on stage.
fromEqnSt :: IsUnit u => UID -> NP -> Sentence -> (Stage -> Symbol) ->
  Space -> u -> e -> QDefinition e
fromEqnSt nm desc def symb sp un =
  QD (dqd' (cc' (nw $ ncUID nm desc) def) symb sp (Just $ unitWrapper un)) []

-- | Same as 'fromEqn', but symbol depends on stage and has no units.
fromEqnSt' :: UID -> NP -> Sentence -> (Stage -> Symbol) -> Space -> e -> QDefinition e
fromEqnSt' nm desc def symb sp =
  QD (dqd' (cc' (nw $ ncUID nm desc) def) symb sp Nothing) []

-- | Same as 'fromEqnSt'', but takes a 'String' instead of a 'UID'.
fromEqnSt'' :: String -> NP -> Sentence -> (Stage -> Symbol) -> Space -> e ->
  QDefinition e
fromEqnSt'' nm desc def symb sp =
  QD (dqd' (cc' (mkIdea nm desc Nothing) def) symb sp Nothing) []

-- | Wrapper for fromEqnSt and fromEqnSt'
mkQDefSt :: UID -> NP -> Sentence -> (Stage -> Symbol) -> Space ->
  Maybe UnitDefn -> e -> QDefinition e
mkQDefSt u n s symb sp (Just ud) e = fromEqnSt u n s symb sp ud e
mkQDefSt u n s symb sp Nothing   e = fromEqnSt' u n s symb sp e

-- | Used to help make 'QDefinition's when 'UID', term, and 'Symbol' come from the same source.
mkQuantDef :: (Quantity c, MayHaveUnit c) => c -> e -> QDefinition e
mkQuantDef c = mkQDefSt (c ^. uid) (c ^. term) EmptyS (symbol c) (c ^. typ) (getUnit c)

-- FIXME: See #2788.
-- | Used to help make 'QDefinition's when 'UID' and 'Symbol' come from the same source, with the term separate.
mkQuantDef' :: (Quantity c, MayHaveUnit c) => c -> NP -> e -> QDefinition e
mkQuantDef' c t = mkQDefSt (c ^. uid) t EmptyS (symbol c) (c ^. typ) (getUnit c)

-- HACK - makes the definition EmptyS !!! FIXME
-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation. 
ec :: (Quantity c, MayHaveUnit c) => c -> e -> QDefinition e
ec c = QD (dqd' (cc' (nw c) EmptyS) (symbol c) (c ^. typ) (getUnit c)) []

-- | Factored version of 'QDefinition' functions.
mkFuncDef0 :: (HasUID f, HasSymbol f, HasSpace f,
               HasUID i, HasSymbol i, HasSpace i) =>
  f -> NP -> Sentence -> Maybe UnitDefn -> [i] -> e -> QDefinition e
mkFuncDef0 f n s u is = QD
  (dqd' (cc' (nw (ncUID (f ^. uid) n)) s) (symbol f)
    (f ^. typ) u) (map (^. uid) is)
    -- (mkFunction (map (^. typ) is) (f ^. typ)) u) (map (^. uid) is)

-- | Create a 'QDefinition' function with a symbol, name, term, list of inputs,
-- resultant units, and a defining Expr
mkFuncDef :: (HasUID f, HasSymbol f, HasSpace f,
              HasUID i, HasSymbol i, HasSpace i,
              IsUnit u) =>
  f -> NP -> Sentence -> u -> [i] -> e -> QDefinition e
mkFuncDef f n s u = mkFuncDef0 f n s (Just $ unitWrapper u)

-- | Create a 'QDefinition' function with a symbol, name, term, list of inputs,
-- and a defining Expr
mkFuncDef' :: (HasUID f, HasSymbol f, HasSpace f,
               HasUID i, HasSymbol i, HasSpace i) =>
  f -> NP -> Sentence -> [i] -> e -> QDefinition e
mkFuncDef' f n s = mkFuncDef0 f n s Nothing

-- | Create a 'QDefinition' functions using a symbol, list of inputs, and a
-- defining Expr
mkFuncDefByQ :: (Quantity c, MayHaveUnit c, HasSpace c,
                 Quantity i, HasSpace i) =>
  c -> [i] -> e -> QDefinition e
mkFuncDefByQ f = case getUnit f of
  Just u  -> mkFuncDef  f (f ^. term) EmptyS u
  Nothing -> mkFuncDef' f (f ^. term) EmptyS
