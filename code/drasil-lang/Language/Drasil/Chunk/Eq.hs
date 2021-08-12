{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fprint-potential-instances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Language.Drasil.Chunk.Eq where

import Control.Lens ((^.), view, lens, Lens')
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit), UnitDefn)

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  IsUnit, DefiningExpr(defnExpr), Definition(defn), Quantity, HasSpace(typ),
  ConceptDomain(cdom), Express(express))
import Language.Drasil.Chunk.Quantity (QuantityDict, mkQuant, mkQuant', qw)

import Language.Drasil.ModelExpr (defines)
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.Expr.Lang (Expr(FCall, C))
import Language.Drasil.Expr (sy)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Space (mkFunction, Space)
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UID (UID)

data QDefinition e where
  QD :: Express e => QuantityDict -> Sentence -> [UID] -> e -> [UID] -> QDefinition e

qdQua :: Express e => Lens' (QDefinition e) QuantityDict
qdQua = lens (\(QD qua _ _ _ _) -> qua) (\(QD _ s ins e cd) qua' -> QD qua' s ins e cd)

qdDefn :: Express e => Lens' (QDefinition e) Sentence
qdDefn = lens (\(QD _ s _ _ _) -> s) (\(QD qua _ ins e cd) s' -> QD qua s' ins e cd)

qdInputs :: Express e => Lens' (QDefinition e) [UID]
qdInputs = lens (\(QD _ _ ins _ _) -> ins) (\(QD qua s _ e cd) ins' -> QD qua s ins' e cd)

qdExpr :: Express e => Lens' (QDefinition e) e
qdExpr = lens (\(QD _ _ _ e _) -> e) (\(QD qua s ins _ cd) e' -> QD qua s ins e' cd)

qdCD :: Express e => Lens' (QDefinition e) [UID]
qdCD = lens (\(QD _ _ _ _ cd) -> cd) (\(QD qua s ins e _) cd' -> QD qua s ins e cd')
  

instance Express e => HasUID        (QDefinition e) where uid = qdQua . uid
instance Express e => NamedIdea     (QDefinition e) where term = qdQua . term
instance Express e => Idea          (QDefinition e) where getA = getA . (^. qdQua)
instance Express e => HasSpace      (QDefinition e) where typ = qdQua . typ
instance Express e => HasSymbol     (QDefinition e) where symbol = symbol . (^. qdQua)
instance Express e => Definition    (QDefinition e) where defn = qdDefn
instance Express e => Quantity      (QDefinition e) where
instance Express e => Eq            (QDefinition e) where a == b = (a ^. uid) == (b ^. uid)
instance Express e => MayHaveUnit   (QDefinition e) where getUnit = getUnit . view qdQua
instance DefiningExpr  QDefinition where
  defnExpr = qdExpr
instance Express e => Express       (QDefinition e) where
  express q = f $ express $ q ^. defnExpr
    where
      f :: Express g => g -> ModelExpr
      f = case q ^. qdInputs of
        [] -> defines (sy q)
        is -> defines (FCall (q ^. uid) (map C is) [])
instance Express e => ConceptDomain (QDefinition e) where cdom = (^. qdCD)

-- data QDefinition e = Express e => QD
--   { _qdQua    :: QuantityDict
--   , _qdDefn   :: Sentence
--   , _qdInputs :: [UID]
--   , _qdExpr   :: Express e => e
--   , _qdCD     :: [UID]
--   }

-- makeLenses ''QDefinition

-- elimQD_E :: Express e => QDefinition e -> e
-- elimQD_E = _qdExpr

-- setQD_E :: Express e => QDefinition e -> e -> QDefinition e
-- setQD_E QD
--   { _qdQua = a
--   , _qdDefn = b
--   , _qdInputs = c
--   , _qdCD = d
--   } e = QD a b c e d

-- instance DefiningExpr QDefinition where
--   defnExpr = lens elimQD_E setQD_E

-- instance Contravariant QDefinition where
--   contramap :: (a -> b) -> QDefinition b -> QDefinition a
--   contramap f QD 
--     { _qdQua = a
--     , _qdDefn = b
--     , _qdInputs = c
--     , _qdExpr = d
--     , _qdCD = e
--     } = _ -- QD a b c (f d) e -- QD a b c d e
  

-- instance HasUID (QDefinition e) where uid = qdQua . uid
-- instance NamedIdea     (QDefinition e) where term = qdQua . term
-- instance Idea          (QDefinition e) where getA = getA . (^. qdQua)
-- instance HasSpace      (QDefinition e) where typ = qdQua . typ
-- instance HasSymbol     (QDefinition e) where symbol = symbol . (^. qdQua)
-- instance Definition    (QDefinition e) where defn = qdDefn
-- instance Quantity      (QDefinition e) where
-- instance Eq            (QDefinition e) where a == b = (a ^. uid) == (b ^. uid)
-- instance MayHaveUnit   (QDefinition e) where getUnit = getUnit . view qdQua
-- instance Express e => Express       (QDefinition e) where
--   express q = f (express $ q ^. defnExpr)
--     where
--       f = case q ^. qdInputs of
--         [] -> defines (sy q)
--         is -> defines (FCall (q ^. uid) (map C is) [])
-- instance ConceptDomain (QDefinition e) where cdom = (^. qdCD)


-- | Create a 'QDefinition' with a 'UID', term ('NP'), definition ('Sentence'), 'Symbol',
-- 'Space', unit, and defining expression.
fromEqn :: (Express e, IsUnit u) => String -> NP -> Sentence -> Symbol -> Space -> u -> e -> QDefinition e
fromEqn nm desc def symb sp un expr =
  QD (mkQuant nm desc symb sp (Just $ unitWrapper un) Nothing) def [] expr []

-- | Same as 'fromEqn', but has no units.
fromEqn' :: Express e => String -> NP -> Sentence -> Symbol -> Space -> e -> QDefinition e
fromEqn' nm desc def symb sp expr =
  QD (mkQuant nm desc symb sp Nothing Nothing) def [] expr []

-- | Same as 'fromEqn', but symbol depends on stage.
fromEqnSt :: (Express e, IsUnit u) => String -> NP -> Sentence -> (Stage -> Symbol) ->
  Space -> u -> e -> QDefinition e
fromEqnSt nm desc def symb sp un expr =
  QD (mkQuant' nm desc Nothing sp symb (Just $ unitWrapper un)) def [] expr []

-- | Same as 'fromEqn', but symbol depends on stage and has no units.
fromEqnSt' :: Express e => String -> NP -> Sentence -> (Stage -> Symbol) -> Space -> e ->
  QDefinition e
fromEqnSt' nm desc def symb sp expr =
  QD (mkQuant' nm desc Nothing sp symb Nothing) def [] expr []

-- | Wrapper for fromEqnSt and fromEqnSt'
mkQDefSt :: Express e => UID -> NP -> Sentence -> (Stage -> Symbol) -> Space ->
  Maybe UnitDefn -> e -> QDefinition e
mkQDefSt u n s symb sp (Just ud) e = fromEqnSt u n s symb sp ud e
mkQDefSt u n s symb sp Nothing   e = fromEqnSt' u n s symb sp e

-- | Used to help make 'QDefinition's when 'UID', term, and 'Symbol' come from the same source.
mkQuantDef :: (Quantity c, MayHaveUnit c, Express e) => c -> e -> QDefinition e
mkQuantDef c = mkQDefSt (c ^. uid) (c ^. term) EmptyS (symbol c) (c ^. typ) (getUnit c)

-- | Used to help make 'QDefinition's when 'UID' and 'Symbol' come from the same source, with the term separate.
mkQuantDef' :: (Quantity c, MayHaveUnit c, Express e) => c -> NP -> e -> QDefinition e
mkQuantDef' c t = mkQDefSt (c ^. uid) t EmptyS (symbol c) (c ^. typ) (getUnit c)

-- HACK - makes the definition EmptyS !!! FIXME
-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation. 
ec :: (Quantity c, MayHaveUnit c, Express e) => c -> e -> QDefinition e
ec c eqn = QD (qw c) EmptyS [] eqn []

-- | Factored version of 'QDefinition' functions
mkFuncDef0 :: (HasUID f, HasSymbol f, HasSpace f,
               HasUID i, HasSymbol i, HasSpace i,
               Express e) =>
  f -> NP -> Sentence -> Maybe UnitDefn -> [i] -> e -> QDefinition e
mkFuncDef0 f n s u is e = QD
  (mkQuant' (f ^. uid) n Nothing (mkFunction (map (^. typ) is) (f ^. typ)) (symbol f) u)
  s (map (^. uid) is) e []

-- | Create a 'QDefinition' function with a symbol, name, term, list of inputs, resultant units, and a defining Expr
mkFuncDef :: (HasUID f, HasSymbol f, HasSpace f,
              HasUID i, HasSymbol i, HasSpace i,
              IsUnit u, Express e) =>
  f -> NP -> Sentence -> u -> [i] -> e -> QDefinition e
mkFuncDef f n s u = mkFuncDef0 f n s (Just $ unitWrapper u)

-- | Create a 'QDefinition' function with a symbol, name, term, list of inputs, and a defining Expr
mkFuncDef' :: (HasUID f, HasSymbol f, HasSpace f,
               HasUID i, HasSymbol i, HasSpace i,
               Express e) =>
  f -> NP -> Sentence -> [i] -> e -> QDefinition e
mkFuncDef' f n s = mkFuncDef0 f n s Nothing

-- | Create a 'QDefinition' functions using a symbol, list of inputs, and a defining Expr
mkFuncDefByQ :: (Quantity c, MayHaveUnit c, HasSpace c,
                 Quantity i, HasSpace i,
                 Express e) =>
  c -> [i] -> e -> QDefinition e
mkFuncDefByQ f = case getUnit f of
  Just u  -> mkFuncDef  f (f ^. term) EmptyS u
  Nothing -> mkFuncDef' f (f ^. term) EmptyS

