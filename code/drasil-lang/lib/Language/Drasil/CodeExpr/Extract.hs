module Language.Drasil.CodeExpr.Extract (
    eDep, eDep',
    eNamesRI, eNamesRI'
) where

import Language.Drasil.Space (RealInterval(..))
import Language.Drasil.UID (UID)

import Language.Drasil.CodeExpr.Lang (CodeExpr(..))

import Data.List (nub)
import Data.Set (Set, union, unions, insert, singleton, empty, fromList, toList)

-- | Generic traverse of all expressions that could lead to names.
eNames :: CodeExpr -> Set UID
eNames (AssocA _ l)          = unions (map eNames l)
eNames (AssocB _ l)          = unions (map eNames l)
eNames (C c)                 = singleton c
eNames Lit{}                 = empty
eNames (FCall f x ns)        = unions [singleton f, unions (map eNames x), fromList (map fst ns), unions (map (eNames . snd) ns)]
eNames (New c x ns)          = unions [singleton c, unions (map eNames x), fromList (map fst ns), unions (map (eNames . snd) ns)]
eNames (Message a m x ns)    = unions [singleton a, singleton m, unions (map eNames x), fromList (map fst ns), unions (map (eNames . snd) ns)]
eNames (Field o f)           = fromList [o, f]
eNames (Case _ ls)           = unions [unions (map (eNames . fst) ls), unions (map (eNames . snd) ls)]
eNames (UnaryOp _ u)         = eNames u
eNames (UnaryOpB _ u)        = eNames u
eNames (UnaryOpVV _ u)       = eNames u
eNames (UnaryOpVN _ u)       = eNames u
eNames (ArithBinaryOp _ a b) = eNames a `union` eNames b
eNames (BoolBinaryOp _ a b)  = eNames a `union` eNames b
eNames (EqBinaryOp _ a b)    = eNames a `union` eNames b
eNames (LABinaryOp _ a b)    = eNames a `union` eNames b
eNames (OrdBinaryOp _ a b)   = eNames a `union` eNames b
eNames (VVVBinaryOp _ a b)   = eNames a `union` eNames b
eNames (VVNBinaryOp _ a b)   = eNames a `union` eNames b
eNames (NVVBinaryOp _ a b)   = eNames a `union` eNames b
eNames (Operator _ _ e)      = eNames e
eNames (Matrix a)            = unions (map (unions . map eNames) a)
eNames (RealI c b)           = insert c (eNamesRI b)

-- | Generic traversal of everything that could come from an interval to names (similar to 'eNames').
eNamesRI :: RealInterval CodeExpr CodeExpr -> Set UID
eNamesRI (Bounded (_, il) (_, iu)) = eNames il `union` eNames iu
eNamesRI (UpTo (_, iu))            = eNames iu
eNamesRI (UpFrom (_, il))          = eNames il

-- | Generic traverse of all positions that could lead to 'eNames' without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
eNames' :: CodeExpr -> [UID]
eNames' (AssocA _ l)          = concatMap eNames' l
eNames' (AssocB _ l)          = concatMap eNames' l
eNames' (C c)                 = [c]
eNames' Lit{}                 = []
eNames' (FCall _ x ns)        = concatMap eNames' x ++ map fst ns ++ 
                               concatMap (toList . eNames .snd) ns
eNames' (New _ x ns)          = concatMap eNames' x ++ map fst ns ++ 
                               concatMap (toList . eNames .snd) ns
eNames' (Message a _ x ns)    = a : concatMap eNames' x ++ map fst ns ++ 
                               concatMap (toList . eNames .snd) ns
eNames' (Field o f)           = [o, f]
eNames' (Case _ ls)           = concatMap (eNames' . fst) ls ++ 
                               concatMap (eNames' . snd) ls
eNames' (UnaryOp _ u)         = eNames' u
eNames' (UnaryOpB _ u)        = eNames' u
eNames' (UnaryOpVV _ u)       = eNames' u
eNames' (UnaryOpVN _ u)       = eNames' u
eNames' (ArithBinaryOp _ a b) = eNames' a ++ eNames' b
eNames' (BoolBinaryOp _ a b)  = eNames' a ++ eNames' b
eNames' (EqBinaryOp _ a b)    = eNames' a ++ eNames' b
eNames' (LABinaryOp _ a b)    = eNames' a ++ eNames' b
eNames' (OrdBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (VVVBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (VVNBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (NVVBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (Operator _ _ e)      = eNames' e
eNames' (Matrix a)            = concatMap (concatMap eNames') a
eNames' (RealI c b)           = c : eNamesRI' b

-- | Generic traversal of everything that could come from an interval to names without functions (similar to 'eNames'').
eNamesRI' :: RealInterval CodeExpr CodeExpr -> [UID]
eNamesRI' (Bounded il iu) = eNames' (snd il) ++ eNames' (snd iu)
eNamesRI' (UpTo iu)       = eNames' (snd iu)
eNamesRI' (UpFrom il)     = eNames' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation.
eDep :: CodeExpr -> Set UID
eDep = eNames

-- | Get dependencies from an equation, without functions.
eDep' :: CodeExpr -> [UID]
eDep' = nub . eNames'
