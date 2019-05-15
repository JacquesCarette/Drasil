module Language.Drasil.Expr.Extract(dep, names, names') where

import Data.List (nub)

import Language.Drasil.Expr (Expr(..))
import Language.Drasil.Space (RealInterval(..))

-- | Generic traverse of all positions that could lead to names
names :: Expr -> [String]
names (AssocA _ l)   = concatMap names l
names (AssocB _ l)   = concatMap names l
names (Deriv _ a b) = b : names a
names (C c)         = [c]
names (Int _)       = []
names (Dbl _)       = []
names (Str _)       = []
names (FCall f x)   = names f ++ (concatMap names x)
names (Case ls)     = (concatMap (names . fst) ls) ++ concatMap (names . snd) ls
names (UnaryOp _ u) = names u
names (BinaryOp _ a b)  = names a ++ names b
names (Operator _ _ e)  = names e
names (IsIn  a _)   = names a
names (Matrix a)    = concatMap (concatMap names) a
names (RealI c b)   = c : names_ri b

names_ri :: RealInterval Expr Expr -> [String]
names_ri (Bounded (_,il) (_,iu)) = names il ++ names iu
names_ri (UpTo (_,iu))       = names iu
names_ri (UpFrom (_,il))     = names il

-- | Generic traverse of all positions that could lead to names, without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
names' :: Expr -> [String]
names' (AssocA _ l)   = concatMap names' l
names' (AssocB _ l)   = concatMap names' l
names' (Deriv _ a b) = b : names' a
names' (C c)         = [c]
names' (Int _)       = []
names' (Dbl _)       = []
names' (Str _)       = []
names' (FCall _ x)   = concatMap names' x
names' (Case ls)     = (concatMap (names' . fst) ls) ++ concatMap (names' . snd) ls
names' (UnaryOp _ u) = names' u
names' (BinaryOp _ a b)  = names' a ++ names' b
names' (Operator _ _ e)  = names' e
names' (IsIn  a _)   = names' a
names' (Matrix a)    = concatMap (concatMap names') a
names' (RealI c b)   = c : names'_ri b

names'_ri :: RealInterval Expr Expr -> [String]
names'_ri (Bounded il iu) = names' (snd il) ++ names' (snd iu)
names'_ri (UpTo iu)       = names' (snd iu)
names'_ri (UpFrom il)     = names' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation  
dep :: Expr -> [String]
dep = nub . names
