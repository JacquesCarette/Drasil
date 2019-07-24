module Language.Drasil.Expr.Extract(dep, names, names', namesRI) where

import Data.List (nub)

import Language.Drasil.Expr (Expr(..))
import Language.Drasil.Space (RealInterval(..))

-- | Generic traverse of all positions that could lead to names
names :: Expr -> [String]
names (AssocA _ l)     = concatMap names l
names (AssocB _ l)     = concatMap names l
names (Deriv _ a b)    = b : names a
names (C c)            = [c]
names Int{}            = []
names Dbl{}            = []
names Str{}            = []
names Perc{}           = []
names (FCall f x)      = names f ++ concatMap names x
names (Case _ ls)      = concatMap (names . fst) ls ++ concatMap (names . snd) ls
names (UnaryOp _ u)    = names u
names (BinaryOp _ a b) = names a ++ names b
names (Operator _ _ e) = names e
names (IsIn  a _)      = names a
names (Matrix a)       = concatMap (concatMap names) a
names (RealI c b)      = c : namesRI b

namesRI :: RealInterval Expr Expr -> [String]
namesRI (Bounded (_,il) (_,iu)) = names il ++ names iu
namesRI (UpTo (_,iu))       = names iu
namesRI (UpFrom (_,il))     = names il

-- | Generic traverse of all positions that could lead to names, without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
names' :: Expr -> [String]
names' (AssocA _ l)     = concatMap names' l
names' (AssocB _ l)     = concatMap names' l
names' (Deriv _ a b)    = b : names' a
names' (C c)            = [c]
names' Int{}            = []
names' Dbl{}            = []
names' Str{}            = []
names' Perc{}           = []
names' (FCall _ x)      = concatMap names' x
names' (Case _ ls)      = concatMap (names' . fst) ls ++ concatMap (names' . snd) ls
names' (UnaryOp _ u)    = names' u
names' (BinaryOp _ a b) = names' a ++ names' b
names' (Operator _ _ e) = names' e
names' (IsIn  a _)      = names' a
names' (Matrix a)       = concatMap (concatMap names') a
names' (RealI c b)      = c : namesRI' b

namesRI' :: RealInterval Expr Expr -> [String]
namesRI' (Bounded il iu) = names' (snd il) ++ names' (snd iu)
namesRI' (UpTo iu)       = names' (snd iu)
namesRI' (UpFrom il)     = names' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation  
dep :: Expr -> [String]
dep = nub . names
