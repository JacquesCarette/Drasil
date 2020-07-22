module Language.Drasil.Expr.Extract(dep, names, names', namesRI) where

import Data.List (nub)

import Language.Drasil.Expr (Expr(..))
import Language.Drasil.Space (RealInterval(..), Equal(..))

-- | Generic traverse of all positions that could lead to names
names :: Expr -> [String]
names (AssocA _ l)       = concatMap names l
names (AssocB _ l)       = concatMap names l
names (Deriv _ a b)      = b : names a
names (C c)              = [c]
names Int{}              = []
names Dbl{}              = []
names Str{}              = []
names Perc{}             = []
names (FCall f x ns)     = f : concatMap names x ++ map fst ns ++ 
                           concatMap (names . snd) ns
names (New c x ns)       = c : concatMap names x ++ map fst ns ++ 
                           concatMap (names . snd) ns
names (Message a m x ns) = a : m : concatMap names x ++ map fst ns ++ 
                           concatMap (names . snd) ns
names (Field o f)        = [o, f]
names (Case _ ls)        = concatMap (names . fst) ls ++ concatMap (names . snd) ls
names (UnaryOp _ u)      = names u
names (BinaryOp _ a b)   = names a ++ names b
names (Operator _ _ e)   = names e
names (IsIn  a _)        = names a
names (Matrix a)         = concatMap (concatMap names) a
names (RealI c b)        = c : namesRI b
names (Eql c d)          = c : namesEq d 

namesRI :: RealInterval Expr Expr -> [String]
namesRI (Bounded (_,il) (_,iu)) = names il ++ names iu
namesRI (UpTo (_,iu))       = names iu
namesRI (UpFrom (_,il))     = names il

namesEq :: Equal Expr -> [String]
namesEq (ExactlyEqual a) = names a
-- | Generic traverse of all positions that could lead to names, without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
names' :: Expr -> [String]
names' (AssocA _ l)       = concatMap names' l
names' (AssocB _ l)       = concatMap names' l
names' (Deriv _ a b)      = b : names' a
names' (C c)              = [c]
names' Int{}              = []
names' Dbl{}              = []
names' Str{}              = []
names' Perc{}             = []
names' (FCall _ x ns)     = concatMap names' x ++ map fst ns ++ 
                            concatMap (names .snd) ns
names' (New _ x ns)       = concatMap names' x ++ map fst ns ++ 
                            concatMap (names .snd) ns
names' (Message a _ x ns) = a : concatMap names' x ++ map fst ns ++ 
                            concatMap (names .snd) ns
names' (Field o f)        = [o, f]
names' (Case _ ls)        = concatMap (names' . fst) ls ++ 
                            concatMap (names' . snd) ls
names' (UnaryOp _ u)      = names' u
names' (BinaryOp _ a b)   = names' a ++ names' b
names' (Operator _ _ e)   = names' e
names' (IsIn  a _)        = names' a
names' (Matrix a)         = concatMap (concatMap names') a
names' (RealI c b)        = c : namesRI' b
names' (Eql c d)            = c : namesEq' d  

namesRI' :: RealInterval Expr Expr -> [String]
namesRI' (Bounded il iu) = names' (snd il) ++ names' (snd iu)
namesRI' (UpTo iu)       = names' (snd iu)
namesRI' (UpFrom il)     = names' (snd il)

namesEq' :: Equal Expr -> [String]
namesEq' (ExactlyEqual a) = names' a
---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation  
dep :: Expr -> [String]
dep = nub . names
