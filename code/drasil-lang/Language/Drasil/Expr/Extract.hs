module Language.Drasil.Expr.Extract where

import Data.List (nub)
import qualified Data.List.NonEmpty as NE

import Language.Drasil.Expr (Expr(..))
import Language.Drasil.Expr.Display
import Language.Drasil.Space (RealInterval(..))

deNames :: DisplayExpr -> [String]
deNames (AlgebraicExpr e) = eNames e
deNames (Defines l r)     = deNames l ++ deNames r
deNames (MultiExpr des)   = concatMap deNames des

-- | Generic traverse of all expressions that could lead to names.
eNames :: Expr -> [String]
eNames (AssocA _ l)          = concatMap eNames l
eNames (AssocB _ l)          = concatMap eNames l
eNames (Deriv _ a b)         = b : eNames a
eNames (C c)                 = [c]
eNames Int{}                 = []
eNames Dbl{}                 = []
eNames ExactDbl{}            = []
eNames Str{}                 = []
eNames Perc{}                = []
eNames (FCall f x ns)        = f : concatMap eNames x ++ map fst ns ++ 
                              concatMap (eNames . snd) ns
eNames (New c x ns)          = c : concatMap eNames x ++ map fst ns ++ 
                              concatMap (eNames . snd) ns
eNames (Message a m x ns)    = a : m : concatMap eNames x ++ map fst ns ++ 
                              concatMap (eNames . snd) ns
eNames (Field o f)           = [o, f]
eNames (Case _ ls)           = concatMap (eNames . fst) ls ++ concatMap (eNames . snd) ls
eNames (UnaryOp _ u)         = eNames u
eNames (UnaryOpB _ u)        = eNames u
eNames (UnaryOpVec _ u)      = eNames u
eNames (ArithBinaryOp _ a b) = eNames a ++ eNames b
eNames (BoolBinaryOp _ a b)  = eNames a ++ eNames b
eNames (EqBinaryOp _ a b)    = eNames a ++ eNames b
eNames (LABinaryOp _ a b)    = eNames a ++ eNames b
eNames (OrdBinaryOp _ a b)   = eNames a ++ eNames b
eNames (VVVBinaryOp _ a b)   = eNames a ++ eNames b
eNames (VVNBinaryOp _ a b)   = eNames a ++ eNames b
eNames (Operator _ _ e)      = eNames e
eNames (IsIn  a _)           = eNames a
eNames (Matrix a)            = concatMap (concatMap eNames) a
eNames (RealI c b)           = c : eNamesRI b

-- | Generic traversal of everything that could come from an interval to names (similar to 'names').
eNamesRI :: RealInterval Expr Expr -> [String]
eNamesRI (Bounded (_,il) (_,iu)) = eNames il ++ eNames iu
eNamesRI (UpTo (_,iu))           = eNames iu
eNamesRI (UpFrom (_,il))         = eNames il


deNames' :: DisplayExpr -> [String]
deNames' (AlgebraicExpr e) = eNames e
deNames' (Defines l r)     = deNames' l ++ deNames' r
deNames' (MultiExpr des)   = concatMap deNames' des

-- | Generic traverse of all positions that could lead to eNames without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
eNames' :: Expr -> [String]
eNames' (AssocA _ l)          = concatMap eNames' l
eNames' (AssocB _ l)          = concatMap eNames' l
eNames' (Deriv _ a b)         = b : eNames' a
eNames' (C c)                 = [c]
eNames' Int{}                 = []
eNames' Dbl{}                 = []
eNames' ExactDbl{}            = []
eNames' Str{}                 = []
eNames' Perc{}                = []
eNames' (FCall _ x ns)        = concatMap eNames' x ++ map fst ns ++ 
                               concatMap (eNames .snd) ns
eNames' (New _ x ns)          = concatMap eNames' x ++ map fst ns ++ 
                               concatMap (eNames .snd) ns
eNames' (Message a _ x ns)    = a : concatMap eNames' x ++ map fst ns ++ 
                               concatMap (eNames .snd) ns
eNames' (Field o f)           = [o, f]
eNames' (Case _ ls)           = concatMap (eNames' . fst) ls ++ 
                               concatMap (eNames' . snd) ls
eNames' (UnaryOp _ u)         = eNames' u
eNames' (UnaryOpB _ u)        = eNames' u
eNames' (UnaryOpVec _ u)      = eNames' u
eNames' (ArithBinaryOp _ a b) = eNames' a ++ eNames' b
eNames' (BoolBinaryOp _ a b)  = eNames' a ++ eNames' b
eNames' (EqBinaryOp _ a b)    = eNames' a ++ eNames' b
eNames' (LABinaryOp _ a b)    = eNames' a ++ eNames' b
eNames' (OrdBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (VVVBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (VVNBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (Operator _ _ e)      = eNames' e
eNames' (IsIn  a _)           = eNames' a
eNames' (Matrix a)            = concatMap (concatMap eNames') a
eNames' (RealI c b)           = c : eNamesRI' b

-- | Generic traversal of everything that could come from an interval to names without functions (similar to 'names'').
eNamesRI' :: RealInterval Expr Expr -> [String]
eNamesRI' (Bounded il iu) = eNames' (snd il) ++ eNames' (snd iu)
eNamesRI' (UpTo iu)       = eNames' (snd iu)
eNamesRI' (UpFrom il)     = eNames' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation.  
eDep :: Expr -> [String]
eDep = nub . eNames

-- | Get dependencies from display expressions.
deDep :: DisplayExpr -> [String]
deDep (AlgebraicExpr e) = eDep e
deDep (Defines l r)     = nub $ deDep l ++ deDep r
deDep (MultiExpr des)   = nub $ concat $ NE.map deDep des
