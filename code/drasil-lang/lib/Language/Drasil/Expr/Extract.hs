-- | Extract UIDs from an expression so that they can be looked up in the chunk database and rendered.
module Language.Drasil.Expr.Extract where

import Data.Set (Set, union, unions, insert, singleton, empty)

import Language.Drasil.Expr.Lang (Expr(..))
import Language.Drasil.Space (RealInterval(..))
import Language.Drasil.UID (UID)

-- | Generic traverse of all expressions that could lead to names.
eNames :: Expr -> Set UID
eNames (AssocA _ l)          = unions (map eNames l)
eNames (AssocB _ l)          = unions (map eNames l)
eNames (C c)                 = singleton c
eNames Lit{}                 = empty
eNames (FCall f x)           = insert f (unions (map eNames x))
eNames (Case _ ls)           = unions (map (eNames . fst) ls) `union`
                               unions (map (eNames . snd) ls)
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
eNamesRI :: RealInterval Expr Expr -> Set UID
eNamesRI (Bounded (_, il) (_, iu)) = eNames il `union` eNames iu
eNamesRI (UpTo (_, iu))            = eNames iu
eNamesRI (UpFrom (_, il))          = eNames il

-- | Generic traverse of all positions that could lead to 'eNames' without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
eNames' :: Expr -> Set UID
eNames' (AssocA _ l)          = unions (map eNames' l)
eNames' (AssocB _ l)          = unions (map eNames' l)
eNames' (C c)                 = singleton c
eNames' Lit{}                 = empty
eNames' (FCall _ x)           = unions (map eNames' x)
eNames' (Case _ ls)           = unions (map (eNames' . fst) ls) `union`
                                unions (map (eNames' . snd) ls)
eNames' (UnaryOp _ u)         = eNames' u
eNames' (UnaryOpB _ u)        = eNames' u
eNames' (UnaryOpVV _ u)       = eNames' u
eNames' (UnaryOpVN _ u)       = eNames' u
eNames' (ArithBinaryOp _ a b) = eNames' a `union` eNames' b
eNames' (BoolBinaryOp _ a b)  = eNames' a `union` eNames' b
eNames' (EqBinaryOp _ a b)    = eNames' a `union` eNames' b
eNames' (LABinaryOp _ a b)    = eNames' a `union` eNames' b
eNames' (OrdBinaryOp _ a b)   = eNames' a `union` eNames' b
eNames' (VVVBinaryOp _ a b)   = eNames' a `union` eNames' b
eNames' (VVNBinaryOp _ a b)   = eNames' a `union` eNames' b
eNames' (NVVBinaryOp _ a b)   = eNames' a `union` eNames' b
eNames' (Operator _ _ e)      = eNames' e
eNames' (Matrix a)            = unions (map (unions . map eNames') a)
eNames' (RealI c b)           = insert c (eNamesRI' b)

-- | Generic traversal of everything that could come from an interval to names without functions (similar to 'eNames'').
eNamesRI' :: RealInterval Expr Expr -> Set UID
eNamesRI' (Bounded il iu) = eNames' (snd il) `union` eNames' (snd iu)
eNamesRI' (UpTo iu)       = eNames' (snd iu)
eNamesRI' (UpFrom il)     = eNames' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above