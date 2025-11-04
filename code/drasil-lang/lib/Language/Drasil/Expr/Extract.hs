-- | Extract UIDs from an expression so that they can be looked up in the chunk database and rendered.
module Language.Drasil.Expr.Extract where

import Data.Containers.ListUtils (nubOrd)

import Language.Drasil.Expr.Lang (Expr(..))
import Language.Drasil.Space (RealInterval(..))
import Drasil.Database.UID (UID)

-- | Generic traverse of all expressions that could lead to names.
eNames :: Expr -> [UID]
eNames (AssocA _ l)          = concatMap eNames l
eNames (AssocB _ l)          = concatMap eNames l
eNames (AssocC _ l)          = concatMap eNames l
eNames (C c)                 = [c]
eNames Lit{}                 = []
eNames (FCall f x)           = f : concatMap eNames x
eNames (Case _ ls)           = concatMap (eNames . fst) ls ++
                               concatMap (eNames . snd) ls
eNames (UnaryOp _ u)         = eNames u
eNames (UnaryOpB _ u)        = eNames u
eNames (UnaryOpVV _ u)       = eNames u
eNames (UnaryOpVN _ u)       = eNames u
eNames (ArithBinaryOp _ a b) = eNames a ++ eNames b
eNames (BoolBinaryOp _ a b)  = eNames a ++ eNames b
eNames (EqBinaryOp _ a b)    = eNames a ++ eNames b
eNames (LABinaryOp _ a b)    = eNames a ++ eNames b
eNames (OrdBinaryOp _ a b)   = eNames a ++ eNames b
eNames (VVVBinaryOp _ a b)   = eNames a ++ eNames b
eNames (VVNBinaryOp _ a b)   = eNames a ++ eNames b
eNames (NVVBinaryOp _ a b)   = eNames a ++ eNames b
eNames (ESSBinaryOp _ _ s)   = eNames s
eNames (ESBBinaryOp _ _ s)   = eNames s
eNames (Operator _ _ e)      = eNames e
eNames (Matrix a)            = concatMap (concatMap eNames) a
eNames (Set _ a)             = concatMap eNames a
eNames (Variable _ e)        = eNames e
eNames (RealI c b)           = c : eNamesRI b

-- | Generic traversal of everything that could come from an interval to names (similar to 'eNames').
eNamesRI :: RealInterval Expr Expr -> [UID]
eNamesRI (Bounded (_, il) (_, iu)) = eNames il ++ eNames iu
eNamesRI (UpTo (_, iu))            = eNames iu
eNamesRI (UpFrom (_, il))          = eNames il

-- | Generic traverse of all positions that could lead to 'eNames' without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
eNames' :: Expr -> [UID]
eNames' (AssocA _ l)          = concatMap eNames' l
eNames' (AssocB _ l)          = concatMap eNames' l
eNames' (AssocC _ l)          = concatMap eNames' l
eNames' (C c)                 = [c]
eNames' Lit{}                 = []
eNames' (FCall _ x)           = concatMap eNames' x
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
eNames' (ESSBinaryOp _ _ s)   = eNames' s
eNames' (ESBBinaryOp _ _ s)   = eNames' s
eNames' (Operator _ _ e)      = eNames' e
eNames' (Matrix a)            = concatMap (concatMap eNames') a
eNames' (Set _ a)             = concatMap eNames' a
eNames' (Variable _ e)        = eNames' e
eNames' (RealI c b)           = c : eNamesRI' b

-- | Generic traversal of everything that could come from an interval to names without functions (similar to 'eNames'').
eNamesRI' :: RealInterval Expr Expr -> [UID]
eNamesRI' (Bounded il iu) = eNames' (snd il) ++ eNames' (snd iu)
eNamesRI' (UpTo iu)       = eNames' (snd iu)
eNamesRI' (UpFrom il)     = eNames' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation.
eDep :: Expr -> [UID]
eDep = nubOrd . eNames
