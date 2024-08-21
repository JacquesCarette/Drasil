-- | Defines functions to find Chunk UIDs within 'ModelExpr's.
module Language.Drasil.ModelExpr.Extract where

import Data.Containers.ListUtils (nubOrd)

import Language.Drasil.ModelExpr.Lang (ModelExpr(..))
import Language.Drasil.Space (RealInterval(..))
import Language.Drasil.UID (UID)

-- | Generic traverse of all expressions that could lead to names.
meNames :: ModelExpr -> [UID]
meNames (AssocA _ l)          = concatMap meNames l
meNames (AssocB _ l)          = concatMap meNames l
meNames (Deriv _ _ a b)       = b : meNames a
meNames (C c)                 = [c]
meNames Lit{}                 = []
meNames Spc{}                 = []
meNames (FCall f x)           = f : concatMap meNames x
meNames (Case _ ls)           = concatMap (meNames . fst) ls ++
                                concatMap (meNames . snd) ls
meNames (UnaryOp _ u)         = meNames u
meNames (UnaryOpB _ u)        = meNames u
meNames (UnaryOpVV _ u)       = meNames u
meNames (UnaryOpVN _ u)       = meNames u
meNames (ArithBinaryOp _ a b) = meNames a ++ meNames b
meNames (BoolBinaryOp _ a b)  = meNames a ++ meNames b
meNames (EqBinaryOp _ a b)    = meNames a ++ meNames b
meNames (LABinaryOp _ a b)    = meNames a ++ meNames b
meNames (SpaceBinaryOp _ a b) = meNames a ++ meNames b
meNames (StatBinaryOp _ a b)  = meNames a ++ meNames b
meNames (OrdBinaryOp _ a b)   = meNames a ++ meNames b
meNames (VVVBinaryOp _ a b)   = meNames a ++ meNames b
meNames (VVNBinaryOp _ a b)   = meNames a ++ meNames b
meNames (NVVBinaryOp _ a b)   = meNames a ++ meNames b
meNames (Operator _ _ e)      = meNames e
meNames (Matrix a)            = concatMap (concatMap meNames) a
meNames (RealI c b)           = c : meNamesRI b
meNames (ForAll _ _ de)       = meNames de

-- | Generic traversal of everything that could come from an interval to names (similar to 'meNames').
meNamesRI :: RealInterval ModelExpr ModelExpr -> [UID]
meNamesRI (Bounded (_, il) (_, iu)) = meNames il ++ meNames iu
meNamesRI (UpTo (_, iu))            = meNames iu
meNamesRI (UpFrom (_, il))          = meNames il

-- | Generic traverse of all positions that could lead to 'meNames' without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
meNames' :: ModelExpr -> [UID]
meNames' (AssocA _ l)          = concatMap meNames' l
meNames' (AssocB _ l)          = concatMap meNames' l
meNames' (Deriv _ _ a b)       = b : meNames' a
meNames' (C c)                 = [c]
meNames' Lit{}                 = []
meNames' Spc{}                 = []
meNames' (FCall _ x)           = concatMap meNames' x
meNames' (Case _ ls)           = concatMap (meNames' . fst) ls ++ 
                                 concatMap (meNames' . snd) ls
meNames' (UnaryOp _ u)         = meNames' u
meNames' (UnaryOpB _ u)        = meNames' u
meNames' (UnaryOpVV _ u)       = meNames' u
meNames' (UnaryOpVN _ u)       = meNames' u
meNames' (ArithBinaryOp _ a b) = meNames' a ++ meNames' b
meNames' (BoolBinaryOp _ a b)  = meNames' a ++ meNames' b
meNames' (EqBinaryOp _ a b)    = meNames' a ++ meNames' b
meNames' (LABinaryOp _ a b)    = meNames' a ++ meNames' b
meNames' (OrdBinaryOp _ a b)   = meNames' a ++ meNames' b
meNames' (SpaceBinaryOp _ a b) = meNames' a ++ meNames' b
meNames' (StatBinaryOp _ a b)  = meNames' a ++ meNames' b
meNames' (VVVBinaryOp _ a b)   = meNames' a ++ meNames' b
meNames' (VVNBinaryOp _ a b)   = meNames' a ++ meNames' b
meNames' (NVVBinaryOp _ a b)   = meNames' a ++ meNames' b
meNames' (Operator _ _ e)      = meNames' e
meNames' (Matrix a)            = concatMap (concatMap meNames') a
meNames' (RealI c b)           = c : meNamesRI' b
meNames' (ForAll _ _ de)       = meNames' de

-- | Generic traversal of everything that could come from an interval to names without functions (similar to 'meNames'').
meNamesRI' :: RealInterval ModelExpr ModelExpr -> [UID]
meNamesRI' (Bounded il iu) = meNames' (snd il) ++ meNames' (snd iu)
meNamesRI' (UpTo iu)       = meNames' (snd iu)
meNamesRI' (UpFrom il)     = meNames' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation.  
meDep :: ModelExpr -> [UID]
meDep = nubOrd . meNames
