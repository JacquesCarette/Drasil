module Drasil.Code.CodeExpr.Extract (
    eDep, eDep',
    eNamesRI, eNamesRI'
) where

import Data.Containers.ListUtils (nubOrd)

import Drasil.Database (UID)

import Language.Drasil.Space (RealInterval(..))
import Drasil.Code.CodeExpr.Lang (CodeExpr(..))

-- | Generic traverse of all expressions that could lead to names.
eNames :: CodeExpr -> [UID]
eNames (AssocA _ l)          = concatMap eNames l
eNames (AssocB _ l)          = concatMap eNames l
eNames (AssocC _ l)          = concatMap eNames l
eNames (C c)                 = [c]
eNames Lit{}                 = []
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
eNames (UnaryOpCC _ u)       = eNames u
eNames (UnaryOpCN _ u)       = eNames u
eNames (ArithBinaryOp _ a b) = eNames a ++ eNames b
eNames (BoolBinaryOp _ a b)  = eNames a ++ eNames b
eNames (EqBinaryOp _ a b)    = eNames a ++ eNames b
eNames (LABinaryOp _ a b)    = eNames a ++ eNames b
eNames (OrdBinaryOp _ a b)   = eNames a ++ eNames b
eNames (CCCBinaryOp _ a b)   = eNames a ++ eNames b
eNames (CCNBinaryOp _ a b)   = eNames a ++ eNames b
eNames (NCCBinaryOp _ a b)   = eNames a ++ eNames b
eNames (ESSBinaryOp _ _ s)   = eNames s
eNames (ESBBinaryOp _ _ s)   = eNames s
eNames (Operator _ _ e)      = eNames e
eNames (NatCCBinaryOp _ _ c) = eNames c  -- Only extract from the CodeExpr part
eNames (Clif _ _)           = []        -- For now, return empty list for Clif basis blades
eNames (Matrix a)            = concatMap (concatMap eNames) a
eNames (Set _ a)             = concatMap eNames a
eNames (Variable _ e)        = eNames e
eNames (RealI c b)           = c : eNamesRI b

-- | Generic traversal of everything that could come from an interval to names (similar to 'eNames').
eNamesRI :: RealInterval CodeExpr CodeExpr -> [UID]
eNamesRI (Bounded (_, il) (_, iu)) = eNames il ++ eNames iu
eNamesRI (UpTo (_, iu))           = eNames iu
eNamesRI (UpFrom (_, il))         = eNames il

-- | Generic traverse of all positions that could lead to 'eNames' without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
eNames' :: CodeExpr -> [UID]
eNames' (AssocA _ l)          = concatMap eNames' l
eNames' (AssocB _ l)          = concatMap eNames' l
eNames' (AssocC _ l)          = concatMap eNames' l
eNames' (C c)                 = [c]
eNames' Lit{}                 = []
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
eNames' (UnaryOpCC _ u)       = eNames' u
eNames' (UnaryOpCN _ u)       = eNames' u
eNames' (ArithBinaryOp _ a b) = eNames' a ++ eNames' b
eNames' (BoolBinaryOp _ a b)  = eNames' a ++ eNames' b
eNames' (EqBinaryOp _ a b)    = eNames' a ++ eNames' b
eNames' (LABinaryOp _ a b)    = eNames' a ++ eNames' b
eNames' (OrdBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (CCCBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (CCNBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (NCCBinaryOp _ a b)   = eNames' a ++ eNames' b
eNames' (ESSBinaryOp _ _ s)   = eNames' s
eNames' (ESBBinaryOp _ _ s)   = eNames' s
eNames' (Operator _ _ e)      = eNames' e
eNames' (NatCCBinaryOp _ _ c) = eNames' c  -- Only extract from the CodeExpr part
eNames' (Clif _ _)           = []         -- For now, return empty list for Clif basis blades
eNames' (Matrix a)            = concatMap (concatMap eNames') a
eNames' (Set _ a)             = concatMap eNames' a
eNames' (Variable _ e)        = eNames' e
eNames' (RealI c b)           = c : eNamesRI' b

-- | Generic traversal of everything that could come from an interval to names without functions (similar to 'eNames'').
eNamesRI' :: RealInterval CodeExpr CodeExpr -> [UID]
eNamesRI' (Bounded il iu) = eNames' (snd il) ++ eNames' (snd iu)
eNamesRI' (UpTo iu)       = eNames' (snd iu)
eNamesRI' (UpFrom il)     = eNames' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation.
eDep :: CodeExpr -> [UID]
eDep = nubOrd . eNames

-- | Get dependencies from an equation, without functions.
eDep' :: CodeExpr -> [UID]
eDep' = nubOrd . eNames'