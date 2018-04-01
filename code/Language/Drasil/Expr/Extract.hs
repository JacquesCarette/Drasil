module Language.Drasil.Expr.Extract(dep, vars, codevars, codevars') where

import Data.List (nub)
import Control.Lens ((^.))
import Language.Drasil.Expr (Expr(..), RealInterval(..),Inclusive(..))
import Language.Drasil.ChunkDB
import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.Quantity (QuantityDict)

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
names (Matrix a)    = concatMap (concat . map names) a
names (RealI c b)   = c : names_ri b

names_ri :: RealInterval -> [String]
names_ri (Bounded il iu) = names_inc il ++ names_inc iu
names_ri (UpTo iu)       = names_inc iu
names_ri (UpFrom il)     = names_inc il

names_inc :: Inclusive Expr -> [String]
names_inc (Inc e) = names e
names_inc (Exc e) = names e

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
names' (Matrix a)    = concatMap (concat . map names') a
names' (RealI c b)   = c : names'_ri b

names'_ri :: RealInterval -> [String]
names'_ri (Bounded il iu) = names'_inc il ++ names'_inc iu
names'_ri (UpTo iu)       = names'_inc iu
names'_ri (UpFrom il)     = names'_inc il

names'_inc :: Inclusive Expr -> [String]
names'_inc (Inc e) = names' e
names'_inc (Exc e) = names' e

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from an equation  
dep :: Expr -> [String]
dep = nub . names

-- | Get a list of quantities (QuantityDict) from an equation in order to print
vars :: (HasSymbolTable s) => Expr -> s -> [QuantityDict]
vars e m = map resolve $ dep e
  where resolve x = symbLookup x $ m ^. symbolTable

-- | Get a list of CodeChunks from an equation
codevars :: (HasSymbolTable s) => Expr -> s -> [CodeChunk]
codevars e m = map resolve $ dep e
  where resolve x = codevar $ symbLookup x $ m ^. symbolTable

-- | Get a list of CodeChunks from an equation (no functions)
codevars' :: (HasSymbolTable s) => Expr -> s -> [CodeChunk]
codevars' e m = map resolve $ nub $ names' e
  where resolve x = codevar $ symbLookup x $ m ^. symbolTable
