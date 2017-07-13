{-# LANGUAGE RankNTypes #-}
module Language.Drasil.Expr.Extract(dep, vars, codevars, toVC) where

import Data.List (nub)
import Control.Lens hiding ((:<),(:>))
import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), UFunc(..), BiFunc(..), Quantifier(..))
import Language.Drasil.Chunk (id)
import Language.Drasil.Chunk.VarChunk (VarChunk(..), vc', makeVC)
import Language.Drasil.Chunk.SymbolForm (SymbolForm, symbol)
import Language.Drasil.Space  -- need this for code generation
import Language.Drasil.ChunkDB

import Language.Drasil.NounPhrase -- temporary until Expr can constrain Quantity without circular import
import Language.Drasil.Chunk.Code

-- | Get dependencies from an equation  
dep :: Expr -> [String]
dep (a :/ b)      = nub (dep a ++ dep b)
dep (a :* b)      = nub (dep a ++ dep b)
dep (a :+ b)      = nub (dep a ++ dep b)
dep (a :^ b)      = nub (dep a ++ dep b)
dep (a :- b)      = nub (dep a ++ dep b)
dep (a :. b)      = nub (dep a ++ dep b)
dep (a :&& b)     = nub (dep a ++ dep b)
dep (a :|| b)     = nub (dep a ++ dep b)
dep (Deriv _ a b) = nub (dep a ++ dep b)
dep (Not e)       = dep e
dep (Neg e)       = dep e
dep (C c)         = [c ^. id]
dep (Int _)       = []
dep (Dbl _)       = []
dep (Bln _)       = []
dep (V _)         = []
dep (FCall f x)   = nub (dep f ++ (concat $ map dep x))
dep (Case ls)     = nub (concat (map (dep . fst) ls))
dep (a := b)      = nub (dep a ++ dep b)
dep (a :!= b)     = nub (dep a ++ dep b)
dep (a :< b)      = nub (dep a ++ dep b)
dep (a :> b)      = nub (dep a ++ dep b)
dep (a :>= b)     = nub (dep a ++ dep b)
dep (a :<= b)     = nub (dep a ++ dep b)
dep (UnaryOp u)   = dep (unpack u)
dep (Grouping e)  = dep e
dep (BinaryOp b)  = nub (concat $ map dep (binop b))
dep (a :=>  b)    = nub (dep a ++ dep b)
dep (a :<=> b)    = nub (dep a ++ dep b)
dep (IsIn  a _)   = nub (concat $ map dep a)
dep (NotIn a _)   = nub (concat $ map dep a)
dep (State a b)   = nub ((concat $ map (dep . quant) a) ++ dep b)
dep (Matrix a)    = nub (concat $ map (concat . map dep) a)

-- | Get a list of VarChunks from an equation in order to print
vars :: Expr -> SymbolMap -> [VarChunk]
vars (a :/ b)     m = nub (vars a m ++ vars b m)
vars (a :* b)     m = nub (vars a m ++ vars b m)
vars (a :+ b)     m = nub (vars a m ++ vars b m)
vars (a :^ b)     m = nub (vars a m ++ vars b m)
vars (a :- b)     m = nub (vars a m ++ vars b m)
vars (a :. b)     m = nub (vars a m ++ vars b m)
vars (a :&& b)    m = nub (vars a m ++ vars b m)
vars (a :|| b)    m = nub (vars a m ++ vars b m)
vars (Deriv _ a b) m = nub (vars a m ++ vars b m)
vars (Not e)      m = vars e m
vars (Neg e)      m = vars e m
vars (C c)        m = [toVC c m]
vars (Int _)      _ = []
vars (Dbl _)      _ = []
vars (Bln _)      _ = []
vars (V _)        _ = []
vars (FCall f x)  m = nub (vars f m ++ (concat $ map (\y -> vars y m) x))
vars (Case ls)    m = nub (concat (map (\x -> vars (fst x) m) ls))
vars (a := b)     m = nub (vars a m ++ vars b m)
vars (a :!= b)    m = nub (vars a m ++ vars b m)
vars (a :> b)     m = nub (vars a m ++ vars b m)
vars (a :< b)     m = nub (vars a m ++ vars b m)
vars (a :<= b)    m = nub (vars a m ++ vars b m)
vars (a :>= b)    m = nub (vars a m ++ vars b m)
vars (UnaryOp u)  m = vars (unpack u) m
vars (Grouping e) m = vars e m
vars (BinaryOp b) m = nub (concat $ map (\x -> vars x m) (binop b))
vars (a :=>  b)   m = nub (vars a m ++ vars b m)
vars (a :<=> b)   m = nub (vars a m ++ vars b m)
vars (IsIn  a _)  m = nub (concat $ map (\x -> vars x m) a)
vars (NotIn a _)  m = nub (concat $ map (\x -> vars x m) a)
vars (State a b)  m = nub ((concat $ map (\x -> vars (quant x) m) a) ++ vars b m)
vars (Matrix a)   m = nub (concat $ map (\x -> concat $ map (\y -> vars y m) x) a)

-- | Get a list of CodeChunks from an equation
codevars :: Expr -> [CodeChunk]
codevars (a :/ b)     = nub (codevars a ++ codevars b)
codevars (a :* b)     = nub (codevars a ++ codevars b)
codevars (a :+ b)     = nub (codevars a ++ codevars b)
codevars (a :^ b)     = nub (codevars a ++ codevars b)
codevars (a :- b)     = nub (codevars a ++ codevars b)
codevars (a :. b)     = nub (codevars a ++ codevars b)
codevars (a :&& b)    = nub (codevars a ++ codevars b)
codevars (a :|| b)    = nub (codevars a ++ codevars b)
codevars (Deriv _ a b) = nub (codevars a ++ codevars b)
codevars (Not e)      = codevars e
codevars (Neg e)      = codevars e
codevars (C c)        = [codevar $ makeVC (c ^. id) (pn "") (c ^. symbol)]
codevars (Int _)      = []
codevars (Dbl _)      = []
codevars (Bln _)      = []
codevars (V _)        = []
codevars (FCall f x)  = nub (codevars f ++ (concat $ map (\y -> codevars y) x))
codevars (Case ls)    = nub (concat (map (\x -> codevars (fst x)) ls))
codevars (a := b)     = nub (codevars a ++ codevars b)
codevars (a :!= b)    = nub (codevars a ++ codevars b)
codevars (a :> b)     = nub (codevars a ++ codevars b)
codevars (a :< b)     = nub (codevars a ++ codevars b)
codevars (a :<= b)    = nub (codevars a ++ codevars b)
codevars (a :>= b)    = nub (codevars a ++ codevars b)
codevars (UnaryOp u)  = codevars (unpack u)
codevars (Grouping e) = codevars e
codevars (BinaryOp b) = nub (concat $ map (\x -> codevars x) (binop b))
codevars (a :=>  b)   = nub (codevars a ++ codevars b)
codevars (a :<=> b)   = nub (codevars a ++ codevars b)
codevars (IsIn  a _)  = nub (concat $ map codevars a)
codevars (NotIn a _)  = nub (concat $ map codevars a)
codevars (State a b)  = nub ((concat $ map (codevars . quant) a) ++ codevars b)
codevars (Matrix a)   = nub (concat $ map (concat . map codevars) a)


-- | Helper function for vars and dep, gets the Expr portion of a UFunc
unpack :: UFunc -> Expr
unpack (Log e) = e
unpack (Summation _ e) = e
unpack (Abs e) = e
unpack (Norm e) = e
unpack (Integral _ e _) = e
unpack (Sin e) = e
unpack (Cos e) = e
unpack (Tan e) = e
unpack (Sec e) = e
unpack (Csc e) = e
unpack (Cot e) = e
unpack (Product _ e) = e
unpack (Exp e) = e
unpack (Sqrt e) = e

-- | Helper function for vars and dep, gets Exprs from binary operations.
binop :: BiFunc -> [Expr]
binop (Cross e f) = [e,f]

-- | Helper function for vars and dep, gets Exprs from Quantifier
quant :: Quantifier -> Expr
quant (Forall e) = e
quant (Exists e) = e

-- Steven edit:  need this to have a type for code generation
--   setting to all to rational
-- | Convert any chunk to a VarChunk as long as it is an instance of SymbolForm.
-- Again, used for printing equations/descriptions mostly.
toVC :: (SymbolForm c) => c -> SymbolMap -> VarChunk
toVC c m = vc' (lookupC) (lookupC ^. symbol) (Rational)
  where lookupC = symbLookup c m
