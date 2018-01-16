{-# LANGUAGE RankNTypes #-}
module Language.Drasil.Expr.Extract(dep, vars, codevars, codevars') where

import Data.List (nub)
import Control.Lens hiding ((:<),(:>))
import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), UFunc(..), BiFunc(..), EOperator(..))
import Language.Drasil.Chunk (id)
import Language.Drasil.ChunkDB
import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.Quantity (QWrapper)

--FIXME: Missing Patterns
-- | Get dependencies from an equation  
dep :: Expr -> [String]
dep (Assoc _ l)   = nub (concat $ map dep l)
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
dep (V _)         = []
dep (FCall f x)   = nub (dep f ++ (concat $ map dep x))
dep (Case ls)     = nub (concat $ map (dep . fst) ls ++ map (dep . snd) ls)
dep (EEquals a b)      = nub (dep a ++ dep b)
dep (ENEquals a b)     = nub (dep a ++ dep b)
dep (ELess a b)        = nub (dep a ++ dep b)
dep (EGreater a b)     = nub (dep a ++ dep b)
dep (EGreaterEq a b)   = nub (dep a ++ dep b)
dep (ELessEq a b)      = nub (dep a ++ dep b)
dep (UnaryOp u)   = dep (unpack u)
dep (Grouping e)  = dep e
dep (BinaryOp b)  = nub (concat $ map dep (binop b))
dep (EOp o)       = dep (unpackop o)
dep (a :=>  b)    = nub (dep a ++ dep b)
dep (a :<=> b)    = nub (dep a ++ dep b)
dep (IsIn  a _)   = nub (dep a)
dep (ForAll _ b)  = nub (dep b)
dep (Exists _ b)  = nub (dep b)
dep (Matrix a)    = nub (concat $ map (concat . map dep) a)
dep (Index a i)   = nub (dep a ++ dep i)
dep (Len a)       = nub (dep a)
dep (Append a b)  = nub (dep a ++ dep b) 

-- | Get a list of quantities (QWrapper) from an equation in order to print
vars :: (HasSymbolTable s) => Expr -> s -> [QWrapper]
vars (Assoc _ l)  m = nub $ concat $ map (\x -> vars x m) l
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
vars (C c)        m = [symbLookup c $ m ^. symbolTable]
vars (Int _)      _ = []
vars (Dbl _)      _ = []
vars (V _)        _ = []
vars (FCall f x)  m = nub (vars f m ++ (concat $ map (\y -> vars y m) x))
vars (Case ls)    m = nub (concat $ map (\x -> vars (fst x) m) ls ++ map (\x -> vars (snd x) m) ls)
vars (EEquals a b)     m = nub (vars a m ++ vars b m)
vars (ENEquals a b)    m = nub (vars a m ++ vars b m)
vars (EGreater a b)     m = nub (vars a m ++ vars b m)
vars (ELess a b)     m = nub (vars a m ++ vars b m)
vars (ELessEq a b)    m = nub (vars a m ++ vars b m)
vars (EGreaterEq a b)    m = nub (vars a m ++ vars b m)
vars (UnaryOp u)  m = vars (unpack u) m
vars (Grouping e) m = vars e m
vars (BinaryOp b) m = nub (concat $ map (\x -> vars x m) (binop b))
vars (EOp o) m = vars (unpackop o) m
vars (a :=>  b)   m = nub (vars a m ++ vars b m)
vars (a :<=> b)   m = nub (vars a m ++ vars b m)
vars (IsIn  a _)  m = nub (vars a m)
vars (ForAll _ b)  m = nub $ vars b m
vars (Exists _ b)  m = nub $ vars b m
vars (Matrix a)   m = nub (concat $ map (\x -> concat $ map (\y -> vars y m) x) a)
vars (Index a i)  m = nub (vars a m ++ vars i m)
vars (Len a)      m = nub (vars a m)
vars (Append a b) m = nub (vars a m ++ vars b m) 

-- | Get a list of CodeChunks from an equation
codevars :: (HasSymbolTable s) => Expr -> s -> [CodeChunk]
codevars (Assoc _ l)  sm = nub (concat $ map (\x -> codevars x sm) l)
codevars (a :/ b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (a :* b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (a :+ b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (a :^ b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (a :- b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (a :. b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (a :&& b)    sm = nub (codevars a sm ++ codevars b sm)
codevars (a :|| b)    sm = nub (codevars a sm ++ codevars b sm)
codevars (Deriv _ a b) sm = nub (codevars a sm ++ codevars b sm)
codevars (Not e)      sm = codevars e sm
codevars (Neg e)      sm = codevars e sm
codevars (C c)        sm = [codevar $ symbLookup c (sm ^. symbolTable)]
codevars (Int _)      _ = []
codevars (Dbl _)      _ = []
codevars (V _)        _ = []
codevars (FCall (C c) x)  sm = nub ((codefunc $ symbLookup c (sm ^. symbolTable)) : (concat $ map (\y -> codevars y sm) x))
codevars (FCall f x)  sm = nub (codevars f sm ++ (concat $ map (\y -> codevars y sm) x))
codevars (Case ls)    sm = nub (concat $ map (\x -> codevars (fst x) sm) ls ++ map (\x -> codevars (snd x) sm) ls)
codevars (EEquals a b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (ENEquals a b)    sm = nub (codevars a sm ++ codevars b sm)
codevars (EGreater a b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (ELess a b)     sm = nub (codevars a sm ++ codevars b sm)
codevars (ELessEq a b)    sm = nub (codevars a sm ++ codevars b sm)
codevars (EGreaterEq a b)    sm = nub (codevars a sm ++ codevars b sm)
codevars (UnaryOp u)  sm = codevars (unpack u) sm
codevars (Grouping e) sm = codevars e sm
codevars (BinaryOp b) sm = nub (concat $ map (\x -> codevars x sm) (binop b))
codevars (EOp o)  sm = codevars (unpackop o) sm
codevars (a :=>  b)   sm = nub (codevars a sm ++ codevars b sm)
codevars (a :<=> b)   sm = nub (codevars a sm ++ codevars b sm)
codevars (IsIn  a _)  sm = nub (codevars a sm)
codevars (ForAll _ b) sm = nub $ codevars b sm
codevars (Exists _ b) sm = nub $ codevars b sm
codevars (Matrix a)   sm = nub (concat $ map (concat . map (\x -> codevars x sm)) a)
codevars (Index a i)  sm = nub (codevars a sm ++ codevars i sm)
codevars (Len a)      sm = nub (codevars a sm)
codevars (Append a b) sm = nub (codevars a sm ++ codevars b sm) 

-- | Get a list of CodeChunks from an equation (no functions)
codevars' :: (HasSymbolTable s) => Expr -> s -> [CodeChunk]
codevars' (Assoc _ l)  sm = nub (concat $ map (\x -> codevars' x sm) l)
codevars' (a :/ b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (a :* b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (a :+ b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (a :^ b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (a :- b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (a :. b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (a :&& b)    sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (a :|| b)    sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (Deriv _ a b) sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (Not e)      sm = codevars' e sm 
codevars' (Neg e)      sm = codevars' e sm 
codevars' (C c)        sm = [codevar $ symbLookup c (sm ^. symbolTable)]
codevars' (Int _)       _ = []
codevars' (Dbl _)       _ = []
codevars' (V _)         _ = []
codevars' (FCall _ x)  sm = nub (concat $ map (\y -> codevars' y sm) x)
codevars' (Case ls)    sm = nub (concat $ map (\x -> codevars' (fst x) sm) ls ++
                              map (\y -> codevars' (snd y) sm) ls)
codevars' (EEquals a b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (ENEquals a b)    sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (EGreater a b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (ELess a b)     sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (ELessEq a b)    sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (EGreaterEq a b)    sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (UnaryOp u)  sm = codevars' (unpack u) sm
codevars' (Grouping e) sm = codevars' e sm
codevars' (BinaryOp b) sm = nub (concat $ map (\x -> codevars' x sm) (binop b))
codevars' (EOp o)      sm = codevars' (unpackop o) sm
codevars' (a :=>  b)   sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (a :<=> b)   sm = nub (codevars' a sm ++ codevars' b sm)
codevars' (IsIn  a _)  sm = nub (codevars' a sm)
codevars' (ForAll _ b)  sm = nub $ codevars' b sm
codevars' (Exists _ b)  sm = nub $ codevars' b sm
codevars' (Matrix a)   sm = nub (concat $ map (concat . map (\x -> codevars' x sm)) a)
codevars' (Index a i)  sm = nub (codevars' a sm ++ codevars' i sm)
codevars' (Len a)      sm = nub (codevars' a sm)
codevars' (Append a b) sm = nub (codevars' a sm ++ codevars' b sm) 


-- | Helper function for vars and dep, gets the Expr portion of a UFunc
unpack :: UFunc -> Expr
unpack (Log e) = e
unpack (Abs e) = e
unpack (Norm e) = e
unpack (Sin e) = e
unpack (Cos e) = e
unpack (Tan e) = e
unpack (Sec e) = e
unpack (Csc e) = e
unpack (Cot e) = e
unpack (Exp e) = e
unpack (Sqrt e) = e

unpackop :: EOperator -> Expr
unpackop (Summation _ e) = e
unpackop (Product _ e) = e
unpackop (Integral _ e) = e

-- | Helper function for vars and dep, gets Exprs from binary operations.
binop :: BiFunc -> [Expr]
binop (Cross e f) = [e,f]

-- Steven edit:  need this to have a type for code generation
--   setting to all to rational
-- | Convert any chunk to a VarChunk as long as it is an instance of SymbolForm.
-- Again, used for printing equations/descriptions mostly.
--toVC :: (Chunk c, HasSymbolTable s) => c -> s -> VarChunk
--toVC c m = vc' (lookupC) (symbol lookupC) (Rational)
  --where lookupC = symbLookup c (m ^. symbolTable)
