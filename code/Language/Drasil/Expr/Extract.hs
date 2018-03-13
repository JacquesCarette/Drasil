module Language.Drasil.Expr.Extract(dep, vars, codevars, codevars') where

import Data.List (nub)
import Control.Lens hiding ((:<),(:>))
import Language.Drasil.Expr (Expr(..), EOperator(..))
import Language.Drasil.ChunkDB
import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.Quantity (QuantityDict)

--FIXME: Missing Patterns
-- | Get dependencies from an equation  
dep :: Expr -> [String]
dep (Assoc _ l)   = nub (concat $ map dep l)
dep (Deriv _ a b) = nub (b : dep a)
dep (C c _)       = [c]
dep (Int _)       = []
dep (Dbl _)       = []
dep (Str _)       = []
dep (FCall f x)   = nub (dep f ++ (concat $ map dep x))
dep (Case ls)     = nub (concat $ map (dep . fst) ls ++ map (dep . snd) ls)
dep (UnaryOp _ u) = dep u
dep (BinaryOp _ a b)  = nub (dep a ++ dep b)
dep (EOp o)       = dep (unpackop o)
dep (IsIn  a _)   = nub (dep a)
dep (Matrix a)    = nub (concat $ map (concat . map dep) a)

-- | Get a list of quantities (QuantityDict) from an equation in order to print
vars :: (HasSymbolTable s) => Expr -> s -> [QuantityDict]
vars (Assoc _ l)    m = nub $ concat $ map (\x -> vars x m) l
vars (Deriv _ a b)  m = nub (vars a m ++ [symbLookup b $ m ^. symbolTable])
vars (C c _)        m = [symbLookup c $ m ^. symbolTable]
vars (Int _)        _ = []
vars (Dbl _)        _ = []
vars (Str _)        _ = []
vars (FCall f x)    m = nub (vars f m ++ (concat $ map (\y -> vars y m) x))
vars (Case ls)      m = nub (concat $ map (\x -> vars (fst x) m) ls ++ map (\x -> vars (snd x) m) ls)
vars (UnaryOp _ u)  m = vars u m
vars (BinaryOp _ a b)   m = nub $ vars a m ++ vars b m
vars (EOp o)        m = vars (unpackop o) m
vars (IsIn  a _)    m = nub (vars a m)
vars (Matrix a)     m = nub (concat $ map (\x -> concat $ map (\y -> vars y m) x) a)

-- | Get a list of CodeChunks from an equation
codevars :: (HasSymbolTable s) => Expr -> s -> [CodeChunk]
codevars (Assoc _ l)  sm = nub (concat $ map (\x -> codevars x sm) l)
codevars (Deriv _ a b) sm = nub (codevars a sm ++ [codevar $ symbLookup b (sm ^. symbolTable)])
codevars (C c _)      sm = [codevar $ symbLookup c (sm ^. symbolTable)]
codevars (Int _)      _ = []
codevars (Dbl _)      _ = []
codevars (Str _)      _ = []
codevars (FCall (C c _) x)  sm = nub ((codefunc $ symbLookup c (sm ^. symbolTable)) : (concat $ map (\y -> codevars y sm) x))
codevars (FCall f x)  sm = nub (codevars f sm ++ (concat $ map (\y -> codevars y sm) x))
codevars (Case ls)    sm = nub (concat $ map (\x -> codevars (fst x) sm) ls ++ map (\x -> codevars (snd x) sm) ls)
codevars (UnaryOp _ u)  sm = codevars u sm
codevars (BinaryOp _ a b) sm = nub $ codevars a sm ++ codevars b sm
codevars (EOp o)  sm = codevars (unpackop o) sm
codevars (IsIn  a _)  sm = nub (codevars a sm)
codevars (Matrix a)   sm = nub (concat $ map (concat . map (\x -> codevars x sm)) a)

-- | Get a list of CodeChunks from an equation (no functions)
codevars' :: (HasSymbolTable s) => Expr -> s -> [CodeChunk]
codevars' (Assoc _ l)  sm = nub (concat $ map (\x -> codevars' x sm) l)
codevars' (Deriv _ a b) sm = nub (codevars' a sm ++ [codevar $ symbLookup b (sm^.symbolTable)])
codevars' (C c _)       sm = [codevar $ symbLookup c (sm ^. symbolTable)]
codevars' (Int _)       _ = []
codevars' (Dbl _)       _ = []
codevars' (Str _)       _ = []
codevars' (FCall _ x)  sm = nub (concat $ map (\y -> codevars' y sm) x)
codevars' (Case ls)    sm = nub (concat $ map (\x -> codevars' (fst x) sm) ls ++
                              map (\y -> codevars' (snd y) sm) ls)
codevars' (UnaryOp _ u)  sm = codevars' u sm
codevars' (BinaryOp _ a b) sm = nub $ codevars' a sm ++ codevars' b sm
codevars' (EOp o)      sm = codevars' (unpackop o) sm
codevars' (IsIn  a _)  sm = nub (codevars' a sm)
codevars' (Matrix a)   sm = nub (concat $ map (concat . map (\x -> codevars' x sm)) a)

unpackop :: EOperator -> Expr
unpackop (Summation _ e) = e
unpackop (Product _ e) = e
unpackop (Integral _ e) = e
