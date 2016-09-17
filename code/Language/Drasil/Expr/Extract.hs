{-# LANGUAGE RankNTypes #-}
module Language.Drasil.Expr.Extract(dep, vars, toVC) where

import Data.List (nub)
import Control.Lens hiding ((:<),(:>))

import Language.Drasil.Expr (Expr(..), UFunc(..))
import Language.Drasil.Chunk (VarChunk(..), Quantity, name, symbol, descr)

--Get dependency from equation  
dep :: Expr -> [String]
dep (a :/ b)      = nub (dep a ++ dep b)
dep (a :* b)      = nub (dep a ++ dep b)
dep (a :+ b)      = nub (dep a ++ dep b)
dep (a :^ b)      = nub (dep a ++ dep b)
dep (a :- b)      = nub (dep a ++ dep b)
dep (a :. b)      = nub (dep a ++ dep b)
dep (Deriv _ a b) = nub (dep a ++ dep b)
dep (Neg e)       = dep e
dep (C c)         = [c ^. name]
dep (Int _)       = []
dep (Dbl _)       = []
dep (V _)         = []
dep (FCall f x)   = nub (dep f ++ (concat $ map dep x))
dep (Case ls)     = nub (concat (map (dep . fst) ls))
dep (a := b)      = nub (dep a ++ dep b)
dep (a :< b)      = nub (dep a ++ dep b)
dep (a :> b)      = nub (dep a ++ dep b)
dep (UnaryOp u)   = dep (unpack u)
dep (Grouping e)  = dep e

--Get a list of VarChunks from an equation in order to print
vars :: Expr -> [VarChunk]
vars (a :/ b)      = nub (vars a ++ vars b)
vars (a :* b)      = nub (vars a ++ vars b)
vars (a :+ b)      = nub (vars a ++ vars b)
vars (a :^ b)      = nub (vars a ++ vars b)
vars (a :- b)      = nub (vars a ++ vars b)
vars (a :. b)      = nub (vars a ++ vars b)
vars (Deriv _ a b) = nub (vars a ++ vars b)
vars (Neg e)       = vars e
vars (C c)         = [toVC c]
vars (Int _)       = []
vars (Dbl _)       = []
vars (V _)         = []
vars (FCall f x)   = nub (vars f ++ (concat $ map vars x))
vars (Case ls)     = nub (concat (map (vars . fst) ls))
vars (a := b)      = nub (vars a ++ vars b)
vars (a :> b)      = nub (vars a ++ vars b)
vars (a :< b)      = nub (vars a ++ vars b)
vars (UnaryOp u) = vars (unpack u)
vars (Grouping e)  = vars e

unpack :: UFunc -> Expr
unpack (Log e) = e
unpack (Summation _ e) = e
unpack (Abs e) = e
unpack (Integral _ e _) = e
unpack (Sin e) = e
unpack (Cos e) = e
unpack (Tan e) = e
unpack (Sec e) = e
unpack (Csc e) = e
unpack (Cot e) = e
-- Convert any chunk to a VarChunk as long as it is an instance of Quantity.
-- Again, used for printing equations/descriptions mostly.
toVC :: Quantity c => c -> VarChunk
toVC c = VC (c ^. name) (c ^. descr) (c ^. symbol)
