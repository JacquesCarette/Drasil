{-# LANGUAGE RankNTypes #-}
module Language.Drasil.Expr.Extract(dep, vars, toVC) where

import Data.List (nub)
import Control.Lens hiding ((:<),(:>))
import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), UFunc(..), BiFunc(..))
import Language.Drasil.Chunk (id)
import Language.Drasil.Chunk.NamedIdea (NamedIdea)
import Language.Drasil.Chunk.VarChunk (VarChunk(..), vc')
import Language.Drasil.Chunk.SymbolForm (SymbolForm, symbol)
import Language.Drasil.Space  -- need this for code generation

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
dep (C c)         = [c ^. id]
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
dep (BinaryOp b)  = nub (concat $ map dep (binop b))

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
vars (UnaryOp u)   = vars (unpack u)
vars (Grouping e)  = vars e
vars (BinaryOp b)  = nub (concat $ map vars (binop b))

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

binop :: BiFunc -> [Expr]
binop (Cross e f) = [e,f]


-- Convert any chunk to a VarChunk as long as it is an instance of SymbolForm.
-- Again, used for printing equations/descriptions mostly.
-- Steven edit:  need this to have a type for code generation
--   setting to all to rational
toVC :: (NamedIdea c, SymbolForm c) => c -> VarChunk
toVC c = vc' c (c ^. symbol) (Rational)
