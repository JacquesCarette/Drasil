{-# LANGUAGE RankNTypes #-}
module Language.Drasil.Expr.Extract(dep, vars, toVC, SymbolMap, symbolMap) where

import Data.List (nub)
import Control.Lens hiding ((:<),(:>))
import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), UFunc(..), BiFunc(..))
import Language.Drasil.Chunk (Chunk, id)
import Language.Drasil.Chunk.Quantity (Quantity)
import Language.Drasil.Chunk.Wrapper.QSWrapper (QSWrapper, qs)
import Language.Drasil.Chunk.VarChunk (VarChunk(..), vc')
import Language.Drasil.Chunk.SymbolForm (SymbolForm, symbol)
import Language.Drasil.Space  -- need this for code generation

import qualified Data.Map as Map

type SymbolMap = Map.Map String QSWrapper

symbolMap :: (SymbolForm c, Quantity c) => [c] -> SymbolMap
symbolMap cs = Map.fromList (map (\x -> ((x ^. id), qs x)) cs)

-- | Get dependencies from an equation  
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

-- | Get a list of VarChunks from an equation in order to print
vars :: Expr -> SymbolMap -> [VarChunk]
vars (a :/ b)     m = nub (vars a m ++ vars b m)
vars (a :* b)     m = nub (vars a m ++ vars b m)
vars (a :+ b)     m = nub (vars a m ++ vars b m)
vars (a :^ b)     m = nub (vars a m ++ vars b m)
vars (a :- b)     m = nub (vars a m ++ vars b m)
vars (a :. b)     m = nub (vars a m ++ vars b m)
vars (Deriv _ a b) m = nub (vars a m ++ vars b m)
vars (Neg e)      m = vars e m
vars (C c)        m = [toVC c m]
vars (Int _)      _ = []
vars (Dbl _)      _ = []
vars (V _)        _ = []
vars (FCall f x)  m = nub (vars f m ++ (concat $ map (\y -> vars y m) x))
vars (Case ls)    m = nub (concat (map (\x -> vars (fst x) m) ls))
vars (a := b)     m = nub (vars a m ++ vars b m)
vars (a :> b)     m = nub (vars a m ++ vars b m)
vars (a :< b)     m = nub (vars a m ++ vars b m)
vars (UnaryOp u)  m = vars (unpack u) m
vars (Grouping e) m = vars e m
vars (BinaryOp b) m = nub (concat $ map (\x -> vars x m) (binop b))

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

-- | Helper function for vars and dep, gets Exprs from binary operations.
binop :: BiFunc -> [Expr]
binop (Cross e f) = [e,f]

-- Steven edit:  need this to have a type for code generation
--   setting to all to rational
-- | Convert any chunk to a VarChunk as long as it is an instance of SymbolForm.
-- Again, used for printing equations/descriptions mostly.
toVC :: (SymbolForm c) => c -> SymbolMap -> VarChunk
toVC c m = vc' (lookupC) (lookupC ^. symbol) (Rational)
  where lookupC = symbLookup c m

symbLookup :: (Chunk c) => c -> SymbolMap -> QSWrapper
symbLookup c m = let lookC = Map.lookup (c ^. id) m in
                 getS lookC
  where getS (Just x) = x
        getS Nothing = error $ "Symbol: " ++ (c ^. id) ++ " not found in SymbolMap"
