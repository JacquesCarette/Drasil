{-# OPTIONS -Wall #-} 
{-# LANGUAGE RankNTypes #-}
module ExprTools where

import Data.List (nub)
import ASTInternal (Expr(..))
import Control.Lens
import Chunk (VarChunk(..), Quantity, name, symbol, descr)

--Get dependency from equation  
get_dep :: Expr -> [String]
get_dep (a :/ b) = nub (get_dep a ++ get_dep b)
get_dep (a :* b) = nub (get_dep a ++ get_dep b)
get_dep (a :+ b) = nub (get_dep a ++ get_dep b)
get_dep (a :^ b) = nub (get_dep a ++ get_dep b)
get_dep (a :- b) = nub (get_dep a ++ get_dep b)
get_dep (C c) = [c ^. name]
get_dep (Int _) = []
get_dep (Dbl _) = []
get_dep (V _) = []

get_VCs :: Expr -> [VarChunk]
get_VCs (a :/ b) = nub (get_VCs a ++ get_VCs b)
get_VCs (a :* b) = nub (get_VCs a ++ get_VCs b)
get_VCs (a :+ b) = nub (get_VCs a ++ get_VCs b)
get_VCs (a :^ b) = nub (get_VCs a ++ get_VCs b)
get_VCs (a :- b) = nub (get_VCs a ++ get_VCs b)
get_VCs (C c) = [toVC c]
get_VCs (Int _) = []
get_VCs (Dbl _) = []
get_VCs (V _) = []

toVC :: Quantity c => c -> VarChunk
toVC c = VC (c ^. name) (c ^. descr) (c ^. symbol)

-- --Get dependency from equation  
-- get_dep_chunks :: Quantity c => Expr -> [c]
-- get_dep_chunks (a :/ b) = (get_dep a ++ get_dep b)
-- get_dep_chunks (a :* b) = (get_dep a ++ get_dep b)
-- get_dep_chunks (a :+ b) = (get_dep a ++ get_dep b)
-- get_dep_chunks (a :^ b) = (get_dep a ++ get_dep b)
-- get_dep_chunks (a :- b) = (get_dep a ++ get_dep b)
-- get_dep_chunks (C c) = (c:[])
-- get_dep_chunks (Int _) = []
-- get_dep_chunks (Dbl _) = []
-- get_dep_chunks (V _) = []