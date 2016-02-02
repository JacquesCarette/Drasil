{-# OPTIONS -Wall #-} 
{-# LANGUAGE RankNTypes #-}
module ExprTools where

import Data.List (nub)
import ASTInternal (Expr(..))
import Control.Lens
import Chunk (Quantity, name, symbol)
import Symbol (Symbol)

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