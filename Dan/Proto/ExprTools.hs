{-# OPTIONS -Wall #-} 
module ExprTools where
import Data.List
import Chunk (Chunk(..))
import ASTInternal (Expr(..))

--Get dependency from equation  
get_dep :: (Eq c, Chunk c) => Expr c -> [c]
get_dep (a :/ b) = nub (get_dep a ++ get_dep b)
get_dep (a :* b) = nub (get_dep a ++ get_dep b)
get_dep (a :+ b) = nub (get_dep a ++ get_dep b)
get_dep (a :^ b) = nub (get_dep a ++ get_dep b)
get_dep (a :- b) = nub (get_dep a ++ get_dep b)
get_dep (C c) = [c]
get_dep (Int _) = []
get_dep (Dbl _) = []
get_dep (V _) = []
