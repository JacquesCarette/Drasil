module ASTInternal where
import Chunk
import Data.List

data Expr = Chnk Variable
          | Dbl Double
          | Int Integer
          | Expr :* Expr
          | Expr :+ Expr
          | Expr :/ Expr
          | Div Expr Expr -- used internally (ONLY)

type Variable = Chunk FName FDesc

----------------------------------------------------------------
-- Make things prettier
v = Chnk

--Get dependency from equation  
get_dep :: Expr -> Dependency
--get_dep (Frac a b) = nub (get_dep a ++ get_dep b)
get_dep (a :/ b) = nub (get_dep a ++ get_dep b)
get_dep (a :* b) = nub (get_dep a ++ get_dep b)
get_dep (a :+ b) = nub (get_dep a ++ get_dep b)
get_dep (Chnk c) = [c]
get_dep _ = [] 