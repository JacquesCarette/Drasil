module ToC where

import ASTInternal
import qualified ASTC as C

expr :: Expr -> C.Expr
expr (V v)    = C.V v
expr (Dbl d)  = C.Dbl d
expr (Int i)  = C.Int i
expr (a :* b) = C.Mul (expr a) (expr b)
expr (a :+ b) = C.Add (expr a) (expr b)
expr (a :/ b) = C.Div (expr a) (expr b)
expr (a :^ b) = C.Pow (expr a) (expr b)
expr (a :- b) = C.Sub (expr a) (expr b)
 -- How to handle chunks?