{-# OPTIONS -Wall #-} 
module ToPlain where
import ASTInternal
import qualified ASTPlain as P

expr :: Expr -> P.Expr
expr (V v)    = P.Var v
expr (Dbl d)  = P.Dbl d
expr (Int i)  = P.Int i
expr (a :* b) = P.Mul (expr a) (expr b)
expr (a :+ b) = P.Add (expr a) (expr b)
expr (a :/ b) = P.Div (expr a) (expr b)
expr (a :^ b) = P.Pow (expr a) (expr b)
expr (a :- b) = P.Sub (expr a) (expr b)
expr (C c)    = P.C c