{-# OPTIONS -Wall #-} 
module ToTex where
import ASTInternal
import qualified ASTTex as T

expr :: Expr -> T.TExp
expr (V v)    = T.Var v
expr (Dbl d)  = T.Dbl d
expr (Int i)  = T.Int i
expr (a :* b) = T.Mul (expr a) (expr b)
expr (a :+ b) = T.Add (expr a) (expr b)
expr (a :/ b) = T.Frac (replace_divs a) (replace_divs b)
expr (a :^ b) = T.Pow (expr a) (expr b)
expr (a :- b) = T.Sub (expr a) (expr b)
expr (C c)    = T.C c

replace_divs :: Expr -> T.TExp
replace_divs (a :/ b) = T.Div (replace_divs a) (replace_divs b)
replace_divs (a :+ b) = T.Add (replace_divs a) (replace_divs b)
replace_divs (a :* b) = T.Mul (replace_divs a) (replace_divs b)
replace_divs (a :^ b) = T.Pow (replace_divs a) (replace_divs b)
replace_divs (a :- b) = T.Sub (replace_divs a) (replace_divs b)
replace_divs a = expr a