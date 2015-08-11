{-# OPTIONS -Wall #-} 
module ToTex_MK2 where
import ASTInternal_MK2
import qualified ASTTeX_MK2 as T
import Text.PrettyPrint

expr :: Expr -> T.Expr
expr (V v)    = T.Var v
expr (Dbl d)  = T.Dbl d
expr (Int i)  = T.Int i
expr (a :* b) = T.Mul (expr a) (expr b)
expr (a :+ b) = T.Add (expr a) (expr b)
expr (a :/ b) = T.Frac (replace_divs a) (replace_divs b)
expr (a :^ b) = T.Pow (expr a) (expr b)
expr (a :- b) = T.Sub (expr a) (expr b)
expr (C c)    = T.C c

replace_divs :: Expr -> T.Expr
replace_divs (a :/ b) = T.Div (replace_divs a) (replace_divs b)
replace_divs (a :+ b) = T.Add (replace_divs a) (replace_divs b)
replace_divs (a :* b) = T.Mul (replace_divs a) (replace_divs b)
replace_divs (a :^ b) = T.Pow (replace_divs a) (replace_divs b)
replace_divs (a :- b) = T.Sub (replace_divs a) (replace_divs b)
replace_divs a = expr a

spec :: Spec -> T.Spec
spec (E e) = T.E (expr e)
spec (S s) = T.S (text s)
spec (a :+: b) = spec a T.:+: spec b
spec (a :-: b) = spec a T.:-: spec b
spec (a :^: b) = spec a T.:^: spec b
spec Empty = T.S empty
spec (U u) = convertUnicode u
spec (M m) = T.M m
spec (CS c) = T.CS c
--spec (F f s) = 

convertUnicode :: Unicode -> T.Spec
convertUnicode Tau_L = T.S $ text "\\tau"
convertUnicode Tau_U = T.S $ text "\\Tau"
convertUnicode Alpha_L = T.S $ text "\\alpha"
convertUnicode Alpha_U = T.S $ text "\\Alpha"
convertUnicode Circle = T.S $ text "\\circ"
convertUnicode Delta_U = T.S $ text "\\Delta"
convertUnicode Delta_L = T.S $ text "\\delta"
convertUnicode Rho_U = T.S $ text "\\Rho"
convertUnicode Rho_L = T.S $ text "\\rho"
convertUnicode Phi_U = T.S $ text "\\Phi"
convertUnicode Phi_L = T.S $ text "\\phi"
