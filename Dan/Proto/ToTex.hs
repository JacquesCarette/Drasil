module ToTex where
import ASTInternal
import Chunk
import Helpers
import Config

expr :: Expr -> String
expr (Chnk c) = getStr Equation c
expr (Dbl d)  = show d
expr (Int i)  = show i
expr (a :* b) = mul a b
expr (a :+ b) = expr a ++ "+" ++ expr b
expr (a :/ b) = fraction (expr (replace_divs a)) (expr (replace_divs b))
expr (Div a b) = expr a ++ "/" ++ expr b

mul a b@(Dbl _) = expr a ++ "*" ++ expr b
mul a b@(Int _) = expr a ++ "*" ++ expr b
mul a b         = expr a ++ expr b

fraction a b = "\\frac{" ++ a ++ "}{" ++ b ++ "}"

replace_divs (a :/ b) = (Div (replace_divs a) (replace_divs b))
replace_divs (a :* b) = (replace_divs a) :* (replace_divs b)
replace_divs (a :+ b) = (replace_divs a) :+ (replace_divs b)
replace_divs a = a

