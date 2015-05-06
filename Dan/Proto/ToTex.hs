module ToTex where
import ASTInternal
import Chunk
import Helpers

expr :: Expr -> String
expr (Chnk c) = getStr "Equation" c
expr (Dbl d)  = show d
expr (Int i)  = show i
expr (a :* b) = mul a b
expr (a :+ b) = expr a ++ "+" ++ expr b
expr (Frac a b) = fraction (expr a) (expr b)

mul a b@(Dbl _) = expr a ++ "*" ++ expr b
mul a b@(Int _) = expr a ++ "*" ++ expr b
mul a b         = expr a ++ expr b

fraction a b = "\\frac{" ++ a ++ "}{" ++ b ++ "}"