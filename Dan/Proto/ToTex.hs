{-# OPTIONS -Wall #-} 
module ToTex where
import ASTInternal
import Helpers
import qualified Data.Map.Strict as Map
import Config
import Text.PrettyPrint
import Data.Maybe

expr :: Expr -> String
expr (Chnk c) = getStr Equation c Eqn
expr (Dbl d)  = show d
expr (Int i)  = show i
expr (a :* b) = mul a b
expr (a :+ b) = expr a ++ "+" ++ expr b
expr (a :/ b) = fraction (expr (replace_divs a)) (expr (replace_divs b))
expr (Div a b) = expr a ++ "/" ++ expr b

mul :: Expr -> Expr -> String
mul a b@(Dbl _) = expr a ++ "*" ++ expr b
mul a b@(Int _) = expr a ++ "*" ++ expr b
mul a b         = expr a ++ expr b

fraction :: String -> String -> String
fraction a b = "\\frac{" ++ a ++ "}{" ++ b ++ "}"

replace_divs :: Expr -> Expr
replace_divs (a :/ b) = (Div (replace_divs a) (replace_divs b))
replace_divs (a :* b) = (replace_divs a) :* (replace_divs b)
replace_divs (a :+ b) = (replace_divs a) :+ (replace_divs b)
replace_divs a = a

format :: Context -> Spec -> String
format c spec = case output of TeX -> format_Tex c spec
                               Plain -> ""

format_Tex :: Context -> Spec -> String                             
format_Tex _ (E e) = expr e
format_Tex _ (S s) = s
format_Tex _ (G x) = greek x
format_Tex Pg (a :- b) = 
  "$"++format_Tex Pg a ++"_"++ format_Tex Pg b++"$"
format_Tex c (a :- b) = 
  format_Tex c a ++ "_" ++ format_Tex c b
format_Tex Pg (a :^ b) = 
  "$"++format_Tex Pg a ++"^"++ format_Tex Pg b++"$"
format_Tex c (a :^ b) = format_Tex c a ++ "^" ++ format_Tex c b
format_Tex _ _ = ""

get :: FName -> Chunk FName FDesc -> Context -> Doc
get name chunk con = text $ getStr name chunk con

getStr :: FName -> Chunk FName FDesc -> Context -> String
getStr name chunk con = format con (fromMaybe (Empty) (Map.lookup name chunk))

greek :: GreekChar -> String
greek (Tau_L) = "\\tau"
greek _ = "\\Tau"