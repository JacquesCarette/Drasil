{-# OPTIONS -Wall #-} 
module PrintTex where
import ASTTex
import ToTex
import qualified Data.Map.Strict as Map
import Config
import Text.PrettyPrint
import Data.Maybe
import qualified ASTInternal as AST

p_expr :: TExp -> String
p_expr (C c) = getStr AST.Equation c AST.Eqn
p_expr (Dbl d)  = show d
p_expr (Int i)  = show i
p_expr (Mul a b) = mul a b
p_expr (Add a b) = p_expr a ++ "+" ++ p_expr b
p_expr (Frac a b) = fraction (p_expr a) (p_expr b)
p_expr (Div a b) = p_expr a ++ "/" ++ p_expr b
p_expr (Var v) = v

mul :: TExp -> TExp -> String
mul a b@(Dbl _) = p_expr a ++ "*" ++ p_expr b
mul a b@(Int _) = p_expr a ++ "*" ++ p_expr b
mul a b         = p_expr a ++ p_expr b

fraction :: String -> String -> String
fraction a b = "\\frac{" ++ a ++ "}{" ++ b ++ "}"

--This needs to be moved elsewhere. Preferably to the main file.
format :: AST.Context -> AST.Spec -> String
format c spec = case output of AST.TeX -> format_Tex c spec
                               AST.Plain -> ""
--This is fine here.
format_Tex :: AST.Context -> AST.Spec -> String                             
format_Tex _ (AST.E e) = p_expr $ expr e
format_Tex _ (AST.S s) = s
format_Tex _ (AST.U x) = uni x
format_Tex AST.Pg (a AST.:- b) = 
  "$"++format_Tex AST.Pg a ++"_{"++ (format_Tex AST.Pg b)++"}$"
format_Tex c (a AST.:- b) = 
  format_Tex c a ++ "_{" ++ format_Tex c b ++ "}"
format_Tex AST.Pg (a AST.:^ b) = 
  "$"++format_Tex AST.Pg a ++"^{"++ format_Tex AST.Pg b++"}$"
format_Tex c (a AST.:^ b) = format_Tex c a ++ "^{" ++ format_Tex c b ++ "}"
format_Tex _ (AST.M _) = ""
format_Tex _ AST.Empty = ""

--This function should be moved elsewhere, preferably somewhere accessible to
  --the recipes, should also be changed to use the internal AST instead of Doc
get :: AST.FName -> AST.Chunk AST.FName AST.FDesc -> AST.Context -> Doc
get name chunk con = text $ getStr name chunk con

--This function can be collapsed into get. Functionality may need to be tweaked.
getStr :: AST.FName -> AST.Chunk AST.FName AST.FDesc -> AST.Context -> String
getStr name chunk con = format con (fromMaybe (AST.Empty) (Map.lookup name chunk))

uni :: AST.Unicode -> String
uni (AST.Tau_L) = "\\tau"
uni (AST.Tau_U) = "\\Tau"
uni (AST.Alpha_L) = "\\alpha"
uni (AST.Alpha_U) = "\\Alpha"
-- uni _ = error "Invalid unicode character selection"

getWFormat :: [AST.Chunk AST.FName AST.FDesc] -> (AST.FName,AST.FName) -> Doc ->
                Doc -> [Doc]
getWFormat [] _ _ _ = [empty]
getWFormat (c:cs) (x,y) between after = 
  [(get x c AST.Pg <+> between <+> get y c AST.Pg <> after)] ++
  (getWFormat cs (x,y) between after)
  
writeDep :: [AST.FName] -> AST.Dependency -> String -> String -> AST.Context -> [Doc]
writeDep [] _ _ _ _ = [empty]
writeDep _ [] _ _ _ = [empty]
writeDep (x:[]) (c:[]) _ es con = 
  [get x c con <+> text es]
writeDep (x:[]) (c:_) _ es con= 
  [get x c con <+> text es]
writeDep (x:xs) (c:[]) is es con= 
  [get x c con <+> text is] ++ writeDep xs [c] is es con
writeDep (x:xs) (c:cs) is es con= 
  writeDep (x:xs) (c:[]) is es con ++ writeDep (x:xs) cs is es con