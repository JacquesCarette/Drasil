{-# OPTIONS -Wall #-} 
module PrintTex where
import ASTTex
import ToTex
import qualified Data.Map.Strict as Map
import Config
import Text.PrettyPrint
import Data.Maybe
import qualified ASTInternal as AST
import Helpers

p_expr :: TExp -> String
p_expr (C c) = getStr AST.Equation c AST.Eqn
p_expr (Dbl d)  = show d
p_expr (Int i)  = show i
p_expr (Mul a b) = mul a b
p_expr (Add a b) = p_expr a ++ "+" ++ p_expr b
p_expr (Frac a b) = fraction (p_expr a) (p_expr b)
p_expr (Div a b) = p_expr a ++ "/" ++ p_expr b
p_expr (Var v) = v
p_expr (Pow a b) = p_expr a ++ "^" ++ p_expr b

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
format_Tex _ (AST.M unit) = writeUnit unit
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
  
printSIU :: [AST.Chunk AST.FName AST.FDesc] -> [AST.FName] -> Doc -> Doc -> [Doc]
printSIU [] _ _ _ = [empty]
printSIU _ [] _ _ = [empty]
printSIU (c:cs) (x:xs) between after = 
  [printSym (fromMaybe (error "not found") (Map.lookup AST.SIU c)) <+> eq] ++
  [(get x c AST.Pg <+> between)] ++ (get_all xs c AST.Pg) ++ [after] ++
  (printSIU cs (x:xs) between after)
  
get_all :: [AST.FName] -> AST.Chunk AST.FName AST.FDesc -> AST.Context -> [Doc]
get_all [] _ _= [empty]
get_all (x:xs) c con = [(get x c con)] ++ get_all xs c con
  
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

writeUnit :: AST.Unit -> String  
writeUnit (AST.Fundamental s) = s
writeUnit (AST.Derived s e) = s ++ " = " ++ pU_expr (expr e)

pU_expr :: TExp -> String
pU_expr (C c) = getStr AST.SIU c AST.Eqn
pU_expr (Dbl d)  = show d
pU_expr (Int i)  = show i
pU_expr (Mul a b) = mulU a b
pU_expr (Add a b) = pU_expr a ++ "+" ++ pU_expr b
pU_expr (Frac a b) = fraction (pU_expr a) (pU_expr b)
pU_expr (Div a b) = pU_expr a ++ "/" ++ pU_expr b
pU_expr (Var v) = v
pU_expr (Pow a b) = pU_expr a ++ "^" ++ pU_expr b

mulU :: TExp -> TExp -> String
mulU a b@(Dbl _) = pU_expr a ++ "*" ++ pU_expr b
mulU a b@(Int _) = pU_expr a ++ "*" ++ pU_expr b
mulU a b         = pU_expr a ++ pU_expr b

printSym :: AST.Spec -> Doc
printSym (AST.M (AST.Derived s _)) = text s
printSym (AST.M (AST.Fundamental s)) = text s
printSym _ = error "I can't let you do that Dave"