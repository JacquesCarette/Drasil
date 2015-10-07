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

type Chunk = AST.Chunk

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
p_expr (Sub a b) = p_expr a ++ "-" ++ p_expr b

mul :: TExp -> TExp -> String
mul a b@(Dbl _) = p_expr a ++ "*" ++ p_expr b
mul a b@(Int _) = p_expr a ++ "*" ++ p_expr b
mul a b         = p_expr a ++ p_expr b

fraction :: String -> String -> String
fraction a b = "\\frac{" ++ a ++ "}{" ++ b ++ "}"

--This needs to be moved elsewhere. Preferably to the main file.
format :: AST.Context -> AST.FDesc -> String
format c spec = case output of AST.TeX -> format_Tex c spec
                               AST.Plain -> ""
--This is fine here.
format_Tex :: AST.Context -> AST.FDesc -> String  
format_Tex _ (AST.E e) = p_expr $ expr e
format_Tex _ (AST.S s) = s
format_Tex _ (AST.U x) = uni x
format_Tex c (AST.F AST.Hat x) = "\\hat{" ++ format_Tex c x ++ "}"
format_Tex c (AST.F AST.Vector x) = "\\bf{" ++ format_Tex c x ++ "}"
format_Tex AST.Pg (a AST.:-: b) = 
  "$"++format_Tex AST.Pg a ++"_{"++ (format_Tex AST.Pg b)++"}$"
format_Tex c (a AST.:-: b) = 
  format_Tex c a ++ "_{" ++ format_Tex c b ++ "}"
format_Tex AST.Pg (a AST.:^: b) = 
  "$"++format_Tex AST.Pg a ++"^{"++ format_Tex AST.Pg b++"}$"
format_Tex AST.Pg (a AST.:+: b) = 
  "$"++format_Tex AST.Pg a ++ format_Tex AST.Pg b ++ "$"
format_Tex c (a AST.:+: b) = format_Tex c a ++ format_Tex c b
format_Tex c (a AST.:^: b) = format_Tex c a ++ "^{" ++ format_Tex c b ++ "}"
format_Tex c (AST.M unit) = writeUnit unit c
format_Tex _ AST.Empty = ""


{-format_Tex AST.Pg a = "$" ++ format_T a ++ "$"
format_Tex AST.Eqn a = format_T a
format_Tex AST.Code a = format_T a

format_T :: AST.FDesc -> String
format_T (AST.E e) = p_expr $ expr e
format_T (AST.S s) = s
format_T (AST.U x) = uni x
format_T (AST.F AST.Hat x) = "\\hat{" ++ format_T x ++ "}"
format_T (AST.F AST.Vector x) = "\\bf{" ++ format_T x ++ "}"
format_T (a AST.:-: b) = format_T a ++ "_{" ++ format_T b ++ "}"
format_T (a AST.:+: b) = format_T a ++ format_T b
format_T (a AST.:^: b) = format_T a ++ "^{" ++ format_T b ++ "}"
format_T (AST.M unit) = writeUnit unit
format_T AST.Empty = ""-}

--This function should be moved elsewhere, preferably somewhere accessible to
  --the recipes, should also be changed to use the internal AST instead of Doc
get :: AST.FName -> Chunk -> AST.Context -> Doc
get name chunk con = text $ getStr name chunk con

--This function can be collapsed into get. Functionality may need to be tweaked.
getStr :: AST.FName -> Chunk -> AST.Context -> String
getStr AST.Equation chunk AST.Eqn = format AST.Eqn (fromMaybe (fromMaybe (error "wut") (Map.lookup AST.Symbol chunk)) (Map.lookup AST.Equation chunk))
getStr AST.Equation chunk con =
  format con (fromMaybe (fromMaybe (error "wut") (Map.lookup AST.VarName chunk)) 
    (Map.lookup AST.Equation chunk))
getStr name chunk con = format con (fromMaybe (AST.Empty) (Map.lookup name chunk))

uni :: AST.Unicode -> String
uni (AST.Tau_L) = "\\tau"
uni (AST.Tau_U) = "\\Tau"
uni (AST.Alpha_L) = "\\alpha"
uni (AST.Alpha_U) = "\\Alpha"
uni (AST.Circle) = "\\circ"
uni (AST.Delta_U) = "\\Delta"
uni (AST.Delta_L) = "\\delta"
uni (AST.Rho_U) = "\\Rho"
uni (AST.Rho_L) = "\\rho"
uni (AST.Phi_U) = "\\Phi"
uni (AST.Phi_L) = "\\phi"
-- uni _ = error "Invalid unicode character selection"
  
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

writeUnit :: AST.Unit -> AST.Context -> String  
writeUnit (AST.Fundamental s) _ = s
writeUnit (AST.Derived s e) AST.Pg = "$" ++ s ++ " = " ++ pU_expr (expr e) ++ "$"
writeUnit (AST.Derived s e) _ = s ++ " = " ++ pU_expr (expr e)

printSIU :: [Chunk] -> [AST.FName] -> Doc -> Doc -> [Doc]
printSIU [] _ _ _ = [empty]
printSIU _ [] _ _ = [empty]
printSIU (c:cs) (x:xs) bet aft = 
  [unitSymbol (fromMaybe (AST.Empty) (Map.lookup AST.SIU c)) <+> bet <+>
  get x c AST.Pg] ++ gets xs c AST.Pg ++ [aft] ++ printSIU cs (x:xs) bet aft

gets :: [AST.FName] -> Chunk -> AST.Context -> [Doc]
gets [] _ _ = [empty]
gets (x:xs) c con = [get x c con] ++ gets xs c con

unitSymbol :: AST.FDesc -> Doc
unitSymbol (AST.M (AST.Derived s _)) = text s
unitSymbol (AST.M (AST.Fundamental s)) = text s
unitSymbol _ = empty

pU_expr :: TExp -> String
pU_expr (C c) = getStr AST.SIU c AST.Eqn
pU_expr (Dbl d)  = show d
pU_expr (Int i)  = show i
pU_expr (Mul a b) = mulU a b
pU_expr (Add a b) = pU_expr a ++ "+" ++ pU_expr b
pU_expr (Frac a b) = "\\mathrm{" ++ fraction (pU_expr a) (pU_expr b) ++ "}"
pU_expr (Div a b) = pU_expr a ++ "/" ++ pU_expr b
pU_expr (Var v) = v
pU_expr (Pow a b) = pU_expr a ++ "^" ++ pU_expr b
pU_expr (Sub a b) = pU_expr a ++ "-" ++ pU_expr b

mulU :: TExp -> TExp -> String
mulU a b@(Dbl _) = pU_expr a ++ "*" ++ pU_expr b
mulU a b@(Int _) = pU_expr a ++ "*" ++ pU_expr b
mulU a b         = pU_expr a ++ " " ++ pU_expr b --For clarity in units

makeTable :: AST.LayoutObj -> Doc
makeTable t = vcat (beginTable ++ (createRows t) ++ endTable)

beginTable :: [Doc]
beginTable = [text "~\\newline \\begin{longtable}{l l p{11cm}}"]

endTable :: [Doc]
endTable = [text "\\end{longtable}"]

createRows :: AST.LayoutObj -> [Doc]
createRows (AST.Table [] _) = [empty]
createRows (AST.Table _ []) = [empty]
createRows (AST.Table (c:cs) f) = [createColumns c f] ++ [dbs] ++ createRows (AST.Table cs f)

createColumns :: Chunk -> [AST.Field] -> Doc
createColumns _ [] = empty
createColumns c (f:[])			  	  = get f c AST.Pg
createColumns c (f:fs)			  	  = createColumns c (f:[]) <+> text "& \\blt" <+> createColumns c fs
