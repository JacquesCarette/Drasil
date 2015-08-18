{-# OPTIONS -Wall #-} 
module PrintTeX_MK2 where
import ASTTeX_MK2
import ToTeX_MK2
import Text.PrettyPrint
import qualified ASTInternal_MK2 as A
import Prelude hiding (print)
import Config_MK2 (srsTeXParams)
import Helpers_MK2
import Chunk_MK2 (find)

genTeX :: A.DocType -> A.Document -> Doc
genTeX typ doc = build typ $ makeDocument doc

build :: A.DocType -> Document -> Doc
build A.SRS doc = buildSRS srsTeXParams doc
build A.LPM _   = error "Unimplemented"
build A.Code _  = error "Unimplemented"

buildSRS :: [A.DocParams] -> Document -> Doc
buildSRS ((A.DocClass sb b) : (A.UsePackages ps) : []) (Document t a c) =
  docclass sb b $$ listpackages ps $$ title (p_spec Pg t) $$ 
  author (p_spec Pg a) $$ print c
buildSRS _ _ = error "Invalid syntax in Document Parameters"

listpackages :: [String] -> Doc
listpackages []     = empty
listpackages (p:[]) = usepackage p
listpackages (p:ps) = usepackage p $$ listpackages ps

print :: [LayoutObj] -> Doc
print []                         = empty
print ((Section t contents):cs)  = sec (p_spec Pg t) $$ print contents $$ print cs
print ((Paragraph contents):cs)  = text (p_spec Pg contents) $$ print cs
print ((EqnBlock contents):cs)   = text (p_spec Pg contents) $$ print cs
print ((Table chunks fields):cs) = makeTable chunks fields $$ print cs

p_spec :: Context -> Spec -> String
p_spec Pg (CS c)      = dollar (p_spec Pg (spec (find A.Symbol c "Erroneous use of chunk")))
p_spec con (a :+: b)  = p_spec con a ++ p_spec con b
p_spec con (a :-: b)  = p_spec con a ++ "_" ++ brace (p_spec con b)
p_spec con (a :^: b)  = p_spec con a ++ "^" ++ brace (p_spec con b)
p_spec con (CS c)     = p_spec con $ spec (find A.Symbol c "Erroneous use of chunk")
p_spec _ (S s)        = s
p_spec _ (E e)        = p_expr e

p_expr :: Expr -> String
p_expr (Var v)    = v
p_expr (Dbl d)    = show d
p_expr (Int i)    = show i
p_expr (Add a b)  = p_expr a ++ "+" ++ p_expr b
p_expr (Sub a b)  = p_expr a ++ "-" ++ p_expr b
p_expr (Mul a b)  = mul a b
p_expr (Frac a b) = fraction (p_expr a) (p_expr b) --Found in Helpers_MK2
p_expr (Div a b)  = p_expr a ++ "/" ++ p_expr b
p_expr (Pow a b)  = p_expr a ++ "^" ++ brace (p_expr b)
p_expr (C c)      = p_spec Eqn $ spec (find A.Equation c "No equation or symbol for chunk")

mul :: Expr -> Expr -> String
mul a b@(Dbl _) = p_expr a ++ "*" ++ p_expr b
mul a b@(Int _) = p_expr a ++ "*" ++ p_expr b
mul a b         = p_expr a ++ p_expr b

makeTable _ _ = error "need to implement tables"