{-# OPTIONS -Wall #-} 
module PrintTeX_MK2 where
import ASTTeX_MK2
import ToTeX_MK2
import Text.PrettyPrint
import qualified ASTInternal_MK2 as A
import Prelude hiding (print)
import Config_MK2 (srsTeXParams,colAwidth,colBwidth,verboseDDDescription)
import Helpers_MK2
import Chunk_MK2 (find, findOptional)

genTeX :: A.DocType -> A.Document -> Doc
genTeX typ doc = build typ $ makeDocument doc

build :: A.DocType -> Document -> Doc
build A.SRS doc = buildSRS srsTeXParams doc
build A.LPM _   = error "Unimplemented"
build A.Code _  = error "Unimplemented"

buildSRS :: [A.DocParams] -> Document -> Doc
buildSRS ((A.DocClass sb b) : (A.UsePackages ps) : []) (Document t a c) =
  docclass sb b $$ listpackages ps $$ title (p_spec Pg t) $$ 
  author (p_spec Pg a) $$ begin $$ print c $$ endS
buildSRS _ _ = error "Invalid syntax in Document Parameters"

listpackages :: [String] -> Doc
listpackages []     = empty
listpackages (p:[]) = usepackage p
listpackages (p:ps) = usepackage p $$ listpackages ps

print :: [LayoutObj] -> Doc
print []                         = empty
print ((Section t contents):cs)  = sec (p_spec Pg t) $$ print contents $$ print cs
print ((Paragraph contents):cs)  = text (p_spec Pg contents) $$ print cs
print ((EqnBlock contents):cs)   = makeEquation contents $$ print cs
print ((Table chunks fields):cs) = makeTable chunks fields $$ print cs
print ((Definition dtype chunk fields):cs) = makeDefn dtype chunk fields $$ print cs

p_spec :: Context -> Spec -> String
p_spec Pg (CS c)      = dollar (printSymbol Pg c)
p_spec Pg (E e)       = dollar (p_expr e)
p_spec con (a :+: b)  = p_spec con a ++ p_spec con b
p_spec con (a :-: b)  = p_spec con a ++ "_" ++ brace (p_spec con b)
p_spec con (a :^: b)  = p_spec con a ++ "^" ++ brace (p_spec con b)
p_spec con (CS c)     = printSymbol Pg c
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

makeEquation :: Spec -> Doc
makeEquation contents = 
  text ("\\begin{equation}" ++ p_spec Eqn contents ++ "\\end{equation}")
  --TODO: Add auto-generated labels -> Need to be able to ensure labeling based
  --  on chunk (i.e. "eq:h_g" for h_g = ...

makeTable :: A.Chunks -> [A.Field] -> Doc
makeTable [] _ = error "No chunks provided for creating table"
makeTable _ [] = error "No fields provided for creating table"
makeTable c f  = text ("\\begin{longtable}" ++ brace (lAndDim f)) 
  $$ makeRows c f $$ text "\\end{longtable}"

makeRows :: A.Chunks -> [A.Field] -> Doc
makeRows [] _ = error "No chunks provided for creating row"
makeRows _ [] = error "No fields provided for creating row"
makeRows (c:[]) f = text (makeColumns c f)
makeRows (c:cs) f = text (makeColumns c f) $$ dbs $$ makeRows cs f

makeColumns :: A.Chunk -> [A.Field] -> String
makeColumns _ [] = error "No fields provided for creating column"
makeColumns c (A.Symbol:[]) = p_spec Pg $ CS c 
makeColumns c (A.Symbol:f) = p_spec Pg (CS c) ++ " & " ++ makeColumns c f
makeColumns c (f:[]) = p_spec Pg $ spec  
  (find f c ("Error: missing field " ++ writeField f ++ " in chunk" ++ 
  (printSymbol Code c)))
makeColumns c (f:fs) = p_spec Pg (spec 
  (find f c ("Error: missing field " ++ writeField f ++ " in chunk" ++ 
  (printSymbol Code c)))) ++ " & " ++ makeColumns c fs
  
makeDefn :: A.DType -> A.Chunk -> [A.Field] -> Doc
makeDefn _ _ []   = error "No fields provided for data definition"
makeDefn A.Data c f = beginDataDefn $$ makeDDTable c f $$ endDataDefn

beginDataDefn = text "~" <>newline<+> text "\\noindent \\begin{minipage}{\\textwidth}"
  
endDataDefn = text "\\end{minipage}" <> dbs
  
makeDDTable c f = vcat [
  text $ "\\begin{tabular}{p{"++show colAwidth++"\\textwidth} p{"++show colBwidth++"\\textwidth}}",
  text $ "\\toprule \\textbf{Refname} & \\textbf{DD:$"++ (printSymbol Pg c) ++"$}",
  text $ "\\label{DD:" ++ (printSymbol Code c) ++ "}",
  makeDDRows c f, dbs <+> text ("\\bottomrule \\end{tabular}")
  ]

makeDDRows _ [] = error "No fields to create DD table"
makeDDRows c (f@(A.Symbol):[]) = ddBoilerplate f (text (p_spec Pg (CS c)))
makeDDRows c (f@(A.Symbol):fs) = 
  ddBoilerplate f (text (p_spec Pg (spec (A.CS c)))) $$ makeDDRows c fs
makeDDRows c (f@(A.Description):fs) = ddBoilerplate f $ writeDesc c $$
  (if (verboseDDDescription) then (newline $$ descDependencies c)
  else empty)
makeDDRows c (f:[]) = ddBoilerplate f $ ddWritetext f c
makeDDRows c (f:fs) = ddBoilerplate f (ddWritetext f c) $$ makeDDRows c fs

printSymbol con chunk =
  p_spec con $ spec (find A.Symbol chunk "Error: No symbol for chunk")


ddBoilerplate = \f -> \t -> dbs <+> text "\\midrule" <+> dbs $$ text 
  (writeField f ++ " & ") <> t
ddWritetext = \f -> \c -> text (p_spec Pg (spec (find f c ("Error: missing field" ++ 
  writeField f ++ " in chunk " ++ printSymbol Code c))))

descDependencies :: A.Chunk -> Doc
descDependencies c = writeDescs (unSpec (spec (findOptional A.Dependencies c)))

unSpec :: Spec -> A.Chunks
unSpec (D cs) = cs
unSpec (S s) = []

writeDescs :: A.Chunks -> Doc
writeDescs [] = error "Nothing to write" --Might change this in case chunk has no dependencies
writeDescs (c:[]) = writeDesc c
writeDescs (c:cs) = writeDesc c <+> newline $$ writeDescs cs

writeDesc :: A.Chunk -> Doc
writeDesc c  = text $ 
  p_spec Pg (CS c) ++ " is the " ++ p_spec Pg (spec (find A.Description c "Missing Description"))


