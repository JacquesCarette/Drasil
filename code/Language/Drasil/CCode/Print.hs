module Language.Drasil.CCode.Print(printCode, genCode) where

import Language.Drasil.CCode.AST
import Language.Drasil.CCode.Helpers
import Language.Drasil.CCode.Import
import qualified Language.Drasil.Chunk.Module as Mod
import Language.Drasil.Chunk
import Language.Drasil.Printing.Helpers (indent, hat, ast, pls, slash, hyph,
  assign, eq, leq, lt, geq, gt, angbrac)
import Data.Char (toUpper)

import Text.PrettyPrint
import Control.Lens ((^.))

newtype CType = CType Type

ptype :: Type -> String
ptype IntType     = "int"
ptype VoidType    = "void"
ptype StrType     = "char*"
ptype (PtrType t) = "*" ++ ptype t
ptype DblType     = "double"

instance Show CType where
  show (CType t) = ptype t

genCode :: Lang -> Mod.ModuleChunk -> [(String, Doc)]
genCode CLang mc  = let codeMod = toCodeModule CLang mc
                        codeDoc = printCode codeMod
                        headerDoc = printCode $
                                      toHeader CLang (mc ^. name) codeMod
                    in  [ ( mc ^. name ++ ".c" , codeDoc) ,
                          ( mc ^. name ++ ".h" , headerDoc)
                        ]

--genHeader :: Code -> Doc
--genHeader (C _)

printCode :: Code -> Doc
printCode (C h v m)   = (vcat $ map header h) $+$
                        text "" $+$
                        (vcat $ map varDecl v) $+$
                        text "" $+$
                        (vcat $ map method m)
printCode (H n md)    = let def = map toUpper n ++ "_H_INCLUDED"
                        in  text ("#ifndef " ++ def) $+$
                            text ("#define " ++ def) $+$
                            text "" $+$
                            (vcat $ map (\x -> methodDecl x <> text ";") md) $+$
                            text "" $+$
                            text ("#endif")


header :: Header -> Doc
header (Library h) = text "#include" <+> (text . angbrac) (h++".h")
header (Local h) = text "#include" <+> (doubleQuotes . text) (h++".h")

var :: VarDecl -> Doc
var (VarDecl t v) = text (ptype t) <+> text v

varDecl :: VarDecl -> Doc
varDecl v = var v <> text ";"

method :: Method -> Doc
method (d, ss) = methodDecl d <> text "{" $+$
  text "    " <> (vcat $ map stat ss) $+$ -- TODO: update tabbing style
  text "}" $+$
  text ""

methodDecl :: MethodDecl -> Doc
methodDecl (MethodDecl t n ds) = text (ptype t) <+> text n <> parens (args ds)
  
-- this should also be a fold; and it should go to Doc, not String!
--args :: [ArgsDecl] -> String
--args [] = ""
--args ((ArgsDecl t v):ds) = (ptype t ++ " " ++ v) ++ ", " ++ args ds

args :: [VarDecl] -> Doc
args [] = empty
args ds = hsep $ punctuate (text ",") (map var ds)

stat :: Statement -> Doc
stat (Declare t v Nothing)      = text (ptype t) <+> text v <+> text ";"
stat (Declare t v (Just e))     = text (ptype t) <+> stat (Assign v e)
stat (Assign v e)               = text v <+> assign <+> code e <> text ";"
stat (If e sthen Nothing)       = text "if" <+> parens (code e) <+> text "{" $+$
                                    indent (vcat $ map stat sthen) $+$
                                    text "}"
stat (If e sthen (Just selse))  = stat (If e sthen Nothing) <+> text "else" <+> text "{" $+$
                                    indent (vcat $ map stat selse) $+$
                                    text "}"
stat (Print s)                  = text "printf" <> parens (doubleQuotes (text s)) <> text ";"
stat (MethodCall m a)           = text (getMethodName m) <> parens (hsep $ punctuate (text ",") (map code a)) <> text ";"
stat (Return c)                 = text "return" <+> code c <> text ";"


code :: CodeExpr -> Doc
code (Var v)    = text v
code (Int i)    = text (show i)
code (Dbl d)    = text (show d)
code (Pow b e)  = code b <> hat <> parens (code e)
code (Mult x y) = code x <+> ast <+> code y
code (Add x y)  = parens (code x <+> pls <+> code y)
code (Div n d)  = parens (code n <+> slash <+> code d)
code (Sub x y)  = parens (code x <+> hyph <+> code y)
code (Eq x y)   = code x <+> eq <+> code y
code (Leq x y)  = code x <+> leq <+> code y
code (Lt x y)   = code x <+> lt <+> code y
code (Geq x y)  = code x <+> geq <+> code y
code (Gt x y)   = code x <+> gt <+> code y
