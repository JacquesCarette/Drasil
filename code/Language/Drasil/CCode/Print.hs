module Language.Drasil.CCode.Print(printCode) where

import Language.Drasil.CCode.AST
import Language.Drasil.Printing.Helpers (indent, paren, hat, ast, pls, slash, hyph, eq, deq, leq, lt, geq, gt, angbrac)

import Text.PrettyPrint

newtype CType = CType Type

ptype :: Type -> String
ptype IntType     = "int"
ptype VoidType    = "void"
ptype StrType     = "char*"
ptype (PtrType t) = "*" ++ ptype t
ptype DblType     = "double"

pheader :: Header -> String
pheader StdLibHeader = "stdlib.h"
pheader StdIOHeader  = "stdio.h"

instance Show CType where
  show (CType t) = ptype t

printCode :: Code -> Doc
printCode (C i m)   = include i $+$
                        text "" $+$
                        (vcat $ map method m)

include :: Include -> Doc
include h = vcat $ map ((<+>) (text "#include")) $ map (text . angbrac . pheader) h

method :: Method -> Doc
method (d, ss) = methodDecl d <> text "{" $$ 
  text "    " <> (vcat $ map stat ss) $$ -- TODO: update tabbing style
  text "}"

methodDecl :: MethodDecl -> Doc
methodDecl (MethodDecl t n ds) = text (ptype t) <+> text n <> 
  text (paren $ args ds)
  
-- this should also be a fold; and it should go to Doc, not String!
args :: [ArgsDecl] -> String
args [] = ""
args ((ArgsDecl t v):ds) = (ptype t ++ " " ++ v) ++ ", " ++ args ds

stat :: Statement -> Doc
stat (Assign v e)               = text v <+> eq <+> code e <> text ";"
stat (If e sthen Nothing)       = text "if" <+> parens (code e) <+> text "{" $+$
                                    indent (vcat $ map stat sthen) $+$
                                    text "}"
stat (If e sthen (Just selse))  = stat (If e sthen Nothing) <+> text "else" <+> text "{" $+$
                                    indent (vcat $ map stat selse) $+$
                                    text "}"
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
code (Eq x y)   = code x <+> deq <+> code y
code (Leq x y)  = code x <+> leq <+> code y
code (Lt x y)   = code x <+> lt <+> code y
code (Geq x y)  = code x <+> geq <+> code y
code (Gt x y)   = code x <+> gt <+> code y
