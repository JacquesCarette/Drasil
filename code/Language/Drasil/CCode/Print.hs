module Language.Drasil.CCode.Print(printCode) where

import Language.Drasil.CCode.AST
import Language.Drasil.Printing.Helpers (paren, hat, ast, pls, slash, hyph)

import Text.PrettyPrint

newtype CType = CType Type

ptype :: Type -> String
ptype IntType     = "int"
ptype VoidType    = "void"
ptype StrType     = "char*"
ptype (PtrType t) = "*" ++ ptype t
ptype DblType     = "double"
  
instance Show CType where
  show (CType t) = ptype t

printCode :: Code -> Doc
printCode (C x) = vcat $ map method x

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
stat (Return c) = text "return" <+> code c <> text ";"

code :: CodeExpr -> Doc
code (Var v)    = text v
code (Int i)    = text (show i)
code (Dbl d)    = text (show d)
code (Pow b e)  = code b <> hat <> parens (code e)
code (Mult x y) = code x <+> ast <+> code y
code (Add x y)  = parens (code x <+> pls <+> code y)
code (Div n d)  = parens (code n <+> slash <+> code d)
code (Sub x y)  = parens (code x <+> hyph <+> code y)
