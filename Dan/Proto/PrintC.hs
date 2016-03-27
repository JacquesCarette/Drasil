module PrintC where

import ASTCode
import Helpers (paren, hat, ast, pls, slash, hyph)

import Text.PrettyPrint

newtype CType = CType { getType :: Type }

instance Show CType where
  show (CType IntType)     = "int"
  show (CType VoidType)    = "void"
  show (CType StrType)     = "char*"
  show (CType (PtrType t)) = "*" ++ show (CType t)
  show (CType DblType)     = "double"
  
printCode :: Code -> Doc
printCode (C x) = vcat $ map printMethod x

printMethod :: Method -> Doc
printMethod (d, ss) = printDecl d <> text "{" $$ 
  text "    " <> printStatements ss $$ -- TODO: update tabbing style
  text "}"

printDecl :: Declaration -> Doc
printDecl (MethDecl t n ds) = text (show (CType t)) <+> text n <> 
  text (paren $ printArgs ds)
printDecl _                 = error "Unimplemented declaration in PrintC"
  
-- this should also be a fold
printArgs :: [Declaration] -> String
printArgs [] = ""
printArgs ((ArgDecl t v):[])   = show (CType t) ++ " " ++ v
printArgs (x@(ArgDecl _ _):ds) = printArgs [x] ++ ", " ++ printArgs ds
printArgs _ = error "Attempted to print non-argument as an argument. See PrintC"

printStatements :: [Statement] -> Doc
printStatements sl = vcat $ map printStmt sl

printStmt :: Statement -> Doc
printStmt (Return c) = text "return" <+> printCE c <> text ";"

printCE :: CodeExpr -> Doc
printCE (Var v)    = text v
printCE (Int i)    = text (show i)
printCE (Dbl d)    = text (show d)
printCE (Pow b e)  = printCE b <> hat <> parens (printCE e)
printCE (Mult x y) = printCE x <+> ast <+> printCE y
printCE (Add x y)  = parens (printCE x <+> pls <+> printCE y)
printCE (Div n d)  = parens (printCE n <+> slash <+> printCE d)
printCE (Sub x y)  = parens (printCE x <+> hyph <+> printCE y)
