{-# OPTIONS -Wall #-} 
module PrintC where

import ASTC
import qualified ASTInternal as AST
import ToC
import Config
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified PrintPlain as P
import Helpers

code :: Chunk -> AST.CodeType -> String
code c (AST.Calc) = printMeth $ buildMethod c

buildMethod :: Chunk -> Method
buildMethod c = M ("calc_"++P.plaintext c) (AST.get_dep (get AST.Equation c)) (buildStmt c)

buildStmt :: Chunk -> Stmt
buildStmt c = Return (expr (get AST.Equation c))

get :: AST.FName -> Chunk -> AST.Expr
get n c = pull (fromMaybe (error "No field found") (Map.lookup n c))

pull :: AST.FDesc -> AST.Expr
pull (AST.E e) = e
pull _ = error "Not an expr"

printMeth :: Method -> String
printMeth (M name params stmt) = cMeth (printPrec precision) name params stmt

printPrec :: AST.Precision -> String
printPrec (AST.Single) = "float"
printPrec (AST.Double) = "double"

cMeth :: String -> Name -> [Chunk] -> Stmt -> String
cMeth ret name params stmt =
  ret ++ " " ++ name ++ paren (paramList params) ++ "\n{\n\t" ++ sPrint stmt 
    ++ "\n}"
  
paramList :: [Chunk] -> String
paramList [] = ""
paramList (p:[]) = printPrec (precision) ++ " " ++ p_expr (expr (AST.C p))
paramList (p:ps) = 
  printPrec (precision)++ " " ++ p_expr (expr (AST.C p)) ++ ", " ++ paramList ps

sPrint :: Stmt -> String  
sPrint (Return e)     = "return " ++ p_expr e ++ ";"
sPrint (Wrap e)       = p_expr e ++ ";"
sPrint (Block [])     = ""
sPrint (Block (s:ss)) = sPrint s ++ "\n" ++ sPrint (Block ss)

p_expr :: Expr -> String
p_expr (V v) = v
p_expr (Dbl d) = show d
p_expr (Int i) = show i
p_expr (Mul a b) = p_expr a ++ "*" ++ p_expr b
p_expr (Div a b) = p_expr a ++ "/" ++ p_expr b
p_expr (Pow a b) = p_expr a ++ "^" ++ paren (p_expr b)
p_expr (Add a b) = paren $ p_expr a ++ "+" ++ p_expr b
p_expr (Sub a b) = paren $ p_expr a ++ "-" ++ p_expr b