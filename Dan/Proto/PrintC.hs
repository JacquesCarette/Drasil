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
code c (AST.Calc) = printMeth precision $ buildMethod c

buildMethod :: Chunk -> Method
buildMethod c = M ("calc_"++P.plaintext c) (AST.get_dep (get AST.Equation c)) (buildStmt c)

buildStmt :: Chunk -> Stmt
buildStmt c = Return (expr (get AST.Equation c))

get :: AST.FName -> Chunk -> AST.Expr
get n c = pull (fromMaybe (error "No field found") (Map.lookup n c))

pull :: AST.FDesc -> AST.Expr
pull (AST.E e) = e
pull _ = error "Not an expr"

printMeth :: AST.Precision -> Method -> String
printMeth (AST.Single) (M name params stmt) = cMeth "float" name params stmt
printMeth (AST.Double) (M name params stmt) = cMeth "double" name params stmt

cMeth :: String -> Name -> [Chunk] -> Stmt -> String
cMeth ret name params stmt =
  ret ++ " " ++ name ++ paren (paramList params) ++ sPrint stmt
  
paramList :: [Chunk] -> String
paramList [] = ""
paramList (p:ps) = p_expr (expr (AST.C p)) ++ paramList ps
  
sPrint (Return e)     = "return " ++ p_expr e ++ ";"
sPrint (Wrap e)       = p_expr e ++ ";"
sPrint (Block [])     = ""
sPrint (Block (s:ss)) = sPrint s ++ sPrint (Block ss)

--TODO--
p_expr _ = ""