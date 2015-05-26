module ASTC where
import qualified ASTInternal as AST

type Name = String
type Chunk = AST.Chunk AST.FName AST.FDesc

data Method = Name [Chunk] Stmt --MethodName Parameters CodeStatements
--CodeType -> Name -> [Chunk] -> Stmt; CodeType determines return type

data Expr = V Name
          | Dbl Double
          | Int Integer
          | Pow Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Call Name Expr
          
data Stmt = Return Expr
          | Wrap Expr
          | Block [Stmt]