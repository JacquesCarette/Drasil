{-# OPTIONS -Wall #-} 
module ASTTex where
import qualified ASTInternal as AST

data TExp = Var AST.Variable
          | Dbl Double
          | Int Integer
          | Mul TExp TExp
          | Add TExp TExp
          | Frac TExp TExp
          | Div TExp TExp
          | C (AST.Chunk AST.FName AST.FDesc)
          | Pow TExp TExp
          | Sub TExp TExp
  deriving (Eq, Ord)