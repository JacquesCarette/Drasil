{-# OPTIONS -Wall #-} 
module ASTPlain where

import qualified ASTInternal as AST
type Chunk = AST.Chunk AST.FName AST.FDesc

data Expr = Var AST.Variable
          | Dbl Double
          | Int Integer
          | Mul Expr Expr
          | Add Expr Expr
          | Div Expr Expr
          | C Chunk
          | Pow Expr Expr
          | Sub Expr Expr
  deriving (Eq, Ord)