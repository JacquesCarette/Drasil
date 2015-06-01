{-# OPTIONS -Wall #-} 
module ASTPlain where

import ASTInternal (Chunk, Variable)

data Expr = Var Variable
          | Dbl Double
          | Int Integer
          | Mul Expr Expr
          | Add Expr Expr
          | Div Expr Expr
          | C Chunk
          | Pow Expr Expr
          | Sub Expr Expr
  deriving (Eq, Ord)
