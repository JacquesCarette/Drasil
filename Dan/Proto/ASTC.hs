{-# OPTIONS -Wall #-} 
module ASTC where

import ASTInternal (Chunk, Chunks)

type Name = String

data Method = M Name Chunks Stmt --MethodName Parameters CodeStatements
--CodeType -> Name -> [Chunk] -> Stmt; CodeType determines return type

data Expr = V Name
          | Dbl Double
          | Int Integer
          | Pow Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          
data Stmt = Return Expr
          | Wrap Expr
          | Block [Stmt]
