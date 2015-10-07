{-# OPTIONS -Wall #-} 
module ASTTex where
import ASTInternal (Chunk, Variable)

data TExp = Var Variable
          | Dbl Double
          | Int Integer
          | Mul TExp TExp
          | Add TExp TExp
          | Frac TExp TExp
          | Div TExp TExp
          | C Chunk
          | Pow TExp TExp
          | Sub TExp TExp
  deriving (Eq, Ord)
