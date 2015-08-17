{-# OPTIONS -Wall #-} 
module ASTTeX_MK2 where

import ASTInternal_MK2 (Chunk, Chunks, Variable, Unit, Field)
import Text.PrettyPrint

data Expr = Var Variable
          | Dbl Double
          | Int Integer
          | Mul Expr Expr
          | Add Expr Expr
          | Frac Expr Expr
          | Div Expr Expr
          | C Chunk
          | Pow Expr Expr
          | Sub Expr Expr
  deriving (Eq, Ord)
  
data Spec = E Expr
          | S Doc --DocType instead of strings here.
          | Spec :+: Spec
          | Spec :^: Spec
          | Spec :-: Spec
          | M Unit
          | CS Chunk --No need for Format / Empty / Unicode here, they will be converted to TeX specific strings. As will Spec combinations.
          
data Document = Document Title Author [LayoutObj]
type Title = Spec
type Author = Spec
type Contents = Spec

data LayoutObj = Table Chunks [Field]
               | Section Title [LayoutObj]
               | Paragraph Contents