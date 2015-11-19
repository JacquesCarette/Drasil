{-# OPTIONS -Wall #-} 
module ASTTeX where

import Chunk (Chunk(..))
import ASTInternal (Variable)
import Unit (Unit)
import Spec (DType)
import Format (TeX(..))

--Might want to create our own TeX chunk to avoid cascading modes since they're
--pointless once we've decided to use TeX.
data Expr = Var Variable
          | Dbl Double
          | Int Integer
          | Mul Expr Expr
          | Add Expr Expr
          | Frac Expr Expr
          | Div Expr Expr
          | C (Chunk TeX)
          | Pow Expr Expr
          | Sub Expr Expr
  deriving Eq
  
data Spec = E Expr
          | S String
          | Spec :+: Spec
          | Spec :^: Spec
          | Spec :-: Spec
          | M (Unit TeX)
          | CS (Chunk TeX) --No need for Format / Empty / Unicode here, they will be converted to TeX specific strings. As will Spec combinations.
          | D [c TeX]
data Document = Document Title Author [LayoutObj]
type Title = Spec
type Author = Spec
type Contents = Spec

data LayoutObj = Table [c TeX] [Field]
               | Section Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               | Definition DType (Chunk TeX) [Field]
               
data Context = Pg | Eqn | Code -- paragraph, equation, or code. This will affect
                               -- the formatting of the finished document.
