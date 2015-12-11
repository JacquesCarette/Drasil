{-# OPTIONS -Wall #-} 
module ASTTeX where

import ASTInternal (Variable)
import Spec ()
import Symbol

--Might want to create our own TeX chunk to avoid cascading modes since they're
--pointless once we've decided to use TeX.
data Expr = Var Variable
          | Dbl Double
          | Int Integer
          | Mul Expr Expr
          | Add Expr Expr
          | Frac Expr Expr
          | Div Expr Expr
          -- | C Chunk
          | Pow Expr Expr
          | Sub Expr Expr
  deriving Eq

--Probably needs to be converted to GADT
  
data Spec = E Expr
          | S String
          | Spec :+: Spec
          | Spec :^: Spec
          | Spec :-: Spec
          | Spec :/: Spec
          | N Symbol Parameters Variables
          -- | M Unit
          -- | CS Chunk --No need for Format / Empty / Unicode here, they will be converted to TeX specific strings. As will Spec combinations.
          -- | D [Chunk]
data Document = Document Title Author [LayoutObj]
type Title = Spec
type Author = Spec
type Contents = Spec

data LayoutObj = Table [[Spec]]
               | Section Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               -- | Definition DType c [c -> Spec]
