{-# OPTIONS -Wall #-} 
module ASTTeX where

import ASTInternal (Variable)
import Spec ()

--Might want to create our own TeX chunk to avoid cascading modes since they're
--pointless once we've decided to use TeX.
data Expr = Var Variable
          | Dbl Double
          | Int Integer
          | Mul Expr Expr
          | Add Expr Expr
          | Frac Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Sub Expr Expr
  deriving Eq

data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Spec :^: Spec -- superscript
          | Spec :-: Spec -- subscript
          | Spec :/: Spec -- frac

data Document = Document Title Author [LayoutObj]
type Title = Spec
type Author = Spec
type Contents = Spec

data LayoutObj = Table [[Spec]]
               | Section Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               -- | Definition DType c [c -> Spec]
