{-# OPTIONS -Wall #-} 
module ASTTeX where

import ASTInternal (Variable)
import Spec ()
import Symbol (Symbol)
import Unit (USymb)

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
          | Sy USymb
          | N Symbol

data Document = Document Title Author [LayoutObj]
type Title = Spec
type Author = Spec
type Contents = Spec

data LayoutObj = Table [[Spec]]
               | Section Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               -- | Definition DType c [c -> Spec]
