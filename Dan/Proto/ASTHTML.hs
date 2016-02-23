{-# OPTIONS -Wall #-} 
module ASTHTML where

import ASTInternal (Variable)
import LayoutObjs (DType)
import Symbol (Symbol)
import Spec (USymb)
import ASTCode (Code)

data Expr = Var Variable
          | Dbl Double
          | Int Integer
          | Mul Expr Expr
          | Add Expr Expr
          | Frac Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Sub Expr Expr
          | Sym Symbol

infixr 5 :+:
data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Spec :^: Spec -- superscript
          | Spec :-: Spec -- subscript
          | Spec :/: Spec -- frac
          | Sy USymb
          | N Symbol
          | HARDNL

data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [Spec]
type Tags = [String]

data LayoutObj = Table Tags [[Spec]]
               | Header Int Contents
               | Paragraph Contents
               | HDiv Tags [LayoutObj]
               | Tagless Contents
               | CodeBlock Code
               | Definition DType [(String,LayoutObj)]
               | List ListType Items
               -- | Span Tags Contents
               
data ListType = Ordered | Unordered

instance Show ListType where
  show Ordered = "o"
  show Unordered = "u"