{-# OPTIONS -Wall #-} 
module ASTHTML where

import ASTInternal (Variable)
import Symbol (Symbol)
import Spec (USymb)
import ASTCode (Code)

data Expr = Var   Variable
          | Dbl   Double
          | Int   Integer
          | Mul   Expr Expr
          | Add   Expr Expr
          | Frac  Expr Expr
          | Div   Expr Expr
          | Pow   Expr Expr
          | Sub   Expr Expr
          | Sym   Symbol
          | Eq    Expr Expr
          | Dot   Expr Expr
          | Neg   Expr

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
          | Ref Spec

data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [Spec]
type Tags     = [String]
type Label    = Spec
type Filepath = String
type Caption  = Spec

data LayoutObj = Table Tags [[Spec]] Label Bool Caption
               | Header Int Contents
               | Paragraph Contents
               | HDiv Tags [LayoutObj]
               | Tagless Contents
               | CodeBlock Code
               | Definition String [(String,LayoutObj)]
               | List ListType Items
               | Figure Label Caption Filepath
               -- | Span Tags Contents
               
data ListType = Ordered | Unordered | Simple

instance Show ListType where
  show Ordered   = "o"
  show Unordered = "u"
  show Simple    = error "Printing Simple list failed, see ASTHTML/PrintHTML"