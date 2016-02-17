{-# OPTIONS -Wall #-} 
module ASTHTML where
--This is a duplicate of ASTTeX, if anything they should be combined into
--  something like "ASTLayout", or the printHTML should be mined and ToHTML
--  should take care of tagging/arranging things.
import ASTInternal (Variable)
import Spec (DType)
import Symbol (Symbol)
import Unit (USymb)
import ASTCode

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

type Tags = [String]

data LayoutObj = Table [[Spec]]
               | Section Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               | CodeBlock Code
               | Definition DType [(String,LayoutObj)]
               -- | HDiv Tags LayoutObj
               -- | Span Tags Contents