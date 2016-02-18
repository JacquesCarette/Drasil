{-# OPTIONS -Wall #-} 
module ASTTeX where

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
          | HARDNL        -- newline. Temp fix for multi-line descriptions; 
                          -- May move to a new LayoutObj, but only exists in TeX
                          -- so it's not really a big deal ATM.

data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [Spec]


data LayoutObj = Table [[Spec]]
               | Section Title [LayoutObj]
               | SubSection Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               | CodeBlock Code
               | Definition DType [(String,LayoutObj)]
               | List ListType Items
               
data ListType = Item | Enum

instance Show ListType where
  show Item = "itemize"
  show Enum = "enumerate"