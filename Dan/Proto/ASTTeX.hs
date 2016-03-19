{-# OPTIONS -Wall #-} 
module ASTTeX where

import ASTInternal (Variable)
import Symbol (Symbol)
import Spec (USymb)
import ASTCode (Code)

data Expr = Var  Variable
          | Dbl  Double
          | Int  Integer
          | Mul  Expr Expr
          | Add  Expr Expr
          | Frac Expr Expr
          | Div  Expr Expr
          | Pow  Expr Expr
          | Sub  Expr Expr
          | Sym  Symbol
          | Eq   Expr Expr
          | Dot  Expr Expr
          | Neg  Expr

infixr 5 :+:
data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Spec :^: Spec -- superscript
          | Spec :-: Spec -- subscript
          | Spec :/: Spec -- frac
          | Sy USymb
          | N Symbol
          | Ref Spec
          | HARDNL        -- newline. Temp fix for multi-line descriptions; 
                          -- May move to a new LayoutObj, but only exists in TeX
                          -- so it's not really a big deal ATM.

data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [Spec]
type Depth    = Int
type Label    = Spec
type Filepath = String
type Caption  = Spec

data LayoutObj = Table [[Spec]] Label Bool Title
               | Section Depth Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               | CodeBlock Code
               | Definition String [(String,LayoutObj)]
               | List ListType Items
               | Figure Label Caption Filepath
               
data ListType = Item | Enum | Simple

instance Show ListType where
  show Item = "itemize"
  show Enum = "enumerate"
  show Simple = error "Printing Simple list failed, see ASTTeX/PrintTeX"