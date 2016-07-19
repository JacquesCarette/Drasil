{-# OPTIONS -Wall #-} 
module Language.Drasil.TeX.AST where

import Language.Drasil.Expr (Variable)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (USymb, RefType)
import Language.Drasil.CCode.AST (Code)

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
          | Lt   Expr Expr
          | Gt   Expr Expr
          | Dot  Expr Expr
          | Neg  Expr
          | Call Expr [Expr]
          | Case [(Expr,Expr)]

infixr 5 :+:
data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Spec :^: Spec -- superscript
          | Spec :-: Spec -- subscript
          | Spec :/: Spec -- frac
          | Sy USymb
          | N Symbol
          | Ref RefType Spec
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
               | Section Depth Title [LayoutObj] Label
               | Paragraph Contents
               | EqnBlock Contents
               | CodeBlock Code
               | Definition [(String,LayoutObj)] Label
               | List ListType Items
               | Figure Label Caption Filepath
               | Module Contents
               
data ListType = Item | Enum | Simple | Desc

instance Show ListType where
  show Item = "itemize"
  show Enum = "enumerate"
  show Simple = error "Printing Simple list failed, see ASTTeX/PrintTeX"
