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
          | Op Function [Expr]
          | Grouping Expr
          
data Function = Log
           | Summation (Maybe Expr,Maybe Expr) --Sum (low,high) Bounds
           | Abs
           | Integral (Maybe Expr, Maybe Expr) --Integral (low,high) Bounds
           | Sin
           | Cos
           | Tan
           | Sec
           | Csc
           | Cot
           
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
type Items    = [LayoutObj]
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
               | List ListType
               | Figure Label Caption Filepath
               
data ListType = Item [ItemType] | Enum [ItemType] | Simple [(Spec,ItemType)]

data ItemType = Flat Spec
              | Nested Spec ListType

instance Show ListType where
  show (Item _)   = "itemize"
  show (Enum _)   = "enumerate"
  show (Simple _) = error "Printing Simple list failed, see ASTTeX/PrintTeX"
  
instance Show Function where
  show Log = "\\log"
  show (Summation _) = "\\sum"
  show Abs = ""
  show (Integral _) = "\\int"
  show Sin = "\\sin"
  show Cos = "\\cos"
  show Tan = "\\tan"
  show Sec = "\\sec"
  show Csc = "\\csc"
  show Cot = "\\cot"