module Language.Drasil.Printing.AST where

import Language.Drasil.UnitLang (USymb)
import Language.Drasil.RefTypes (RefType, RefAdd)
import Language.Drasil.Unicode (Greek, Special)
import Language.Drasil.Chunk.ShortName

data Ops = IsIn | Integer | Real | Rational | Natural | Boolean | Comma | Prime | Log
  | Sin | Cos | Tan | Sec | Csc | Cot | Not | Dim | Exp | Neg | Cross
  | Dot | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff | Subt | And | Or
  | Add | Mul | Summ | Inte | Prod

data Fence = Paren | Curly | Norm | Abs
data OverSymb = Hat
data Fonts = Bold | Emph
data Spacing = Thin

data Expr = Dbl   Double
          | Int   Integer
          | Str   String
          | Case  [(Expr,Expr)]
          | Mtx [[Expr]]
          | Row   [Expr]
          | Ident String
          | Spec  Special
          | Gr    Greek
          | Sub   Expr
          | Sup   Expr
          | MO    Ops
          | Over  OverSymb Expr
          | Fenced Fence Fence Expr
          | Font  Fonts Expr
          | Div   Expr Expr -- actually, fractions are a layout thing!
          | Sqrt  Expr      -- as are roots. Just sqrt for now.
          | Spc   Spacing
          
infixr 5 :+:

data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Sy USymb
          | Sp Special
          | Ref RefType RefAdd Spec ShortName
          | EmptyS
          | Quote Spec    -- quotes are different in different languages
          | HARDNL        -- newline. Temp fix for multi-line descriptions; 
                          -- May move to a new LayoutObj, but only exists in TeX
                          -- so it's not really a big deal ATM.
type Title    = Spec

data ListType = Ordered [ItemType] 
              | Unordered [ItemType]
              | Simple      [(Title,ItemType)]
              | Desc        [(Title,ItemType)]
              | Definitions  [(Title,ItemType)]

data ItemType = Flat Spec
              | Nested Spec ListType
