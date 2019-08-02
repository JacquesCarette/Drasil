module Language.Drasil.Printing.AST where

import Language.Drasil hiding (ItemType, ListType, Expr)

data LinkType = Internal | Cite2 | External

data Ops = IsIn | Integer | Real | Rational | Natural | Boolean | Comma | Prime | Log 
  | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin | Arccos | Arctan | Not
  | Dim | Exp | Neg | Cross | Dot | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff
  | Subt | And | Or | Add | Mul | Summ | Inte | Prod | Point | Perc

data Fence = Paren | Curly | Norm | Abs
data OverSymb = Hat
data Fonts = Bold | Emph
data Spacing = Thin
type Label = Spec

data Expr = Dbl   Double
          | Int   Integer
          | Str   String
          | Case  [(Expr,Expr)]
          | Mtx   [[Expr]]
          | Row   [Expr]
          | Ident String
          | Label String
          | Spec  Special
          
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
          | Ref LinkType String Spec
          | EmptyS
          | Quote Spec    -- quotes are different in different languages
          | HARDNL        -- newline. Temp fix for multi-line descriptions; 
                          -- May move to a new LayoutObj, but only exists in TeX
                          -- so it's not really a big deal ATM.
type Title    = Spec

data ListType = Ordered [(ItemType,Maybe Label)]
              | Unordered [(ItemType,Maybe Label)]
              | Simple      [(Title,ItemType,Maybe Label)]
              | Desc        [(Title,ItemType,Maybe Label)]
              | Definitions  [(Title,ItemType,Maybe Label)]

data ItemType = Flat Spec
              | Nested Spec ListType
