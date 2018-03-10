module Language.Drasil.Printing.AST where

import Language.Drasil.Expr (Oper(..),UFunc,BinOp(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space)
import Language.Drasil.Spec (USymb, RefType)
import Language.Drasil.Unicode (Greek, Special)

data Expr = Dbl   Double
          | Int   Integer
          | Str   String
          | Assoc Oper [Expr]
          | BOp   BinOp Expr Expr
          | Sym   Symbol
          | Call  Expr [Expr]
          | Case  [(Expr,Expr)]
          | UOp   UFunc Expr
          | Grouping Expr
          | Funct Functional Expr
          | IsIn  Expr Space
          | Mtx [[Expr]]
          
data Functional = 
            Summation (Maybe ((Symbol, Expr),Expr))
          | Integral ((Maybe Expr),(Maybe Expr)) Symbol
          | Product (Maybe ((Symbol, Expr), Expr))

prec :: Oper -> Int
prec Mul = 3
prec Add = 4
prec And = 11
prec Or = 12

prec2 :: BinOp -> Int
prec2 Frac = 3
prec2 Div = 3
prec2 Pow = 2
prec2 Subt = 4
prec2 Eq = 9
prec2 NEq  = 9
prec2 Lt  = 9
prec2 Gt  = 9
prec2 LEq  = 9
prec2 GEq  = 9
prec2 Impl = 13
prec2 Iff = 13
prec2 Index = 1
prec2 Dot = 3
prec2 Cross = 3

infixr 5 :+:

data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Spec :^: Spec -- superscript
          | Spec :-: Spec -- subscript
          | Spec :/: Spec -- frac
          | Sy USymb
          | N Symbol
          | G Greek
          | Sp Special
          | Ref RefType Spec
          | EmptyS
          | HARDNL        -- newline. Temp fix for multi-line descriptions; 
                          -- May move to a new LayoutObj, but only exists in TeX
                          -- so it's not really a big deal ATM.

