module Language.Drasil.Printing.AST where

import Language.Drasil.Expr (Variable)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space)

data Oper = Add | Mul | And | Or

data Expr = Var   Variable
          | Dbl   Double
          | Int   Integer
          | Bln   Bool
          | Assoc Oper [Expr]
          | Frac  Expr Expr
          | Div   Expr Expr
          | Pow   Expr Expr
          | Sub   Expr Expr
          | Sym   Symbol
          | Eq    Expr Expr
          | NEq   Expr Expr
          | Lt    Expr Expr
          | Gt    Expr Expr
          | LEq   Expr Expr
          | GEq   Expr Expr
          | Dot   Expr Expr
          | Not   Expr
          | Neg   Expr
          | Call  Expr [Expr]
          | Case  [(Expr,Expr)]
          | Op Function [Expr]
          | Grouping Expr
          | IsIn  Expr Space
          | Forall Symbol Expr
          | Exists Symbol Expr
          | Impl Expr Expr
          | Iff  Expr Expr
          | Mtx [[Expr]]
          | Index Expr Expr
          
data Function = Log
           | Summation (Maybe ((Symbol, Expr),Expr))
           | Abs
           | Norm
           | Integral ((Maybe Expr),(Maybe Expr)) Symbol
           | Sin
           | Cos
           | Tan
           | Sec
           | Csc
           | Cot
           | Cross
           | Product (Maybe ((Symbol, Expr), Expr))
           | Exp
           | Sqrt

prec :: Oper -> Int
prec Mul = 3
prec Add = 4
prec And = 11
prec Or = 12
