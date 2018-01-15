module Language.Drasil.Printing.AST where

import Language.Drasil.Expr (Variable)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space)

data Oper = Add | Mul | And | Or
data BinOp = Frac | Div | Pow | Sub | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff | Index
  | Dot

data Expr = Var   Variable
          | Dbl   Double
          | Int   Integer
          | Assoc Oper [Expr]
          | BOp   BinOp Expr Expr
          | Sym   Symbol
          | Not   Expr
          | Neg   Expr
          | Call  Expr [Expr]
          | Case  [(Expr,Expr)]
          | Op Function [Expr]
          | Grouping Expr
          | IsIn  Expr Space
          | Forall Symbol Expr
          | Exists Symbol Expr
          | Mtx [[Expr]]
          
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

prec2 :: BinOp -> Int
prec2 Frac = 3
prec2 Div = 3
prec2 Pow = 2
prec2 Sub = 4
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
