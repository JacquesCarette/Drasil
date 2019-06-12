module Language.Drasil.Expr.Precedence where

import Language.Drasil.Expr (BinOp(..), BoolOper(..), ArithOper(..),
  UFunc(..), Expr(..))

-- These precedences are inspired from Haskell/F# 
-- as documented at http://kevincantu.org/code/operators.html
-- They are all multiplied by 10, to leave room to weave things in between

-- | prec2 - precendence for binary operators
prec2 :: BinOp -> Int
prec2 Frac = 190
prec2 Pow = 200
prec2 Subt = 180
prec2 Eq = 130
prec2 NEq  = 130
prec2 Lt  = 130
prec2 Gt  = 130
prec2 LEq  = 130
prec2 GEq  = 130
prec2 Impl = 130
prec2 Iff = 130
prec2 Index = 250
prec2 Dot = 190
prec2 Cross = 190

-- | prec - precedence for Binary-Associative (Commutative) operators
precA :: ArithOper -> Int
precA Mul = 190
precA Add = 180

precB :: BoolOper -> Int
precB And = 120
precB Or = 110


-- | prec1 - precedence of unary operators
prec1 :: UFunc -> Int
prec1 Neg = 230
prec1 Exp = 200
prec1 Not = 230
prec1 _ = 250

-- | eprec - "Expression" precedence
eprec :: Expr -> Int
eprec Dbl{}             = 500
eprec Int{}             = 500
eprec Str{}             = 500
eprec Perc{}            = 500
eprec (AssocA op _)     = precA op
eprec (AssocB op _)     = precB op
eprec C{}               = 500
eprec Deriv{}           = prec2 Frac
eprec FCall{}           = 210
eprec Case{}            = 200
eprec Matrix{}          = 220
eprec (UnaryOp fn _)    = prec1 fn
eprec (Operator o _ _)  = precA o
eprec (BinaryOp bo _ _) = prec2 bo
eprec IsIn{}            = 170
eprec RealI{}           = 170
