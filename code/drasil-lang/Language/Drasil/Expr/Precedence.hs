module Language.Drasil.Expr.Precedence where

import Language.Drasil.Expr (Expr(..), 
  BinOp(..), ArithBinOp(..), EqBinOp(..), BoolBinOp, OrdBinOp,
  UFunc(..), UFuncB(..), UFuncVec(..), 
  AssocBoolOper(..), AssocArithOper(..))

-- These precedences are inspired from Haskell/F# 
-- as documented at http://kevincantu.org/code/operators.html
-- They are all multiplied by 10, to leave room to weave things in between

-- | prec2 - precendence for binary operators
prec2 :: BinOp -> Int
prec2 Index = 250
prec2 Dot = 190
prec2 Cross = 190

-- | prec2Arith - precedence for arithmetic-related binary operations
prec2Arith :: ArithBinOp -> Int
prec2Arith Frac = 190
prec2Arith Pow = 200
prec2Arith Subt = 180

-- | prec2Bool - precedence for boolean-related binary operations
prec2Bool :: BoolBinOp -> Int
prec2Bool _ = 130

-- | prec2Eq - precedence for equality-related binary operations
prec2Eq :: EqBinOp -> Int
prec2Eq _  = 130

-- | prec2Ord - precedence for order-related binary operations
prec2Ord :: OrdBinOp -> Int
prec2Ord _  = 130

-- | prec - precedence for Binary-Associative (Commutative) operators
precA :: AssocArithOper -> Int
precA Mul = 190
precA Add = 180

precB :: AssocBoolOper -> Int
precB And = 120
precB Or = 110


-- | prec1 - precedence of unary operators
prec1 :: UFunc -> Int
prec1 Neg = 230
prec1 Exp = 200
prec1 _ = 250

-- | prec1B - precedence of boolean-related unary operators
prec1B :: UFuncB -> Int
prec1B Not = 230

-- | prec1Vec - precedence of vector-related unary operators
prec1Vec :: UFuncVec -> Int
prec1Vec _ = 250

-- | eprec - "Expression" precedence
eprec :: Expr -> Int
eprec Dbl{}                  = 500
eprec Int{}                  = 500
eprec Str{}                  = 500
eprec Perc{}                 = 500
eprec (AssocA op _)          = precA op
eprec (AssocB op _)          = precB op
eprec C{}                    = 500
eprec Deriv{}                = prec2Arith Frac
eprec FCall{}                = 210
eprec New{}                  = 210
eprec Message{}              = 210
eprec Field{}                = 210
eprec Case{}                 = 200
eprec Matrix{}               = 220
eprec (UnaryOp fn _)         = prec1 fn
eprec (UnaryOpB fn _)        = prec1B fn
eprec (UnaryOpVec fn _)      = prec1Vec fn
eprec (Operator o _ _)       = precA o
eprec (BinaryOp bo _ _)      = prec2 bo
eprec (ArithBinaryOp bo _ _) = prec2Arith bo
eprec (BoolBinaryOp bo _ _)  = prec2Bool bo
eprec (EqBinaryOp bo _ _)    = prec2Eq bo
eprec (OrdBinaryOp bo _ _)   = prec2Ord bo
eprec IsIn{}                 = 170
eprec RealI{}                = 170
