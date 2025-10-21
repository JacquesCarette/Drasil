-- | Defines operator precedence.
module Language.Drasil.Expr.Precedence where

import Language.Drasil.Expr.Lang (Expr(..),
  ArithBinOp(..), BoolBinOp, EqBinOp(..), LABinOp, OrdBinOp, CCNBinOp,
  UFunc(..), UFuncB(..),
  AssocBoolOper(..), AssocArithOper(..), CCCBinOp, NCCBinOp, ESSBinOp, ESBBinOp, AssocConcatOper(..))

-- These precedences are inspired from Haskell/F# 
-- as documented at http://kevincantu.org/code/operators.html
-- They are all multiplied by 10, to leave room to weave things in between

-- | prec2Arith - precedence for arithmetic-related binary operations.
prec2Arith :: ArithBinOp -> Int
prec2Arith Frac = 190
prec2Arith Pow = 200
prec2Arith Subt = 180

-- | prec2Bool - precedence for boolean-related binary operations.
prec2Bool :: BoolBinOp -> Int
prec2Bool _ = 130

-- | prec2Eq - precedence for equality-related binary operations.
prec2Eq :: EqBinOp -> Int
prec2Eq _  = 130

-- | prec2LA - precedence for access-related binary operations.
prec2LA :: LABinOp -> Int
prec2LA _ = 250

-- | prec2Ord - precedence for order-related binary operations.
prec2Ord :: OrdBinOp -> Int
prec2Ord _  = 130

-- | prec2CCC - precedence for Vec->Vec->Vec-related binary operations.
prec2CCC :: CCCBinOp -> Int
prec2CCC _ = 190

-- | prec2CCN - precedence for Vec->Vec->Num-related binary operations.
prec2CCN :: CCNBinOp -> Int
prec2CCN _ = 190

-- | prec2NCC - precedence for Num->Vec->Vec-related binary operations.
prec2NCC :: NCCBinOp -> Int
prec2NCC _ = 190

-- | prec2ESS - precedence for Element->Set->Set-related binary operations.
prec2ESS :: ESSBinOp -> Int
prec2ESS _  = 130

-- | prec2ESS - precedence for Element->Set->Bool-related binary operations.
prec2ESB :: ESBBinOp -> Int
prec2ESB _  = 130

-- | precA - precedence for arithmetic-related Binary-Associative (Commutative) operators.
precA :: AssocArithOper -> Int
precA Mul = 190
precA Add = 180

-- | precB - precedence for boolean-related Binary-Associative (Commutative) operators.
precB :: AssocBoolOper -> Int
precB And = 120
precB Or = 110

precC :: AssocConcatOper -> Int
precC SUnion = 120


-- | prec1 - precedence of unary operators.
prec1 :: UFunc -> Int
prec1 Neg = 230
prec1 Exp = 200
prec1 _ = 250

-- | prec1B - precedence of boolean-related unary operators.
prec1B :: UFuncB -> Int
prec1B Not = 230

-- | prec1CC - precedence of vector-vector-related unary operators.
prec1CC :: uFuncCC -> Int
prec1CC _ = 250

-- | prec1Vec - precedence of vector-number-related unary operators.
prec1CN :: uFuncCN -> Int
prec1CN _ = 230

-- | eprec - "Expression" precedence.
eprec :: Expr -> Int
eprec Lit{}                  = 500
eprec (AssocA op _)          = precA op
eprec (AssocB op _)          = precB op
eprec (AssocC op _)          = precC op
eprec C{}                    = 500
eprec FCall{}                = 210
eprec Case{}                 = 200
eprec Matrix{}               = 220
eprec Set{}                  = 220
eprec (Variable _ _)         = 220
eprec (UnaryOp fn _)         = prec1 fn
eprec (UnaryOpB fn _)        = prec1B fn
eprec (UnaryOpCC fn _)       = prec1CC fn
eprec (UnaryOpCN fn _)       = prec1CN fn
eprec (Operator o _ _)       = precA o
eprec (ArithBinaryOp bo _ _) = prec2Arith bo
eprec (BoolBinaryOp bo _ _)  = prec2Bool bo
eprec (EqBinaryOp bo _ _)    = prec2Eq bo
eprec (LABinaryOp bo _ _)    = prec2LA bo
eprec (OrdBinaryOp bo _ _)   = prec2Ord bo
eprec (CCCBinaryOp bo _ _)   = prec2CCC bo
eprec (CCNBinaryOp bo _ _)   = prec2CCN bo
eprec (NCCBinaryOp bo _ _)   = prec2NCC bo
eprec (ESSBinaryOp bo _ _)   = prec2ESS bo
eprec (ESBBinaryOp bo _ _)   = prec2ESB bo
eprec (NatCCBinaryOp {})    = 250  -- Reasonable precedence for natural clif operations
eprec (Clif _ _)            = 500  -- High precedence like literals
eprec RealI{}                = 170