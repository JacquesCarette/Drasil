module Language.Drasil.CodeExpr.Precedence (precA, precB, eprec) where

import Language.Drasil.CodeExpr.Lang (CodeExpr(..), UFuncVV, UFuncVN, UFuncB(..),
    UFunc(..), AssocBoolOper(..), AssocArithOper(..), VVNBinOp, NVVBinOp,
    VVVBinOp, OrdBinOp, LABinOp, BoolBinOp, EqBinOp, ArithBinOp(..))

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

-- | prec2VVV - precedence for Vec->Vec->Vec-related binary operations.
prec2VVV :: VVVBinOp -> Int
prec2VVV _ = 190

-- | prec2VVN - precedence for Vec->Vec->Num-related binary operations.
prec2VVN :: VVNBinOp -> Int
prec2VVN _ = 190

-- | prec2NVV - precedence for Num->Vec->Vec-related binary operations.
prec2NVV :: NVVBinOp -> Int
prec2NVV _ = 190

-- | precA - precedence for arithmetic-related Binary-Associative (Commutative) operators.
precA :: AssocArithOper -> Int
precA Mul = 190
precA Add = 180

-- | precB - precedence for boolean-related Binary-Associative (Commutative) operators.
precB :: AssocBoolOper -> Int
precB And = 120
precB Or = 110

-- | prec1 - precedence of unary operators.
prec1 :: UFunc -> Int
prec1 Neg = 230
prec1 Exp = 200
prec1 _ = 250

-- | prec1B - precedence of boolean-related unary operators.
prec1B :: UFuncB -> Int
prec1B Not = 230

-- | prec1VV - precedence of vector-vector-related unary operators.
prec1VV :: UFuncVV -> Int
prec1VV _ = 250

-- | prec1VN - precedence of vector-number-related unary operators.
prec1VN :: UFuncVN -> Int
prec1VN _ = 230

-- | eprec - "Expression" precedence.
eprec :: CodeExpr -> Int
eprec Lit{}                  = 500
eprec (AssocA op _)          = precA op
eprec (AssocB op _)          = precB op
eprec C{}                    = 500
eprec FCall{}                = 210
eprec New{}                  = 210
eprec Message{}              = 210
eprec Field{}                = 210
eprec Case{}                 = 200
eprec Matrix{}               = 220
eprec (UnaryOp fn _)         = prec1 fn
eprec (UnaryOpB fn _)        = prec1B fn
eprec (UnaryOpVV fn _)       = prec1VV fn
eprec (UnaryOpVN fn _)       = prec1VN fn
eprec (Operator o _ _)       = precA o
eprec (ArithBinaryOp bo _ _) = prec2Arith bo
eprec (BoolBinaryOp bo _ _)  = prec2Bool bo
eprec (EqBinaryOp bo _ _)    = prec2Eq bo
eprec (LABinaryOp bo _ _)    = prec2LA bo
eprec (OrdBinaryOp bo _ _)   = prec2Ord bo
eprec (VVVBinaryOp bo _ _)   = prec2VVV bo
eprec (VVNBinaryOp bo _ _)   = prec2VVN bo
eprec (NVVBinaryOp bo _ _)   = prec2NVV bo
eprec RealI{}                = 170
