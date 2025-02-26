-- | Defines operator precedence.
module Language.Drasil.ModelExpr.Precedence where

import Language.Drasil.ModelExpr.Lang 

-- These precedences are inspired from Haskell/F# 
-- as documented at http://kevincantu.org/code/operators.html
-- They are all multiplied by 10, to leave room to weave things in between

-- | prec2Arith - precedence for arithmetic-related binary operations.
prec2Arith :: ArithBinOp -> Int
prec2Arith Frac = 190
prec2Arith Pow  = 200
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

prec2Spc :: SpaceBinOp -> Int
prec2Spc _ = 170

prec2Stat :: StatBinOp -> Int
prec2Stat _ = 130

-- | prec2CCC - precedence for Clif->Clif->Clif-related binary operations.
prec2CCC :: CCCBinOp -> Int
prec2CCC _ = 190

-- | prec2VVN - precedence for Clif->Clif->Num-related binary operations.
prec2CCN :: CCNBinOp -> Int
prec2CCN _ = 190

-- | prec2NCC - precedence for Num->Clif->Clif-related binary operations.
prec2NCC :: NCCBinOp -> Int
prec2NCC _ = 190

prec2ESS :: ESSBinOp -> Int
prec2ESS _ = 190

prec2ESB :: ESBBinOp -> Int
prec2ESB _ = 190

-- | precA - precedence for arithmetic-related Binary-Associative (Commutative) operators.
precA :: AssocArithOper -> Int
precA Mul  = 190
precA Add = 180

-- | precB - precedence for boolean-related Binary-Associative (Commutative) operators.
precB :: AssocBoolOper -> Int
precB And         = 120
precB Or          = 110
precB Equivalence = 100

precC :: AssocConcatOper -> Int
precC SUnion      = 180

-- | prec1 - precedence of unary operators.
prec1 :: UFunc -> Int
prec1 Neg = 230
prec1 Exp = 200
prec1 _   = 250

-- | prec1B - precedence of boolean-related unary operators.
prec1B :: UFuncB -> Int
prec1B Not = 230

-- | prec1VV - precedence of vector-vector-related unary operators.
prec1CC :: UFuncCC -> Int
prec1CC _ = 250

-- | prec1Vec - precedence of vector-number-related unary operators.
prec1CN :: UFuncCN -> Int
prec1CN _ = 230

-- | eprec - `ModelExpr` precedence.
mePrec :: ModelExpr -> Int
mePrec Lit{}                  = 500
mePrec Spc{}                  = 500
mePrec (AssocA op _)          = precA op
mePrec (AssocB op _)          = precB op
mePrec (AssocC op _)          = precC op
mePrec C{}                    = 500
mePrec Deriv{}                = prec2Arith Frac
mePrec FCall{}                = 210
mePrec Case{}                 = 200
mePrec Matrix{}               = 220
mePrec Set{}                  = 220
mePrec (Variable _ _)         = 220
mePrec (UnaryOp fn _)         = prec1 fn
mePrec (UnaryOpB fn _)        = prec1B fn
mePrec (UnaryOpCC fn _)       = prec1CC fn
mePrec (UnaryOpCN fn _)       = prec1CN fn
mePrec (Operator o _ _)       = precA o
mePrec (ArithBinaryOp bo _ _) = prec2Arith bo
mePrec (BoolBinaryOp bo _ _)  = prec2Bool bo
mePrec (EqBinaryOp bo _ _)    = prec2Eq bo
mePrec (LABinaryOp bo _ _)    = prec2LA bo
mePrec (SpaceBinaryOp bo _ _) = prec2Spc bo
mePrec (StatBinaryOp bo _ _)  = prec2Stat bo
mePrec (OrdBinaryOp bo _ _)   = prec2Ord bo
mePrec (CCCBinaryOp bo _ _)   = prec2CCC bo
mePrec (CCNBinaryOp bo _ _)   = prec2CCN bo
mePrec (NCCBinaryOp bo _ _)   = prec2NCC bo
mePrec (ESSBinaryOp bo _ _)   = prec2ESS bo
mePrec (ESBBinaryOp bo _ _)   = prec2ESB bo
mePrec RealI{}                = 170
mePrec ForAll{}               = 130
