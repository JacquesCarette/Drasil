-- | Defines operator precedence.
module Language.Drasil.ModelExpr.Precedence where

import Language.Drasil.ModelExpr.Lang
import Language.Drasil.Expr.Lang (ArithBinOp(..))
import Language.Drasil.Expr.Precedence (prec2Arith, prec2Eq,
  prec2LA, prec2Ord, prec2VVV, prec2VVN, prec2NVV, prec2ESS, prec2ESB,
  precA, precC, prec1, prec1B, prec1VV, prec1VN)

-- These precedences are inspired from Haskell/F#
-- as documented at http://kevincantu.org/code/operators.html
-- They are all multiplied by 10, to leave room to weave things in between

-- | precB - precedence for boolean-related Binary-Associative (Commutative) operators.
precB :: AssocBoolOper -> Int
precB And         = 120
precB Or          = 110
precB Equivalence = 100

prec2Spc :: SpaceBinOp -> Int
prec2Spc _ = 170

prec2Stat :: StatBinOp -> Int
prec2Stat _ = 130

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
mePrec (UnaryOpVV fn _)       = prec1VV fn
mePrec (UnaryOpVN fn _)       = prec1VN fn
mePrec (Operator o _ _)       = precA o
mePrec (ArithBinaryOp bo _ _) = prec2Arith bo
mePrec (EqBinaryOp bo _ _)    = prec2Eq bo
mePrec (LABinaryOp bo _ _)    = prec2LA bo
mePrec (SpaceBinaryOp bo _ _) = prec2Spc bo
mePrec (StatBinaryOp bo _ _)  = prec2Stat bo
mePrec (OrdBinaryOp bo _ _)   = prec2Ord bo
mePrec (VVVBinaryOp bo _ _)   = prec2VVV bo
mePrec (VVNBinaryOp bo _ _)   = prec2VVN bo
mePrec (NVVBinaryOp bo _ _)   = prec2NVV bo
mePrec (ESSBinaryOp bo _ _)   = prec2ESS bo
mePrec (ESBBinaryOp bo _ _)   = prec2ESB bo
mePrec RealI{}                = 170
mePrec ForAll{}               = 130
