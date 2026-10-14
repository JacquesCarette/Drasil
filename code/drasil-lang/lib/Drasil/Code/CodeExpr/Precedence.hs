module Drasil.Code.CodeExpr.Precedence (precA, precB, eprec) where

import Drasil.Code.CodeExpr.Lang (CodeExpr(..))
import Language.Drasil.Expr.Precedence (prec2Arith, prec2Eq,
  prec2LA, prec2Ord, prec2VVV, prec2VVN, prec2NVV, prec2ESS, prec2ESB,
  precA, precB, precC, prec1, prec1B, prec1VV, prec1VN)

-- These precedences are inspired from Haskell/F#
-- as documented at http://kevincantu.org/code/operators.html
-- They are all multiplied by 10, to leave room to weave things in between

-- | eprec - "Expression" precedence.
eprec :: CodeExpr -> Int
eprec Lit{}                  = 500
eprec (AssocA op _)          = precA op
eprec (AssocB op _)          = precB op
eprec (AssocC op _)          = precC op
eprec C{}                    = 500
eprec FCall{}                = 210
eprec New{}                  = 210
eprec Message{}              = 210
eprec Field{}                = 210
eprec Case{}                 = 200
eprec Matrix{}               = 220
eprec Set{}                  = 220
eprec (Variable _ _)         = 220
eprec (UnaryOp fn _)         = prec1 fn
eprec (UnaryOpB fn _)        = prec1B fn
eprec (UnaryOpVV fn _)       = prec1VV fn
eprec (UnaryOpVN fn _)       = prec1VN fn
eprec (Operator o _ _)       = precA o
eprec (ArithBinaryOp bo _ _) = prec2Arith bo
eprec (EqBinaryOp bo _ _)    = prec2Eq bo
eprec (LABinaryOp bo _ _)    = prec2LA bo
eprec (OrdBinaryOp bo _ _)   = prec2Ord bo
eprec (VVVBinaryOp bo _ _)   = prec2VVV bo
eprec (VVNBinaryOp bo _ _)   = prec2VVN bo
eprec (NVVBinaryOp bo _ _)   = prec2NVV bo
eprec (ESSBinaryOp bo _ _)   = prec2ESS bo
eprec (ESBBinaryOp bo _ _)   = prec2ESB bo
eprec RealI{}                = 170
