-- | Re-exporting modules
module Drasil.Code.CodeExpr.Development (
    -- CodeExpr
    CodeExpr(..),
    UFunc(..), UFuncB(..), UFuncVV(..), UFuncVN(..),
    -- re-export from Expr
    ArithBinOp(..), EqBinOp(..), BoolBinOp(..), LABinOp(..),
    OrdBinOp(..), VVVBinOp(..), VVNBinOp(..), NVVBinOp(..),
    ESSBinOp(..), ESBBinOp(..),
    AssocArithOper(..), AssocBoolOper(..), AssocConcatOper(..),
    -- Class
    CodeExprC(..),
    -- Extract
    eDep, eDep',
    eNamesRI, eNamesRI',
    -- Precedence
    eprec, precA, precB,
    -- Render
    expr, realInterval, constraint, CanGenCode(..)
) where

import Drasil.Code.CodeExpr.Lang (CodeExpr(..),
    UFuncVV(..), UFuncVN(..), UFuncB(..), UFunc(..))
import Language.Drasil.Expr.Lang (ArithBinOp(..), EqBinOp(..),
    BoolBinOp(..), LABinOp(..), OrdBinOp(..), VVVBinOp(..),
    VVNBinOp(..), NVVBinOp(..),
    ESSBinOp(..), ESBBinOp(..),
    AssocBoolOper(..), AssocArithOper(..), AssocConcatOper(..))

import Drasil.Code.CodeExpr.Class (CodeExprC(..))
import Drasil.Code.CodeExpr.Extract (eDep, eDep', eNamesRI, eNamesRI')
import Drasil.Code.CodeExpr.Precedence (eprec, precA, precB)
import Drasil.Code.CodeExpr.Convert (expr, realInterval, constraint, CanGenCode(..))
