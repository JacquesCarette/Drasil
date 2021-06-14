{- re-export many things to simplify external use -}
module Language.Drasil.Development (
  -- NounPhrase
    NounPhrase(phraseNP,pluralNP)
  -- DisplayExpr
  , DisplayExpr(..)
  -- Expr
  , UFuncB(..), UFuncVec(..)
  , ArithBinOp(..), BoolBinOp(..), EqBinOp(..)
  , LABinOp(..), OrdBinOp(..), VVVBinOp(..), VVNBinOp(..)
  -- Expr.Extract
  , eDep, eNames, eNames', eNamesRI
  , deDep
  -- Sentence.Extract
  , sdep, lnames, lnames'
  -- Expr.Precendence
  , precA, precB, eprec
  ) where

import Language.Drasil.NounPhrase (NounPhrase(phraseNP,pluralNP))
import Language.Drasil.DisplayExpr (DisplayExpr(..))
import Language.Drasil.Expr (UFuncB(..), UFuncVec(..)
  , ArithBinOp(..), BoolBinOp(..), EqBinOp(..)
  , LABinOp(..), OrdBinOp(..), VVVBinOp(..), VVNBinOp(..))
import Language.Drasil.Expr.Extract
import Language.Drasil.Expr.Precedence (precA, precB, eprec)
import Language.Drasil.Sentence.Extract (sdep, lnames, lnames')
