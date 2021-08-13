-- | Developing the expression language in Drasil. Re-export many things to simplify external use.
module Language.Drasil.Development (
  -- * NounPhrase
    NounPhrase(phraseNP, pluralNP)
  -- * Expr
  , Expr(..), UFunc(..), UFuncB(..), UFuncVV(..), UFuncVN(..)
  , ArithBinOp(..), BoolBinOp(..), EqBinOp(..), LABinOp(..), OrdBinOp(..), VVVBinOp(..), VVNBinOp(..)
  , AssocArithOper(..), AssocBoolOper(..)
  , DerivType(..), Completeness(..), Relation
  -- ** Expr Precedence
  -- Expr.Precedence
  , precA, precB, eprec
  -- ModelExpr.Precedence
  , mePrec
  -- * Extracting UID Functions
  -- Expr.Extract
  , eDep, eNames, eNames', eNamesRI
  -- ModelExpr.Extract
  , meDep
  -- Sentence.Extract
  , sdep, lnames, lnames'
  ) where

import Language.Drasil.NounPhrase (NounPhrase(phraseNP,pluralNP))
import Language.Drasil.Expr (Expr(..), Relation,
  Completeness(..), DerivType(..),
  AssocBoolOper(..), AssocArithOper(..),
  VVNBinOp(..), VVVBinOp(..), OrdBinOp(..), LABinOp(..),
  EqBinOp(..), BoolBinOp(..), ArithBinOp(..),
  UFuncVN(..), UFuncVV(..), UFuncB(..), UFunc(..))
import Language.Drasil.Expr.Extract (eDep, eNames, eNames', eNamesRI)
import Language.Drasil.Expr.Precedence (precA, precB, eprec)
import Language.Drasil.ModelExpr.Extract (meDep)
import Language.Drasil.ModelExpr.Precedence (mePrec)
import Language.Drasil.Sentence.Extract (sdep, lnames, lnames')
