module Language.Drasil.Printing.Import.CodeExpr where

import Language.Drasil hiding (neg, sec, symbol, isIn)
import Language.Drasil.Display
import Language.Drasil.Development (UFuncB(..), UFuncVec(..)
  , ArithBinOp(..), BoolBinOp(..), EqBinOp(..), LABinOp(..)
  , OrdBinOp(..), VVNBinOp(..), VVVBinOp(..)
  , precA, precB, eprec, dePrec, dePrecAssoc, DisplayExpr(..)
  , DisplayBinOp(..), DisplayAssocBinOp(Equivalence))
import Database.Drasil
import Utils.Drasil

import Language.Drasil.Code.Expr

import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import qualified Language.Drasil.Printing.LayoutObj as T
import Language.Drasil.Printing.PrintingInformation (HasPrintingOptions(..),
  PrintingInformation, Notation(Scientific, Engineering), ckdb, stg)

import Language.Drasil.Printing.Import.Helpers
import Language.Drasil.Printing.Import.Space

import Control.Lens ((^.))
import Data.Bifunctor (bimap, second)
import Data.List (intersperse, partition)
import Numeric (floatToDigits)
import Data.Maybe (fromMaybe)

codeExpr :: CodeExpr -> PrintingInformation -> P.Expr
codeExpr = undefined -- TODO
