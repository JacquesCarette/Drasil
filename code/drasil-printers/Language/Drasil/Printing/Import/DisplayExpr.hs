module Language.Drasil.Printing.Import.DisplayExpr where

import Language.Drasil hiding (neg, sec, symbol, isIn)
import Language.Drasil.Display
import Language.Drasil.Development (UFuncB(..), UFuncVec(..)
  , ArithBinOp(..), BoolBinOp(..), EqBinOp(..), LABinOp(..)
  , OrdBinOp(..), VVNBinOp(..), VVVBinOp(..)
  , precA, precB, eprec, dePrec, dePrecAssoc, DisplayExpr(..)
  , DisplayBinOp(..), DisplayAssocBinOp(Equivalence))
import Database.Drasil
import Utils.Drasil

import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import qualified Language.Drasil.Printing.LayoutObj as T
import Language.Drasil.Printing.PrintingInformation (HasPrintingOptions(..),
  PrintingInformation, Notation(Scientific, Engineering), ckdb, stg)

import Language.Drasil.Printing.Import.Expr
import Language.Drasil.Printing.Import.Helpers
import Language.Drasil.Printing.Import.Space

import Control.Lens ((^.))
import Data.Bifunctor (bimap, second)
import Data.List (intersperse, partition)
import Numeric (floatToDigits)
import Data.Maybe (fromMaybe)


-- | Helper that adds parenthesis to a display expression where appropriate.
dispExpr' :: PrintingInformation -> Int -> DisplayExpr -> P.Expr
dispExpr' s p e = fence $ dispExpr e s
  where fence = if dePrec e < p then parens else id

-- | Convert 'DisplayBinOps' into the operators of the AST language.
deBinOp :: DisplayBinOp -> P.Ops
deBinOp IsIn    = P.IsIn
deBinOp Defines = P.Eq

-- | Convert 'DisplayAssocBinOp's into the operators of the AST language.
deAssocBinOp :: DisplayAssocBinOp -> P.Ops
deAssocBinOp Equivalence = P.Eq 
deAssocBinOp _           = P.And

-- | Translate DisplayExprs to printable layout AST.
dispExpr :: DisplayExpr -> PrintingInformation -> P.Expr
dispExpr (AlgebraicExpr e)  sm = expr e sm
dispExpr (SpaceExpr s)      sm = space sm s
dispExpr (BinOp b l r)      sm = P.Row [dispExpr l sm, P.MO $ deBinOp b, dispExpr r sm]
dispExpr (AssocBinOp b des) sm = P.Row $ intersperse (P.MO op) $ map (dispExpr' sm prec) des
  where prec = dePrecAssoc b
        op   = deAssocBinOp b
