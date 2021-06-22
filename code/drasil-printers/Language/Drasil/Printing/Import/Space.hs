module Language.Drasil.Printing.Import.Space where

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

import Control.Lens ((^.))
import Data.Bifunctor (bimap, second)
import Data.List (intersperse, partition)
import Numeric (floatToDigits)
import Data.Maybe (fromMaybe)

-- | Render a 'Space'.
space :: PrintingInformation -> Space -> P.Expr
space _ Integer = P.MO P.Integer
space _ Rational = P.MO P.Rational
space _ Real = P.MO P.Real
space _ Natural = P.MO P.Natural
space _ Boolean = P.MO P.Boolean
space _ Char = P.Ident "Char"
space _ String = P.Ident "String"
space _ Radians = error "Radians not translated"
space _ (Vect _) = error "Vector space not translated"
space _ (Array _) = error "Array space not translated"
space _ (Actor s) = P.Ident s
space sm (DiscreteD l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map (flip expr sm . dbl) l -- [Double]
space _ (DiscreteS l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map P.Str l --ex. let Meal = {"breakfast", "lunch", "dinner"}
space _ Void = error "Void not translated"
