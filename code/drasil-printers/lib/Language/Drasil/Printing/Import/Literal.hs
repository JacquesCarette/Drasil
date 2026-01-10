module Language.Drasil.Printing.Import.Literal (literal) where

import Language.Drasil hiding (neg, sec, symbol, isIn)
import Language.Drasil.Literal.Development (Literal(..))

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (HasPrintingOptions(..),
  PrintingInformation, Notation(Scientific, Engineering))

import Control.Lens ((^.))
import Numeric (floatToDigits)

import Language.Drasil.Printing.Import.Helpers
    (digitsProcess, processExpo)

literal :: Literal -> PrintingInformation -> P.Expr
literal (Dbl d)                  sm = case sm ^. getSetting of
  Engineering ->
     let (f, s) = processExpo $ snd $ floatToDigits 10 d in
     P.Row $ digitsProcess (map toInteger $ fst $ floatToDigits 10 d)
     f 0 (toInteger s)
  Scientific  ->  P.Dbl d
literal (Int i)                   _ = P.Int i
literal (ExactDbl d)              _ = P.Int d
literal (Str s)                   _ = P.Str s
literal (Perc a b)               sm = P.Row [literal (dbl val) sm, P.MO P.Perc]
  where
    val = fromIntegral a / (10 ** fromIntegral (b - 2))

