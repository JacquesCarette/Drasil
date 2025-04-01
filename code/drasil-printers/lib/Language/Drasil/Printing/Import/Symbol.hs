-- | Defines functions for printing symbols and units.
module Language.Drasil.Printing.Import.Symbol (symbol, pUnit) where

import Language.Drasil (USymb(..))
import Language.Drasil.ShortHands (cDelta)
import Language.Drasil.Display (Decoration(..), Symbol(..))
import qualified Language.Drasil.Printing.AST as P

import Data.List (partition)
import Data.Bifunctor (second)

-- | Converts a symbol into an expression.
symbol :: Symbol -> P.Expr
symbol (Variable s) = P.Ident s
symbol (Label    s) = P.Label s
symbol (Integ    n) = P.Int (toInteger n)
symbol (Special  s) = P.Spec s
symbol (Concat  sl) = P.Row $ map symbol sl
symbol (Corners ul dl ur dr s)  =
  let
    -- For the appended (sub/sup)scripts, wrap them in curly braces only if
    -- there's is more than one symbol.
    mergeSymbols :: [Symbol] -> P.Expr
    mergeSymbols [s'] = symbol s'
    mergeSymbols ss   = P.Row $ map symbol ss 

    renderSubSup :: [Symbol] -> [Symbol] -> P.Expr -> [P.Expr]
    renderSubSup [] [] _ = []
    renderSubSup us [] x = [x, P.Sup $ mergeSymbols us]
    renderSubSup [] ds x = [x, P.Sub $ mergeSymbols ds]
    renderSubSup us ds x = [x, P.Sup $ mergeSymbols us, P.Sub $ mergeSymbols ds]

    rendered = renderSubSup ul dl (P.Label "") ++ renderSubSup ur dr (symbol s)
   in P.Row [P.Row rendered] -- Ensure the final rendered symbol is wrapped in curly braces by double-nesting in P.Row.
symbol (Atop f s)               = sFormat f s
symbol Empty                    = P.Row []

-- | Helper that adds decoration to symbols (for vectors, derivatives, etc.).
sFormat :: Decoration -> Symbol -> P.Expr
sFormat Hat        s = P.Over P.Hat $ symbol s
sFormat Vector     s = P.Font P.Bold $ symbol s
sFormat Prime      s = P.Row [symbol s, P.MO P.Prime]
sFormat Delta      s = P.Row [symbol cDelta, symbol s]
sFormat Magnitude  s = P.Fenced P.Norm P.Norm $ symbol s

-- | Renders a unit symbol as a printable expression.
pUnit :: USymb -> P.Expr
pUnit (US ls) = formatu t b
  where
    (t, b) = partition ((> 0) . snd) ls
    formatu :: [(Symbol, Integer)] -> [(Symbol, Integer)] -> P.Expr
    formatu [] l = line l
    formatu l [] = P.Row $ map powu l
    formatu nu de = P.Div (line nu) $ line $ map (second negate) de
    line :: [(Symbol, Integer)] -> P.Expr
    line []  = P.Row [] -- should not happen ?
    line [n] = powu n
    line l   = P.Row $ map powu l
    powu :: (Symbol, Integer) -> P.Expr
    powu (n, 1) = symbol n
    powu (n, p) = P.Row [symbol n, P.Sup $ P.Int p]
