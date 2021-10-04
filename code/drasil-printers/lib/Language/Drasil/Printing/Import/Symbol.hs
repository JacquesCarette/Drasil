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
--symbol (Greek g)    = P.Gr g
symbol (Concat  sl) = P.Row $ map symbol sl
--
-- Handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = P.Row [P.Row [symbol s, P.Sup $ symbol x]]
symbol (Corners [] [] [] [x] s) = P.Row [P.Row [symbol s, P.Sub $ symbol x]]
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol Corners{}                = error "rendering of Corners (general)"
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
