module Language.Drasil.Printing.Import.Symbol where

import Language.Drasil.Display (Decoration(..), Symbol(..))
import qualified Language.Drasil.Printing.AST as P


-- | Helper tha converts a symbol into an expression.
symbol :: Symbol -> P.Expr
symbol (Variable s) = P.Ident s
symbol (Label    s) = P.Label s
symbol (Integ    n) = P.Int (toInteger n)
symbol (Special  s) = P.Spec s
--symbol (Greek g)    = P.Gr g
symbol (Concat  sl) = P.Row $ map symbol sl
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = P.Row [P.Row [symbol s, P.Sup $ symbol x]]
symbol (Corners [] [] [] [x] s) = P.Row [P.Row [symbol s, P.Sub $ symbol x]]
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol Corners{}                = error "rendering of Corners (general)"
symbol (Atop f s)               = sFormat f s
symbol Empty                    = P.Row []

-- | Helper that adds decoration to symbols (for vectors, derivatives, etc.).
sFormat :: Decoration -> Symbol -> P.Expr
sFormat Hat    s = P.Over P.Hat $ symbol s
sFormat Vector s = P.Font P.Bold $ symbol s
sFormat Prime  s = P.Row [symbol s, P.MO P.Prime]
