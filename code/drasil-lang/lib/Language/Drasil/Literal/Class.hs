module Language.Drasil.Literal.Class where

import Language.Drasil.Literal.Lang (Literal(..))

class LiteralC r where
    int      :: Integer -> r
    str      :: String -> r
    dbl      :: Double -> r
    exactDbl :: Integer -> r
    perc     :: Integer -> Integer -> r

instance LiteralC Literal where
    int = Int
    str = Str
    dbl = Dbl
    exactDbl = ExactDbl
    perc = Perc
