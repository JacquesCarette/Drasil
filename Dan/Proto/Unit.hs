{-# OPTIONS -Wall #-} 
module Unit where

import ASTInternal (Expr(..))

data Unit = Fundamental --Fundamental unit type (e.g. "m" for length)
          | Derived Expr --Derived unit type (e.g. "J" for power, from
                                --the expression kg m^2 / s^2