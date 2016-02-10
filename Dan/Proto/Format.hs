{-# OPTIONS -Wall #-} 

module Format where

data Format = TeX | Plain

--------------------------

data FormatC = Hat
            | Vector
            | Grave
            | Acute
  deriving Eq
