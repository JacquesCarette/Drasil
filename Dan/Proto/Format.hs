{-# OPTIONS -Wall #-} 

module Format where

data Format = TeX | Plain | HTML

--------------------------

data FormatC = Hat
            | Vector
            | Grave
            | Acute
  deriving Eq
