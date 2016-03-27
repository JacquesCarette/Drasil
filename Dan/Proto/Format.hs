{-# OPTIONS -Wall #-} 

module Format where

data Format = TeX | Plain | HTML

--------------------------

data Accent = Hat
            | Vector
            | Grave
            | Acute
  deriving Eq
