{-# OPTIONS -Wall #-} 

module Format where

data Format = TeX | Plain | HTML

--------------------------

data Decoration = Hat | Vector deriving Eq
data Accent = Grave | Acute deriving Eq
