{-# OPTIONS -Wall #-} 

module Format where

data TeX = TeX
data Plain = Plain

class Format a where

instance Format TeX where
instance Format Plain where

--------------------------

data FormatC = Hat
            | Vector
            | Grave
            | Acute
  deriving (Eq, Ord)
  
