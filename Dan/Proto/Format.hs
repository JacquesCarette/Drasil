{-# OPTIONS -Wall #-} 

module Format where
import Prelude hiding (id)

data TeX = TeX
data Plain = Plain

class Format a where
  format :: a

instance Format TeX where
  format = TeX
instance Format Plain where
  format = Plain