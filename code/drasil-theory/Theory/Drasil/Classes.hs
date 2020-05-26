-- | Defining all the classes which represent knowledge-about-theories
module Theory.Drasil.Classes (
  -- the classes
    HasInputs(..)
  , HasOutput(..)
  ) where

import Language.Drasil

import Control.Lens (Lens')

-- a class may have Inputs
class HasInputs c where
  inputs :: Lens' c [QuantityDict]
  inp_constraints :: Lens' c [Relation]

class HasOutput c where
  output :: Lens' c QuantityDict
  out_constraints :: Lens' c [RealInterval Expr Expr]
