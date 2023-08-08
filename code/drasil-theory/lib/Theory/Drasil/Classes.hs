-- | Defining all the classes which represent knowledge-about-theories.
module Theory.Drasil.Classes (
  -- the classes
    HasInputs(..)
  , HasOutput(..)
  ) where

import Language.Drasil

import Control.Lens (Lens', Getter)

-- | Members of this class may have inputs.
class HasInputs c where
  -- | Provides a 'Lens' that holds a 'QuantityDict' and maybe constraints.
  inputs :: Lens' c [(QuantityDict, Maybe (RealInterval Expr Expr))]

-- | Members of this class may have outputs.
class HasOutput c where
  -- | Provides a 'Getter' that holds a 'QuantityDict' for output.
  output :: Getter c QuantityDict
  -- | Provides a 'Getter' that holds constraints on the output.
  out_constraints :: Getter c [RealInterval Expr Expr]
