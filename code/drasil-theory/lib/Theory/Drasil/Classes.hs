-- | Defining all the classes which represent knowledge-about-theories.
module Theory.Drasil.Classes (
  -- the classes
    HasInputs(..)
  , HasOutput(..)
  ) where

import Language.Drasil

import Control.Lens (Lens')

-- | Members of this class may have inputs.
class HasInputs c where
  -- | Provides a 'Lens' that holds a 'QuantityDict' and maybe constraints.
  inputs :: Lens' c [(QuantityDict, Maybe (RealInterval Expr Expr))]

-- | Members of this class may have outputs.
class HasOutput c where
  -- | Provides a 'Lens' that holds a 'QuantityDict' for output.
  output :: c -> QuantityDict
  -- | Provides a 'Lens' that holds constraints on the output.
  out_constraints :: c -> [RealInterval Expr Expr]
