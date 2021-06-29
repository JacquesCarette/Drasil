-- | Defining classes that represent knowledge about Citations
module Language.Drasil.Classes.Citations (
    HasFields(getFields)
  ) where

import Language.Drasil.Data.Citation (CiteField)

import Control.Lens (Lens')

-- | 'Citation's should have a fields ('CiteField').
class HasFields c where
  -- | Provides a 'Lens' to 'CiteField's.
  getFields :: Lens' c [CiteField]

