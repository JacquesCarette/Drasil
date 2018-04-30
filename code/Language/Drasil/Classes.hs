-- | Defining all the classes which represent knowledge-about-knowledge
module Language.Drasil.Classes (HasUID(uid), UID) where

import Control.Lens (Lens')

type UID = String

-- | The most basic item: having a unique key, here a UID (as a String)
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use
  uid :: Lens' c UID
