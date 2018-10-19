{-# Language TypeFamilies #-}
-- | Defining the core classes which represent knowledge-about-knowledge
module Language.Drasil.Classes.Core (
    HasUID(uid), UID
  , HasShortName(shortname)
  , HasRefAddress(getRefAdd)
  ) where

import Language.Drasil.Label.Type (LblType)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

import Control.Lens (Lens')

-- | The most basic item: having a unique key, here a UID
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use
  uid :: Lens' c UID

class HasShortName  s where
  shortname :: Lens' s ShortName-- String; The text to be displayed for the link.
                            -- A short name used for referencing within a document that can 
                            -- include symbols and whatnot if required.
                            -- Visible in the typeset documents (pdf)

class HasRefAddress b where
  getRefAdd :: Lens' b LblType 

