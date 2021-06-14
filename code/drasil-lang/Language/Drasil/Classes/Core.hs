{-# Language TypeFamilies #-}
-- | Defining the core classes which represent knowledge-about-knowledge
module Language.Drasil.Classes.Core (
    HasUID(uid)
  , HasShortName(shortname)
  , HasRefAddress(getRefAdd)
  , HasSymbol(symbol)
  , Referable(refAdd, renderRef)
  ) where

import Language.Drasil.ShortName (ShortName)
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UID (UID)
import Language.Drasil.Label.Type (LblType)

import Control.Lens (Lens')

-- | The most basic item: having a unique identifier key, here a UID.
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use.
  uid :: Lens' c UID

-- | A 'ShortName' is the text to be displayed for a link.
-- Used for referencing within a document that can include symbols and whatnot if required.
-- Visible in the typeset documents (pdf).
class HasShortName  s where
  shortname :: s -> ShortName

-- | A HasSymbol is anything which has a 'Symbol'.
class HasSymbol c where
  -- | Provides the 'Symbol' for a particular stage of generation.
  symbol  :: c -> Stage -> Symbol
 
-- | Members must have a reference address.
class HasRefAddress b where
  -- | Provides the ability to hold a reference address.
  getRefAdd :: b -> String

-- | Members of this class have the ability to be referenced.
class HasUID s => Referable s where
  -- | The referencing address (what we're linking to).
  -- Only visible in the source (tex/html).
  refAdd    :: s -> String 
  -- | Alternate form of reference.
  renderRef :: s -> LblType 
