{-# Language TypeFamilies #-}
-- | Defining the core classes which represent knowledge-about-knowledge
module Language.Drasil.Classes.Core (
    HasUID(uid), UID
  , HasShortName(shortname)
  , HasRefAddress(getRefAdd)
  , HasSymbol(symbol)
  ) where

import Language.Drasil.Label.Type (LblType)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UID (UID)

import Control.Lens (Lens')

-- | The most basic item: having a unique key, here a UID
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use
  uid :: Lens' c UID

-- A ShortName is the text to be displayed for a link.
-- Used for referencing within a document that can include symbols and whatnot if required.
-- Visible in the typeset documents (pdf)
class HasShortName  s where
  shortname :: s -> ShortName

-- | A HasSymbol is anything which has a Symbol
class HasSymbol c where
  -- | Provides the Symbol --  for a particular stage of generation
  symbol  :: c -> Stage -> Symbol
 
-- | For a "Reference Address", we just need a getter.
class HasRefAddress b where
  getRefAdd :: b -> LblType 

