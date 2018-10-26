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

class HasShortName  s where
  shortname :: Lens' s ShortName-- String; The text to be displayed for the link.
                            -- A short name used for referencing within a document that can 
                            -- include symbols and whatnot if required.
                            -- Visible in the typeset documents (pdf)

-- | A HasSymbol is anything which has a Symbol
class HasSymbol c where
  -- | Provides the Symbol --  for a particular stage of generation
  symbol  :: c -> Stage -> Symbol
  
class HasRefAddress b where
  getRefAdd :: Lens' b LblType 

