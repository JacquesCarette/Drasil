-- | Defining all the classes which represent knowledge-about-knowledge
module Language.Drasil.Classes (
    HasUID(uid), UID
  , NamedIdea(term)
  ) where

import Language.Drasil.NounPhrase.Core (NP)

import Control.Lens (Lens')

type UID = String

-- | The most basic item: having a unique key, here a UID (as a String)
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use
  uid :: Lens' c UID

-- | A NamedIdea is a 'term' that we've identified (has an 'id') as 
-- being worthy of naming.
class HasUID c => NamedIdea c where
  -- | Lens to the term (a noun phrase)
  term :: Lens' c NP

