-- | Defining classes that represent knowledge about Documents
module Language.Drasil.Classes.Document (
  HasCitation(getCitations)
  ) where

import Language.Drasil.Chunk.Citation (Citation)

import Control.Lens (Lens')

-- | Some documents, as well as some pieces of knowledge, have citations
class HasCitation c where
  getCitations :: Lens' c [Citation]
