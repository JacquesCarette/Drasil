module Drasil.WebsiteLayout.Concepts where

import Language.Drasil
--import Language.Drasil.Chunk.Concept.NamedCombinators
--import qualified Language.Drasil.Sentence.Combinators as S

--import Data.Drasil.Concepts.Documentation (constant)

idwebsitelayout :: IdeaDict
idwebsitelayout      = mkIdea  "websitelayout"          (cn' "Website Layout")                 Nothing

acronyms :: [CI]
acronyms = [websitelayout]

websitelayout :: CI
websitelayout  = commonIdeaWithDict "websitelayout" (pn "website layout library") "Websitelayout" [idwebsitelayout]

