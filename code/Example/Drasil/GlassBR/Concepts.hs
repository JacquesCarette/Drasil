module Drasil.GlassBR.Concepts where

import Language.Drasil

--FIXME: Figure out why this wasn't used in body (until now with srsDoc)
glassBRProg :: ConceptChunk
glassBRProg = dcc' "glassBRProg" (nounPhraseSP "GlassBR program")
  "The glass safety analysis program" "GlassBR" 
