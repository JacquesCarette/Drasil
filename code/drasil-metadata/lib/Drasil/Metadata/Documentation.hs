module Drasil.Metadata.Documentation
  (srs, notebook, software, requirement, softwareReq, specification)
  where

import Drasil.Metadata.Domains (softEng)
import Language.Drasil (CI, NP, IdeaDict, nc, cn, cn', commonIdeaWithDict, fterms, compoundPhraseP1)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNCPP)

softReqSpec :: NP
softReqSpec = fterms compoundPhraseP1 softwareReq specification

software, specification :: IdeaDict
software        = nc "software"       (cn     "software"           )
specification   = nc "specification"  (cn'    "specification"      )

softwareReq :: IdeaDict
softwareReq                  = compoundNCPP software requirement


srs, notebook, requirement :: CI
notebook    = commonIdeaWithDict "notebook"    (cn' "notebook")                                      "NB"      [softEng]
requirement = commonIdeaWithDict "requirement" (cn' "requirement")                                   "R"       [softEng]
srs         = commonIdeaWithDict "srs"         softReqSpec                                           "SRS"     [softEng]

