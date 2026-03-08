module Drasil.Metadata.Documentation
  (srs, notebook, software, requirement, softwareReq, specification,
   introduction, learnObj
  )
  where

import Drasil.Metadata.Domains (documentc, softEng)
import Language.Drasil (CI, NP, IdeaDict, nc, cn, cn', commonIdeaWithDict, fterms, compoundPhraseP1)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNCPP)

softReqSpec :: NP
softReqSpec = fterms compoundPhraseP1 softwareReq specification

introduction, software, specification :: IdeaDict
introduction    = nc "introduction"   (cn'    "introduction"       )
software        = nc "software"       (cn     "software"           )
specification   = nc "specification"  (cn'    "specification"      )

softwareReq :: IdeaDict
softwareReq                  = compoundNCPP software requirement

learnObj, srs, notebook, requirement :: CI
learnObj    = commonIdeaWithDict "learnObj"    (cn' "learning objective")                            "LO"      [documentc]

notebook    = commonIdeaWithDict "notebook"    (cn' "notebook")                                      "NB"      [softEng]
requirement = commonIdeaWithDict "requirement" (cn' "requirement")                                   "R"       [softEng]
srs         = commonIdeaWithDict "srs"         softReqSpec                                           "SRS"     [softEng]

