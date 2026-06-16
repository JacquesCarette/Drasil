module Drasil.GlassBR.Concepts (
  beam, cantilever, edge, glaSlab, glass, lShareFac, plane, responseTy,
  blastRisk, glaPlane, aR, stdOffDist, idglass,
  glassTypeFac, lResistance, nFL, lDurFac,
  ptOfExplsn, con', iGlass, lGlass, annealed, fullyT, heatS
) where

import Drasil.Database (mkUid)
import Language.Drasil (commonIdeaWithDict, cn', nounPhraseSP, CI, IdeaDict, idea')
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)
import Data.Drasil.Concepts.Documentation (response, type_)

idglass :: IdeaDict
idglass = idea' (mkUid "glass") (cn' "Glass")

con' :: [IdeaDict]
con' = [beam, blastRisk, cantilever, edge, glaPlane, glaSlab, plane,
  glass, ptOfExplsn, responseTy]

annealed, aR, fullyT, glassTypeFac, heatS, lDurFac, iGlass, lGlass,
  lResistance, lShareFac, stdOffDist, nFL :: CI

--FIXME: Add compound nounphrases

annealed      = commonIdeaWithDict (mkUid "annealed")      (nounPhraseSP "annealed")                "AN"       [idglass]
aR            = commonIdeaWithDict (mkUid "aR")            (nounPhraseSP "aspect ratio")            "AR"       [idglass]
fullyT        = commonIdeaWithDict (mkUid "fullyT")        (nounPhraseSP "fully tempered")          "FT"       [idglass]
glassTypeFac  = commonIdeaWithDict (mkUid "glassTypeFac")  (nounPhraseSP "glass type factor")       "GTF"      [idglass]
heatS         = commonIdeaWithDict (mkUid "heatS")         (nounPhraseSP "heat strengthened")       "HS"       [idglass]
iGlass        = commonIdeaWithDict (mkUid "iGlass")        (nounPhraseSP "insulating glass")        "IG"       [idglass]
lGlass        = commonIdeaWithDict (mkUid "lGlass")        (nounPhraseSP "laminated glass")         "LG"       [idglass]
lResistance   = commonIdeaWithDict (mkUid "lResistance")   (nounPhraseSP "load resistance")         "LR"       [idglass]
lShareFac     = commonIdeaWithDict (mkUid "lShareFac")     (nounPhraseSP "load share factor")       "LSF"      [idglass]
stdOffDist    = commonIdeaWithDict (mkUid "stdOffDist")    (nounPhraseSP "stand off distance")      "SD"       [idglass]
lDurFac       = commonIdeaWithDict (mkUid "loadDurFactor") (nounPhraseSP "load duration factor")    "LDF"      [idglass]
nFL           = commonIdeaWithDict (mkUid "nFL")           (nounPhraseSP "non-factored load")       "NFL"      [idglass]

{-Terminology-}
-- TODO: See if we can make some of these terms less specific and/or parameterized.

beam, blastRisk, cantilever, edge, glaPlane, glaSlab, plane,
  glass, ptOfExplsn, responseTy :: IdeaDict
beam         = idea' (mkUid "beam")       (cn' "beam")
blastRisk    = idea' (mkUid "blastRisk")  (nounPhraseSP "blast risk")
cantilever   = idea' (mkUid "cantilever") (nounPhraseSP "cantilever")
edge         = idea' (mkUid "edge")       (cn'          "edge")
glass        = idea' (mkUid "glass")      (nounPhraseSP "glass")
glaSlab      = idea' (mkUid "glaSlab")    (cn' "glass slab")
plane        = idea' (mkUid "plane")      (cn' "plane")

ptOfExplsn   = idea' (mkUid "ptOfExplsn") (cn' "point of explosion")

glaPlane     = compoundNC glass plane
responseTy   = compoundNC response type_
