module Drasil.GlassBR.Concepts where --whole file is used

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (assumption, goalStmt, likelyChg,
  notApp, physSyst, response, requirement, refBy, refName, srs, type_, typUnc, 
  unlikelyChg)
import Drasil.Metadata (dataDefn, inModel, thModel)

{--}
idglass :: IdeaDict
idglass      = mkIdea  "glass"          (cn' "Glass")                 Nothing

{--}
con :: [CI]
con = [annealed, aR, fullyT, glassTypeFac, heatS, lDurFac, iGlass, lGlass, 
  lResistance, lShareFac, stdOffDist, nFL]

con' :: [IdeaDict]
con' = [beam, blastRisk, cantilever, edge, glaPlane, glaSlab, plane,
  glass, ptOfExplsn, responseTy]

{-Acronyms-}
acronyms :: [CI]
acronyms = [assumption, annealed, aR, dataDefn, fullyT, goalStmt, 
  glassTypeFac, heatS, iGlass, inModel, likelyChg, lDurFac, 
  lGlass, lResistance, lShareFac, notApp, nFL, physSyst, requirement, 
  refBy, refName, stdOffDist, srs, thModel, typUnc, unlikelyChg]

annealed, aR, fullyT, glassTypeFac, heatS, lDurFac, iGlass, lGlass, 
  lResistance, lShareFac, stdOffDist, nFL :: CI

--FIXME: Add compound nounphrases

annealed      = commonIdeaWithDict "annealed"      (nounPhraseSP "annealed")                "AN"       [idglass]
aR            = commonIdeaWithDict "aR"            (nounPhraseSP "aspect ratio")            "AR"       [idglass]
fullyT        = commonIdeaWithDict "fullyT"        (nounPhraseSP "fully tempered")          "FT"       [idglass]
glassTypeFac  = commonIdeaWithDict "glassTypeFac"  (nounPhraseSP "glass type factor")       "GTF"      [idglass]
heatS         = commonIdeaWithDict "heatS"         (nounPhraseSP "heat strengthened")       "HS"       [idglass]
iGlass        = commonIdeaWithDict "iGlass"        (nounPhraseSP "insulating glass")        "IG"       [idglass]
lGlass        = commonIdeaWithDict "lGlass"        (nounPhraseSP "laminated glass")         "LG"       [idglass]
lResistance   = commonIdeaWithDict "lResistance"   (nounPhraseSP "load resistance")         "LR"       [idglass]
lShareFac     = commonIdeaWithDict "lShareFac"     (nounPhraseSP "load share factor")       "LSF"      [idglass]
stdOffDist    = commonIdeaWithDict "stdOffDist"    (nounPhraseSP "stand off distance")      "SD"       [idglass]
lDurFac       = commonIdeaWithDict "loadDurFactor" (nounPhraseSP "load duration factor")    "LDF"      [idglass]
nFL           = commonIdeaWithDict "nFL"           (nounPhraseSP "non-factored load")       "NFL"      [idglass]

{-Terminology-}
-- TODO: See if we can make some of these terms less specific and/or parameterized.
 
beam, blastRisk, cantilever, edge, glaPlane, glaSlab, plane,
  glass, ptOfExplsn, responseTy :: IdeaDict
beam         = nc "beam"       (cn' "beam")
blastRisk    = nc "blastRisk"  (nounPhraseSP "blast risk")
cantilever   = nc "cantilever" (nounPhraseSP "cantilever")
edge         = nc "edge"       (cn'          "edge")
glass        = nc "glass"      (nounPhraseSP "glass")
glaSlab      = nc "glaSlab"    (cn' "glass slab")
plane        = nc "plane"      (cn' "plane")

ptOfExplsn   = nc "ptOfExplsn" (cn' "point of explosion")

glaPlane     = compoundNC glass plane
responseTy   = compoundNC response type_
