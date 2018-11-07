module Drasil.GlassBR.Concepts where --whole file is used

import Language.Drasil

import Data.Drasil.Concepts.Documentation (assumption, dataDefn, goalStmt, inModel, 
    likelyChg, notApp, physSyst, response, requirement, srs, thModel, type_, typUnc, 
    unlikelyChg)
import Data.Drasil.Phrase (compoundNC)

{--}

{-glassBRProg :: CommonConcept
glassBRProg = dcc' "glassBRProg" (nounPhraseSP "GlassBR program")
  "The glass safety analysis program" "GlassBR"-}

{-Acronyms-}
acronyms :: [CI]
acronyms = [assumption, annealed, aR, dataDefn, fullyT, goalStmt, 
  glassTypeFac, heatS, iGlass, inModel, likelyChg, loadDurFactor, 
  lGlass, lResistance, lShareFac, notApp, nFL, physSyst, requirement, 
  stdOffDist, srs, thModel, typUnc, unlikelyChg]

annealed, aR, fullyT, glassTypeFac, heatS, loadDurFactor, iGlass, lGlass, 
  lResistance, lShareFac, gLassBR, stdOffDist, nFL :: CI

--FIXME: Add compound nounphrases

annealed      = commonIdea "annealed"      (nounPhraseSP "annealed")                "AN"       ["domain specific"]
aR            = commonIdea "aR"            (nounPhraseSP "aspect ratio")            "AR"       ["domain specific"]
fullyT        = commonIdea "fullyT"        (nounPhraseSP "fully tempered")          "FT"       ["domain specific"]
glassTypeFac  = commonIdea "glassTypeFac"  (nounPhraseSP "glass type factor")       "GTF"      ["domain specific"]
heatS         = commonIdea "heatS"         (nounPhraseSP "heat strengthened")       "HS"       ["domain specific"]
iGlass        = commonIdea "iGlass"        (nounPhraseSP "insulating glass")        "IG"       ["domain specific"]
lGlass        = commonIdea "lGlass"        (nounPhraseSP "laminated glass")         "LG"       ["domain specific"]
lResistance   = commonIdea "lResistance"   (nounPhraseSP "load resistance")         "LR"       ["domain specific"]
lShareFac     = commonIdea "lShareFac"     (nounPhraseSP "load share factor")       "LSF"      ["domain specific"]
gLassBR       = commonIdea "gLassBR"       (pn "GlassBR")                           "GlassBR"  ["domain specific"]
stdOffDist    = commonIdea "stdOffDist"    (nounPhraseSP "stand off distance")      "SD"       ["domain specific"]
loadDurFactor = commonIdea "loadDurFactor" (nounPhraseSP "load duration factor")    "LDF"      ["domain specific"]
nFL           = commonIdea "nFL"           (nounPhraseSP "non-factored load")       "NFL"      ["domain specific"]

{-Terminology-}
-- TODO: See if we can make some of these terms less specific and/or parameterized.
 
beam, blastRisk, cantilever, edge, glaPlane, glaSlab, plane,
  glass, ptOfExplsn, responseTy :: NamedChunk
beam         = nc "beam"       (nounPhraseSP "beam")
blastRisk    = nc "blastRisk"  (nounPhraseSP "blast risk")
cantilever   = nc "cantilever" (nounPhraseSP "cantilever")
edge         = nc "edge"       (cn'          "edge")
glass        = nc "glass"      (nounPhraseSP "glass")
glaSlab      = nc "glaSlab"    (nounPhraseSP "glass slab")
plane        = nc "plane"      (nounPhraseSP "plane")

ptOfExplsn   = nc "ptOfExplsn" (cn' "point of explosion")

glaPlane     = compoundNC glass plane
responseTy   = compoundNC response type_
