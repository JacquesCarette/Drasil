module Drasil.GlassBR.Concepts where --whole file is used

import Language.Drasil

import Data.Drasil.Concepts.Documentation (assumption, 
  dataDefn, goalStmt, inModel, likelyChg, notApp, physSyst, 
  response, requirement, srs, thModel, type_, typUnc)

{--}

glassBRProg :: CommonConcept
glassBRProg = dcc' "glassBRProg" (nounPhraseSP "GlassBR program")
  "The glass safety analysis program" "GlassBR" 

{-Acronyms-}
acronyms :: [CI]
acronyms = [assumption, annealedGlass, aR, dataDefn, fullyTGlass,
  goalStmt, glassTypeFac, heatSGlass, iGlass, inModel, likelyChg, 
  loadDurFactor, lGlass, lResistance, lShareFac, notApp, nFL,
  physSyst, requirement, stdOffDist, srs, thModel, eqTNT, typUnc]

annealedGlass, aR, fullyTGlass, glassTypeFac, heatSGlass, loadDurFactor,
  iGlass, lGlass, lResistance, lShareFac, eqTNT, gLassBR, stdOffDist, nFL :: CI

--FIXME: Add compound nounphrases

annealedGlass = commonIdea "annealedGlass" (nounPhraseSP "annealed glass")          "AN"
aR            = commonIdea "aR"            (nounPhraseSP "aspect ratio")            "AR"
fullyTGlass   = commonIdea "fullyTGlass"   (nounPhraseSP "fully tempered glass")    "FT"
glassTypeFac  = commonIdea "glassTypeFac"  (nounPhraseSP "glass type factor")       "GTF"
heatSGlass    = commonIdea "heatSGlass"    (nounPhraseSP "heat strengthened glass") "HS"
iGlass        = commonIdea "iGlass"        (nounPhraseSP "insulating glass")        "IG"
lGlass        = commonIdea "lGlass"        (nounPhraseSP "laminated glass")         "LG"
lResistance   = commonIdea "lResistance"   (nounPhraseSP "load resistance")         "LR"
lShareFac     = commonIdea "lShareFac"     (nounPhraseSP "load share factor")       "LSF"
eqTNT         = commonIdea "eqTNT"         (nounPhraseSP "TNT (Trinitrotoluene) Equivalent Factor") "TNT"
gLassBR       = commonIdea "gLassBR"       (pn "GlassBR")                           "GlassBR"
stdOffDist    = commonIdea "stdOffDist"    (nounPhraseSP "stand off distance")      "SD"
loadDurFactor = commonIdea "loadDurFactor" (nounPhraseSP "load duration factor")    "LDF"
nFL           = commonIdea "nFL"           (nounPhraseSP "non-factored load")       "NFL"

{-Terminology-}
-- TODO: See if we can make some of these terms less specific and/or parameterized.
 
beam, blastRisk, cantilever, edge, glaPlane, glaSlab, plane,
  glass, ptOfExplsn, responseTy :: NamedChunk
beam         = nc "beam"       (nounPhraseSP "beam")
blastRisk    = nc "blastRisk"  (nounPhraseSP "blast risk")
cantilever   = nc "cantilever" (nounPhraseSP "cantilever")
edge         = nc "edge"       (nounPhraseSP "edge")
glass        = nc "glass"      (nounPhraseSP "glass")
glaSlab      = nc "glaSlab"    (nounPhraseSP "glass slab")
plane        = nc "plane"      (nounPhraseSP "plane")

ptOfExplsn   = nc "ptOfExplsn" (cn' "point of explosion")

glaPlane     = compoundNC glass plane
responseTy   = compoundNC response type_
