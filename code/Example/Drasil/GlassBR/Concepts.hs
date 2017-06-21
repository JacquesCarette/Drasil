module Drasil.GlassBR.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Prelude hiding (id)

--FIXME: Figure out why this wasn't used in body (until now with srsDoc)
glassBRProg :: ConceptChunk
glassBRProg = dcc' "glassBRProg" (nounPhraseSP "GlassBR program")
  "The glass safety analysis program" "GlassBR" 

{-Acronyms-}
-- FIXME: Use actual acronyms instead of CCs.

acronyms :: [CI]
acronyms = [assumption, annealedGlass, aspectR, dataDefn, fullyTGlass,
  goalStmt, glassTypeFac, heatSGlass, iGlass, inModel, likelyChg, 
  lGlass, lResistance, lShareFac, physSyst, requirement, srs,
  thModel, eqTNT]

annealedGlass, aspectR, fullyTGlass, glassTypeFac, heatSGlass,
  iGlass, lGlass, lResistance, lShareFac, eqTNT, gLassBR :: CI

--FIXME: So many of these are duplicates of other named chunks/concepts
--FIXME: Add compound nounphrases
--FIXME: Remove `glassTypeFac` once figured out how to incorporate `glassTypeFac_` into s6_1_1_bullets.

annealedGlass = commonIdea "annealedGlass" (nounPhraseSP "annealed glass")          "AN"
aspectR       = commonIdea' "aspectR"      (nounPhraseSP "aspect ratio")            (Atomic "AR")
fullyTGlass   = commonIdea "fullyTGlass"   (nounPhraseSP "fully tempered glass")    "FT"
glassTypeFac  = commonIdea "glassTypeFac"  (nounPhraseSP "glass type factor")       "GTF"
heatSGlass    = commonIdea "heatSGlass"    (nounPhraseSP "heat strengthened glass") "HS"
iGlass        = commonIdea "iGlass"        (nounPhraseSP "insulating glass")        "IG"
lGlass        = commonIdea "lGlass"        (nounPhraseSP "laminated glass")         "LG"
lResistance   = commonIdea "lResistance"   (nounPhraseSP "load resistance")         "LR"
lShareFac     = commonIdea "lShareFac"     (nounPhraseSP "load share factor")       "LSF"
eqTNT         = commonIdea "eqTNT"         (nounPhraseSP "TNT (Trinitrotoluene) Equivalent Factor") "TNT"
gLassBR       = commonIdea "gLassBR"       (pn "GlassBR")                           "GlassBR"

{-Terminology-}
-- TODO: See if we can make some of these terms less specific and/or parameterized.
 
blastRisk, glaSlab :: NamedChunk
blastRisk    = npnc "blastRisk" (nounPhraseSP "blast risk")
glaSlab      = npnc "glaSlab"   (nounPhraseSP "glass slab")