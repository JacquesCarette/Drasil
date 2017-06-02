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

acronyms :: [CINP]
acronyms = [assumption, annealedGlass, aspectR, dataDefn, fullyTGlass,
  goalStmt, glassTypeFac, heatSGlass, iGlass, inModel, likelyChg, lDurFac,
  lGlass, lResistance, lShareFac, notApp, nonFactorL, physSyst, requirement,
  srs, thModel, eqTNT]

annealedGlass, aspectR, fullyTGlass, glassTypeFac, heatSGlass,
  iGlass, lDurFac, lGlass, lResistance, lShareFac, notApp, nonFactorL,
  eqTNT, gLassBR :: CINP
--FIXME: So many of these are duplicates of other named chunks/concepts
--FIXME: Add compound nounphrases
annealedGlass = commonINP "annealedGlass" (nounPhraseSP "annealed glass")          "AN"
aspectR       = commonINP' "aspectR"      (nounPhraseSP "aspect ratio")            (Atomic "AR")
fullyTGlass   = commonINP "fullyTGlass"   (nounPhraseSP "fully tempered glass")    "FT"
glassTypeFac  = commonINP "glassTypeFac"  (nounPhraseSP "glass type factor")       "GTF"
heatSGlass    = commonINP "heatSGlass"    (nounPhraseSP "heat strengthened glass") "HS"
iGlass        = commonINP "iGlass"        (nounPhraseSP "insulating glass")        "IG"
lDurFac       = commonINP "lDurFac"       (nounPhraseSP "load duration factor")    "LDF"
lGlass        = commonINP "lGlass"        (nounPhraseSP "laminated glass")         "LG"
lResistance   = commonINP "lResistance"   (nounPhraseSP "load resistance")         "LR"
lShareFac     = commonINP "lShareFac"     (nounPhraseSP "load share factor")       "LSF"
notApp        = commonINP "notApp"        (nounPhraseSP "not applicable")          "N/A"
nonFactorL    = commonINP "nonFactorL"    (nounPhraseSP "non-factored load")       "NFL"     --lowercase?
eqTNT         = commonINP "eqTNT"         (nounPhraseSP "TNT (Trinitrotoluene) Equivalent Factor") "TNT"
gLassBR       = commonINP "gLassBR"       (pn "GlassBR")                           "GlassBR"

{-Terminology-}
-- TODO: See if we can make some of these terms less specific and/or parameterized.
 
blastRisk, glaSlab :: NPNC
blastRisk    = npnc "blastRisk" (nounPhraseSP "blast risk")
glaSlab      = npnc "glaSlab"   (nounPhraseSP "glass slab")