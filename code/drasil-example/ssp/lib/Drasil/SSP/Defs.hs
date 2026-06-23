module Drasil.SSP.Defs (
  plnStrn, slpSrf, slopeSrf, slope,
  soil, soilPrpty, intrslce, slice, waterTable,
  crtSlpSrf, defs, defs', effFandS, factor, fsConcept,
  layer, morPrice, mtrlPrpty, slip,
  soilLyr, soilMechanics, ssa, stabAnalysis, factorOfSafety, minim,maxim, xCoords,yCoords,
) where

import Control.Lens ((^.))

import Drasil.Database (mkUid)
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.ShortHands (lX,lY)

import Data.Drasil.Concepts.Documentation (analysis, property, safety)
import Data.Drasil.Concepts.Education (mechanics)
import Data.Drasil.Concepts.Math (surface)
import Data.Drasil.Concepts.Physics (threeD, force, stress)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.SolidMechanics (mobShear, normForce, nrmStrss,shearRes)
import Data.Drasil.Quantities.PhysicalProperties (len)

defs :: [IdeaDict]
defs = [factor, soil, intrslce, layer, slip, slope, slice, morPrice,
  soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr, soilMechanics,
  stabAnalysis, ssa]

defs' :: [ConceptChunk]
defs' = [slpSrf, crtSlpSrf, plnStrn, waterTable]

----Other Common Phrases----
soil, layer, material, intrslce, slip, slope, slice, stability,
  morPrice :: IdeaDict
intrslce = idea' (mkUid "interslice") (cn' "interslice")
layer    = idea' (mkUid "layer")      (cn' "layer")
material = idea' (mkUid "material")   (cn' "material")
slice    = idea' (mkUid "slice")      (cn' "slice")
slip     = idea' (mkUid "slip")       (cn  "slip") --FIXME: verb (escape or get loose from (a means of restraint))/noun
                                        --       (an act of sliding unintentionally for a short distance)?
                                        --       (related to issue #129)
slope    = idea' (mkUid "slope")      (cn' "slope")
soil     = idea' (mkUid "soil")       (cn  "soil")
stability = idea' (mkUid "stability") (cn "stability")

morPrice = idea' (mkUid "morPrice")   (pn  "Morgenstern-Price")

soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr, soilMechanics,
  stabAnalysis, ssa, slpSrfCon :: IdeaDict
slpSrfCon = compoundNC slip surface
soilPrpty = compoundNC soil     property
mtrlPrpty = compoundNC material property
itslPrpty = compoundNC intrslce property
slopeSrf  = compoundNC slope surface
soilLyr   = compoundNC soil layer
soilMechanics = compoundNC soil mechanics
stabAnalysis = compoundNC stability analysis
ssa = compoundNC slope stabAnalysis

effFandS, slpSrf, crtSlpSrf, plnStrn, fsConcept, waterTable :: ConceptChunk
effFandS = dccWDS "effective forces and stresses"
  (cn "effective forces and stresses")
  (D.toSent (atStartNP (the normForce)) `S.or_` phrase nrmStrss +:+
  S "carried by the" +:+ phrase soil +:+ S "skeleton" `sC`
  S "composed of the effective" +:+ phrase force `S.or_` phrase stress `S.andThe`
  phrase force `S.or_` phrase stress +:+ S "exerted by water")

slpSrf = dccWDS "slip surface" (slpSrfCon ^. term)
  (D.toSent (atStartNP (a_ surface)) +:+ S "within a" +:+ phrase slope +:+ S "that has the" +:+
  S "potential to fail or displace due to load or other" +:+ plural force)

--FIXME: move to Concepts/soldMechanics.hs? They are too specific though
plnStrn = dccWDS "plane strain" (cn' "plane strain")
  (S "A condition where the resultant" +:+ plural stress +:+ S "in one of" +:+
  S "the directions" `S.ofA` phrase threeD +:+ S "material can be" +:+
  S "approximated as zero. This condition results when a body is" +:+
  S "constrained to not deform in one direction, or when the" +:+
  phrase len +:+ S "of one" +:+ phrase dimension `S.ofThe` S "body" +:+
  S "dominates the others" `sC` S "to the point where it can be assumed as" +:+.
  S "infinite" +:+ atStart' stress +:+ S "in the direction" `S.ofThe` S "dominant" +:+
  phrase dimension +:+ S "can be approximated as zero")

crtSlpSrf = dccWDS "critical slip surface" (cn' "critical slip surface")
  (D.toSent (atStartNP (slpSrf `ofThe` slope)) +:+
  S "that has the lowest" +:+ phrase fsConcept `sC`
  S "and is therefore most likely to experience failure")

fsConcept = dccWDS "FS" factorOfSafety
  (S "The global stability metric" `S.ofA` D.toSent (phraseNP (slpSrf `ofA` slope)) `sC`
  S "defined as the ratio" `S.of_` phrase shearRes +:+
  S "to" +:+ phrase mobShear)
-- OLD DEFN: Stability metric. How likely a slip surface is to
-- experience failure through slipping.

waterTable = cncpt''' (mkUid "water table") (cn' "water table") (S ("The upper boundary of a" ++
  " saturated zone in the ground"))

--
factor :: IdeaDict --FIXME: this is here becuase this phrase is
                     --used in datadefs and instance models
factor = idea' (mkUid "factor") (cn' "factor") -- possible use this everywhere
                                      -- (fs, fs_rc, fsConcept...)
factorOfSafety :: NP
factorOfSafety = factor `of_PS` safety

---------
-- HACK: this belongs in drasil-data
minim, maxim :: IdeaDict -- else clashes with Prelude
minim = idea' (mkUid "minimum") (cn' "minimum")
maxim = idea' (mkUid "maximum") (cn' "maximum")

-- Some sentences want plurals (because of arrays) of things that are normally singular.
xCoords, yCoords :: ConceptChunk
xCoords = cncpt''' (mkUid "xCoords") (nounPhraseSent $ D.P lX D.:-: D.S "-coordinates")
  (S "the location of the points on the x-axis")
yCoords = cncpt''' (mkUid "yCoords") (nounPhraseSent $ D.P lY D.:-: D.S "-coordinates")
  (S "the location of the points on the y-axis")
