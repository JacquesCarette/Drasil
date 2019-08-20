module Drasil.SSP.Defs where --export all of this file

import Language.Drasil
import Data.Drasil.IdeaDicts
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (analysis, assumption, goalStmt,
  likelyChg, physSyst, property, requirement, safety, srs, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Education (mechanics)
import Data.Drasil.Concepts.Math (surface)
import Data.Drasil.Concepts.Physics (twoD, threeD, force, stress)
import Data.Drasil.Concepts.PhysicalProperties (dimension, len)
import Data.Drasil.Concepts.SolidMechanics (mobShear, normForce, nrmStrss,shearRes)

----Acronyms-----
acronyms :: [CI]
acronyms = [twoD, threeD, assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg,
  physSyst, requirement, srs, ssp, thModel, typUnc, unlikelyChg]

ssp :: CI
ssp = commonIdeaWithDict "ssp" (pn' "Slope Stability analysis Program") "SSP"   [civilEng]

defs :: [NamedChunk]
defs = [factor, soil, material, intrslce, layer, slip, slope, slice, morPrice,
  soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr, soilMechanics, 
  stabAnalysis, ssa]

defs' :: [ConceptChunk]
defs' = [slpSrf, crtSlpSrf, plnStrn, fsConcept, waterTable]

----Other Common Phrases----
soil, layer, material, intrslce, slip, slope, slice, stability,
  morPrice :: NamedChunk
intrslce = nc "interslice" (cn' "interslice")
layer    = nc "layer"      (cn' "layer")
material = nc "material"   (cn' "material")
slice    = nc "slice"      (cn' "slice")
slip     = nc "slip"       (cn  "slip") --FIXME: verb (escape or get loose from (a means of restraint))/noun 
                                        --       (an act of sliding unintentionally for a short distance)?
                                        --       (related to issue #129)
slope    = nc "slope"      (cn' "slope")
soil     = nc "soil"       (cn  "soil")
stability = nc "stability" (cn "stability")

morPrice = nc "morPrice"   (pn  "Morgenstern-Price")

soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr, soilMechanics, 
  stabAnalysis, ssa :: NamedChunk
--slpSrf    = compoundNC slip surface
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
  (S "The" +:+ phrase normForce `sOr` phrase nrmStrss +:+
  S "carried by the" +:+ phrase soil +:+ S "skeleton" `sC`
  S "composed of the effective" +:+ phrase force `sOr` phrase stress `andThe`
  phrase force `sOr` phrase stress +:+ S "exerted by water")

slpSrf = dccWDS "slip surface" (cn' "slip surface") (S "A" +:+
  phrase surface +:+ S "within a" +:+ phrase slope +:+ S "that has the" +:+
  S "potential to fail or displace due to load or other" +:+ plural force)

--FIXME: move to Concepts/soldMechanics.hs? They are too specific though
plnStrn = dccWDS "plane strain" (cn' "plane strain") 
  (S "A condition where the resultant" +:+ plural stress +:+ S "in one of" +:+
  S "the directions of a " +:+ phrase threeD +:+ S "material can be" +:+
  S "approximated as zero. This condition results when a body is" +:+ 
  S "constrained to not deform in one direction, or when the" +:+ 
  phrase len +:+ S "of one" +:+ phrase dimension +:+ S "of the body" +:+
  S "dominates the others, to the point where it can be assumed as" +:+.
  S "infinite" +:+ atStart' stress +:+ S "in the direction of the" +:+
  S "dominant" +:+ phrase dimension +:+ S "can be approximated as zero")

crtSlpSrf = dccWDS "critical slip surface" (cn' "critical slip surface") 
  (atStart slpSrf +:+ S "of the" +:+ phrase slope +:+
  S "that has the lowest" +:+ phrase fsConcept `sC`
  S "and is therefore most likely to experience failure")

fsConcept = dccWDS "FS" factorOfSafety
  (S "The global stability metric of a" +:+ phrase slpSrf +:+ S "of a" +:+
  phrase slope `sC` S "defined as the ratio of" +:+ phrase shearRes +:+ 
  S "to" +:+ phrase mobShear)
-- OLD DEFN: Stability metric. How likely a slip surface is to
-- experience failure through slipping.

waterTable = dcc "water table" (cn' "water table") ("The upper boundary of a" ++
  " saturated zone in the ground")

--
factor :: NamedChunk --FIXME: this is here becuase this phrase is
                     --used in datadefs and instance models
factor = nc "factor" (cn' "factor") -- possible use this everywhere
                                      -- (fs, fs_rc, fsConcept...)
factorOfSafety :: NP
factorOfSafety = factor `of_''` safety
