module Drasil.SSP.Defs where --export all of this file

import Language.Drasil
import Data.Drasil.Concepts.Documentation (assumption, dataDefn, genDefn, 
  goalStmt, inModel, likelyChg, physSyst, property, requirement, safety, srs,
  thModel, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (surface)
import Data.Drasil.Concepts.Physics (twoD, force)
import Data.Drasil.Concepts.Education (mechanics)
import Data.Drasil.Concepts.SolidMechanics (mobShear, shearRes)

import Data.Drasil.Phrase(of_'', compoundNC)
import Data.Drasil.IdeaDicts hiding (dataDefn)

----Acronyms-----
acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg,
  physSyst, requirement, srs, ssa, thModel, typUnc, unlikelyChg]

ssa, ssp :: CI
ssa = commonIdeaWithDict "ssa" (cnIS "slope stability analysis") "SSA" [civilEng]
ssp = commonIdeaWithDict "ssp" (pn' "Slope Stability analysis Program") "SSP"   [civilEng]

sspdef :: [NamedChunk]
sspdef = [factor, soil, material, intrslce, layer, slip, slope, slice, morPrice, rgFnElm,
  soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr, soilMechanics]

sspdef' :: [ConceptChunk]
sspdef' = [slpSrf, crtSlpSrf, plnStrn, fs_concept]

----Other Common Phrases----
soil, layer, material, intrslce, slip, slope, slice, morPrice, rgFnElm :: NamedChunk
intrslce = nc "interslice" (cn' "interslice")
layer    = nc "layer"      (cn' "layer")
material = nc "material"   (cn' "material")
slice    = nc "slice"      (cn' "slice")
slip     = nc "slip"       (cn  "slip") --FIXME: verb (escape or get loose from (a means of restraint))/noun 
                                        --       (an act of sliding unintentionally for a short distance)?
                                        --       (related to issue #129)
slope    = nc "slope"      (cn' "slope")
soil     = nc "soil"       (cn  "soil")

morPrice = nc "morPrice"   (cn  "morgenstern price")
rgFnElm  = nc "rgFnElm"    (cn' "rigid finite element")

soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr :: NamedChunk
--slpSrf    = compoundNC slip surface
soilPrpty = compoundNC soil     property
mtrlPrpty = compoundNC material property
itslPrpty = compoundNC intrslce property
slopeSrf  = compoundNC slope surface
soilLyr   = compoundNC soil layer
soilMechanics = compoundNC soil mechanics

slpSrf, crtSlpSrf, plnStrn, fs_concept, waterTable :: ConceptChunk
slpSrf = dccWDS "slip surface" (cn' "slip surface") (S "A" +:+
  phrase surface +:+ S "within a" +:+ phrase slope +:+ S "that has the" +:+
  S "potential to fail or displace due to load or other" +:+. plural force)

--FIXME: move to Concepts/soldMechanics.hs? They are too specific though
plnStrn = dcc "plane strain" (cn' "plane strain") 
  ("The resultant stresses in one of the directions of a " ++
  "3 dimensional material can be approximated as 0. Results " ++
  "when the length of one dimension of the body dominates the " ++
  "others. Stresses in the dominant dimensions direction are " ++
  "the ones that can be approximated as 0.")

crtSlpSrf = dccWDS "critical slip surface" (cn' "critical slip surface") 
  (at_start slpSrf +:+ S "of the" +:+ phrase slope +:+
  S "that has the lowest" +:+ phrase fs_concept `sC`
  S "and is therefore most likely to experience failure.")

fs_concept = dccWDS "FS" factorOfSafety
  (S "The global stability metric of a" +:+ phrase slpSrf +:+ S "of a" +:+
  phrase slope `sC` S "defined as the ratio of" +:+ phrase shearRes +:+ 
  S "to" +:+ phrase mobShear)
-- OLD DEFN: Stability metric. How likely a slip surface is to
-- experience failure through slipping.

waterTable = dcc "water table" (cn' "water table") ("The upper boundary of a" ++
  " saturated zone in the ground.")

--
factor :: NamedChunk --FIXME: this is here becuase this phrase is
                     --used in datadefs and instance models
factor = nc "factor" (cn' "factor") -- possible use this everywhere
                                      -- (fs, fs_rc, fs_concept...)
factorOfSafety :: NP
factorOfSafety = factor `of_''` safety
