module Drasil.SSP.Defs where --export all of this file

import Language.Drasil
import Data.Drasil.Concepts.Documentation (assumption, dataDefn, genDefn, 
  goalStmt, inModel, likelyChg, physSyst, property, requirement, safety, srs,
  thModel, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (surface)
import Data.Drasil.Concepts.Education (mechanics)

import Data.Drasil.Phrase(of_'', compoundNC)
import Data.Drasil.IdeaDicts hiding (dataDefn)

----Acronyms-----
acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg,
  physSyst, requirement, srs, ssa, thModel, typUnc, unlikelyChg]
  
ssa, ssp :: CI
ssa = commonIdeaWithDict "ssa" (cnIS "slope stability analysis") "SSA" [civilEng]
ssp = commonIdeaWithDict "ssp" (cn' "slope stability problem") "SSP"   [civilEng]

sspdef :: [NamedChunk]
sspdef = [factor, soil, material, intrslce, slip, slope, slice, morPrice, rgFnElm,
  slpSrf, soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr, soilMechanics]

sspdef' :: [ConceptChunk]
sspdef' = [crtSlpSrf, plnStrn, fs_concept]

----Other Common Phrases----
soil, material, intrslce, slip, slope, slice, morPrice, rgFnElm :: NamedChunk
intrslce = nc "interslice" (cn' "interslice")
material = nc "material"   (cn' "material")
slice    = nc "slice"      (cn' "slice")
slip     = nc "slip"       (cn  "slip") --FIXME: verb (escape or get loose from (a means of restraint))/noun 
                                        --       (an act of sliding unintentionally for a short distance)?
                                        --       (related to issue #129)
slope    = nc "slope"      (cn' "slope")
soil     = nc "soil"       (cn  "soil")

morPrice = nc "morPrice"   (cn  "morgenstern price")
rgFnElm  = nc "rgFnElm"    (cn' "rigid finite element")

slpSrf, soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr, 
  soilMechanics :: NamedChunk
slpSrf        = compoundNC slip surface
soilPrpty     = compoundNC soil     property
mtrlPrpty     = compoundNC material property
itslPrpty     = compoundNC intrslce property
slopeSrf      = compoundNC slope surface
soilLyr       = compoundNC soil (nc "layer" (cn' "layer"))
soilMechanics = compoundNC soil mechanics

crtSlpSrf, plnStrn, fs_concept :: ConceptChunk
--FIXME: move to Concepts/soldMechanics.hs? They are too specific though
plnStrn = dcc "plane strain" (cn' "plane strain") 
  ("The resultant stresses in one of the directions of a " ++
  "3 dimensional material can be approximated as 0. Results " ++
  "when the length of one dimension of the body dominates the " ++
  "others. Stresses in the dominant dimensions direction are " ++
  "the ones that can be approximated as 0.")

crtSlpSrf = dccWDS "critical slip surface" (cn' "critical slip surface") 
  (at_start slpSrf +:+ S "of the" +:+ phrase slope +:+
  S "that has the lowest global" +:+ phrase fs_concept `sC`
  S "and therefore most likely to experience failure.")

fs_concept = dcc "FS" factorOfSafety
  "The global stability of a surface in a slope"
-- OLD DEFN: Stability metric. How likely a slip surface is to
-- experience failure through slipping.

--
factor :: NamedChunk --FIXME: this is here becuase this phrase is
                     --used in datadefs and instance models
factor = nc "factor" (cn' "factor") -- possible use this everywhere
                                      -- (fs, fs_rc, fs_concept...)
factorOfSafety :: NP
factorOfSafety = factor `of_''` safety
