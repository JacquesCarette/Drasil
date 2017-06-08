module Drasil.SSP.Defs where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.SentenceStructures

----Acronyms-----
acronyms :: [CI]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,
  physSyst,requirement,srs,ssa,thModel]
  
ssa, ssp :: CI
ssa = commonIdea "ssa" (cnIS "slope stability analysis") "SSA"
ssp = commonIdea "ssp" (cn' "slope stability problem") "SSP"

----Other Common Phrases----
soil, material, intrslce, surface_, slip, slope, slice,
  morPrice, rgFnElm :: NamedChunk
intrslce = npnc "interslice" (cn' "interslice")
material = npnc "material"   (cn' "material")
slice    = npnc "slice"      (cn' "slice")
slip     = npnc "slip"       (cn  "slip") --FIXME: adjective?
slope    = npnc "slope"      (cn' "slope")
soil     = npnc "soil"       (cn  "soil")
surface_ = npnc "surface"    (cn' "surface") -- FIXME: use the one from concepts.math

morPrice = npnc "morPrice"   (cn  "morgenstern price")
rgFnElm  = npnc "rgFnElm"    (cn' "rigid finite element")

slpSrf, soilPrpty, mtrlPrpty, itslPrpty, slopeSrf, soilLyr :: NamedChunk
slpSrf    = compoundNC slip surface_
soilPrpty = compoundNC soil     property
mtrlPrpty = compoundNC material property
itslPrpty = compoundNC intrslce property
slopeSrf  = compoundNC slope surface_
soilLyr   = compoundNC soil (npnc "layer" (cn' "layer"))

crtSlpSrf, plnStrn :: ConceptChunk --FIXME: move to Concepts/soldMechanics.hs? They are too spicific though
plnStrn = dcc "plane strain" (cn' "plane strain") 
          ("The resultant stresses in one of the directions of a " ++
          "3 dimensional material can be approximated as 0. Results " ++
          "when the length of one dimension of the body dominates the " ++
          "others. Stresses in the dominate dimensions direction are " ++
          "the ones that can be approximated as 0.")
          
crtSlpSrf = dccWDS "critical slip surface" (cn' "critical slip surface") 
    ((at_start slpSrf) +:+ S "of the" +:+ (phrase slope) +:+ S "that has the lowest global" +:+
    ((phrase factor) `sOf` (phrase safety)) `sC` S "and therefore most likely to experience failure.")
    
--
factor, safety :: NamedChunk
factor = npnc "factor" (cn' "factor")
safety = npnc "safety" (cnIES "safety")
factorOfSafety :: NP
factorOfSafety = factor `of_''` safety