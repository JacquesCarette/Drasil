module Drasil.SSP.Defs where

import Drasil.SSP.Units

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Quantities.SolidMechanics

import Control.Lens ((^.))

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
    (phrase $ fs_rc) `sC` S "and therefore most likely to experience failure.")

----Theoretical Models----
-- possibly temporary "factor of safety" hack FIXME?
factor, safety :: NamedChunk
factor = npnc "factor" (cn' "factor")
safety = npnc "safety" (cnIES "safety")

fs_rc :: RelationConcept
fs_rc = makeRC "fs_rc" (factor `of_''` safety) fs_desc fs_rel

fs_rel :: Relation
fs_rel = (C fs) := (C shearRes) / (C mobShear)

fs_desc :: Sentence
fs_desc = 
  S "The stability metric of the slope, known as the factor of safety" +:+
  (sParen $ P $ fs ^. symbol) `sC` S "is determined by the ratio of the" +:+
  S "shear force at the base of the slope" +:+ (sParen $ P $ mobShear ^. symbol) `sC` 
  S "and the resistive shear" +:+. (sParen $ P $ shearRes ^. symbol)
