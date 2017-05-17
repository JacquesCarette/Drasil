module Drasil.SSP.Defs where

import Drasil.SSP.Units

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Quantities.SolidMechanics

import Control.Lens ((^.))

----Acronyms-----
acronyms :: [CINP]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,
  physSyst,requirement,srs,ssa,thModel]
  
ssa, ssp :: CINP
ssa = commonINP "ssa" (cnIS "slope stability analysis") "SSA"
ssp = commonINP "ssp" (cn' "slope stability problem") "SSP"

----Other Common Phrases----
soil, material, intrslce, surface_, slip, slope, slice,
  morPrice, rgFnElm :: NPNC
intrslce = npnc "interslice" (cn' "interslice")
material = npnc "material"   (cn' "material")
slice    = npnc "slice"      (cn' "slice")
slip     = npnc "slip"       (cn  "slip") --FIXME: adjective?
slope    = npnc "slope"      (cn' "slope")
soil     = npnc "soil"       (cn  "soil")
surface_ = npnc "surface"    (cn' "surface") -- FIXME: use the one from concepts.math

morPrice = npnc "morPrice"   (cn  "morgenstern price")
rgFnElm  = npnc "rgFnElm"    (cn' "rigid finite element")

slpSrf, crtSlpSrf, soilPrpty, mtrlPrpty, itslPrpty, slopeSrf,
  soilLyr :: NPNC
slpSrf    = compoundNPNC slip surface_
crtSlpSrf = compoundNPNC (npnc "critical" (cn "critical")) slpSrf
soilPrpty = compoundNPNC soil     property
mtrlPrpty = compoundNPNC material property
itslPrpty = compoundNPNC intrslce property
slopeSrf  = compoundNPNC slope surface_
soilLyr   = compoundNPNC soil (npnc "layer" (cn' "layer"))

----Theoretical Models----
-- possibly temporary "factor of safety" hack FIXME?
factor, safety :: NPNC
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
