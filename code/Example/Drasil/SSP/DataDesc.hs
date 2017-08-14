module Drasil.SSP.DataDesc where

import Language.Drasil
--import Drasil.SSP.Unitals (elasticMod, cohesion,
  --poissnsRatio, fricAngle, dryWeight, satWeight, waterWeight)

sspInputMod :: Mod
sspInputMod = ModData "InputFormat" [sspInputData]

sspInputData :: DataDesc
sspInputData = [ {- --FIXME: unfinished. Needs more inputs? Needs way to think of (x,y) as two seperate things
  --number of layers, layer direction
  multiLine (straight $ map (listEntry [WithPattern]) [
  fricAngle, cohesion, dryWeight, satWeight, elasticMod, poissnsRatio
  ]) ' ',
  --(x,y) co-ordinates, repeated and part of the multiline
  {- the bellow three fields are optional-}
  --number of geometry points
  singleton waterWeight
  --(x,y) water table geometry points -}
  ]