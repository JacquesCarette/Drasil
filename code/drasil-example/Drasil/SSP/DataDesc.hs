module Drasil.SSP.DataDesc (sspInputMod) where

import Language.Drasil.Code (Func, Mod(Mod), funcData)
--import Drasil.SSP.Unitals (elasticMod, effCohesion,
  --poissnsRatio, fricAngle, dryWeight, satWeight, waterWeight)

sspInputMod :: Mod
sspInputMod = Mod "InputFormat" [sspInputData]

sspInputData :: Func
sspInputData = funcData "get_inputs" $ [
{- --FIXME: unfinished. Needs more inputs? 
    --Needs way to think of (x,y) as two seperate things
    --number of layers, layer direction
    
  multiLine (straight $ map (listEntry [WithPattern]) [
  fricAngle, effCohesion, dryWeight, satWeight, elasticMod, poissnsRatio
  ]) ' ',
  --(x,y) co-ordinates, repeated and part of the multiline
  {- the bellow three fields are optional-}
  --number of geometry points
  singleton waterWeight
  --(x,y) water table geometry points -}
  ]