module Drasil.SSP.DataDesc (inputMod) where

import Language.Drasil.Code (Func, Mod(Mod), funcData)
--import Drasil.SSP.Unitals (elasticMod, effCohesion,
  --poissnsRatio, fricAngle, dryWeight, satWeight, waterWeight)

inputMod :: Mod
inputMod = Mod "InputFormat" [inputData]

inputData :: Func
inputData = funcData "get_input" [
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