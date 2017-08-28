module Main (main) where

import Language.Drasil

import Drasil.HGHC.HGHC (srsBody, mgBody, misBody, hghcSymMap{-, modules-})
--import Drasil.HGHC.Modules (executable)
--import Drasil.HGHC.HeatTransfer (hghcSymMap)

docs :: [Recipe]
docs = [
  Recipe (Website "SRS") srsBody,
  Recipe (Website "MG") mgBody,
  Recipe (Website "MIS") misBody,
  Recipe (SRS "SRS") srsBody,
  Recipe (MG "MG") mgBody,
  Recipe (MIS "MIS") misBody
  ]

main :: IO ()            
main = do
  gen docs hghcSymMap
  --genCode executable modules hghcSymMap
