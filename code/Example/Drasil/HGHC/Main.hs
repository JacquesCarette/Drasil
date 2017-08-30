module Main (main) where

import Language.Drasil

import Drasil.HGHC.HGHC (srsBody, mgBody, misBody {-, modules-})
import Drasil.HGHC.HeatTransfer (allSymbols)

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
  gen docs allSymbols
  --genCode executable modules hghcSymMap
