module Main where

import Language.Drasil (DocType(SRS,MG,MIS,Website),Recipe(..),gen{-,genCode-})

import Drasil.HGHC.HGHC (srsBody,mgBody,misBody{-,modules-})
--import Drasil.HGHC.Modules (executable)
--import Drasil.HGHC.HeatTransfer (hghcSymMap)

docs :: [Recipe]
docs = [ Recipe (Website "SRS") srsBody
       , Recipe (Website "MG") mgBody
       , Recipe (Website "MIS") misBody
       , Recipe (SRS "SRS") srsBody
       , Recipe (MG "MG") mgBody
       , Recipe (MIS "MIS") misBody
       ]

main :: IO ()            
main = do
  gen docs
  --genCode executable modules hghcSymMap
