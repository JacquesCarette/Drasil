module Main (main) where

import Language.Drasil

import Drasil.HGHC.HGHC (srsBody, allSymbols)

docs :: [Recipe]
docs = [
  Recipe (Website "SRS") srsBody,
  Recipe (SRS "SRS") srsBody --,
  ]

main :: IO ()            
main = do
  gen docs allSymbols
  --genCode thisChoices thisCode
