{-# OPTIONS -Wall #-} 
module Main where

import Language.Drasil (DocType(SRS,Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.SWHS.Body (swhs_srs)

docs :: [Recipe]
docs = [Recipe (SRS "SWHS_SRS") swhs_srs,
        Recipe (Website "SWHS_SRS") swhs_srs]

main :: IO ()            
main = do
  gen docs