module Main where
import Language.Drasil(DocType(SRS,MG,Website),Recipe(..),gen)
import Drasil.GlassBR.Body (glassBR_srs, glassBR_srs', glassBR_mg)

docs :: [Recipe]
docs = [Recipe (SRS "GlassBR_SRS")     glassBR_srs',
        Recipe (Website "GlassBR_SRS") glassBR_srs',
        Recipe (MG "GlassBR_MG")       glassBR_mg
        ]
main :: IO()
main = do
  gen docs
