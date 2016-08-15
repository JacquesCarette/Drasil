module Main where
import Language.Drasil.Output.Formats (DocType(SRS,MG,Website))
import Example.Drasil.GlassBR.GlassBRBody (glassBR_srs, glassBR_mg)
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

docs :: [Recipe]
docs = [Recipe (SRS "GlassBR_SRS")     glassBR_srs,
        Recipe (Website "GlassBR_SRS") glassBR_srs,
        Recipe (MG "GlassBR_MG")       glassBR_mg
        ]
main :: IO()
main = do
  gen docs
