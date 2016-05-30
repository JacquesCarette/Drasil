module GlassBRMain where
import Language.Drasil.Output.Formats (DocType(SRS,LPM,Website))
import GlassBRBody1 (glassBR_srs)
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

docs :: [Recipe]
docs = [Recipe (SRS "GlassBR_SRS")     glassBR_srs,
        Recipe (Website "GlassBR_SRS") glassBR_srs
        ]
main :: IO()
main = do
  gen docs
