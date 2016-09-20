module Example.Drasil.SWHS.Generate where

import Language.Drasil (DocType(SRS,MG,Website))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Generate (gen)

import Example.Drasil.SWHS.Body (swhs_srs, swhs_mg)

docs :: [Recipe]
docs = [Recipe (SRS "SWHS_SRS") swhs_srs,
        Recipe (Website "SWHS_SRS") swhs_srs,
        Recipe (MG "SWHS_MG") swhs_mg
       ]

generate :: IO ()       
generate = gen docs