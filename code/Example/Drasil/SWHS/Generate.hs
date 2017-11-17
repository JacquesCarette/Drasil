module Drasil.SWHS.Generate (generate) where

import Language.Drasil

import Drasil.SWHS.Body (swhs_srs')
import Drasil.SWHS.DataDefs (swhsSymMap)

docs :: [Recipe]
docs = [Recipe (SRS "SWHS_SRS") swhs_srs',
        Recipe (Website "SWHS_SRS") swhs_srs'
       ]

generate :: IO ()
generate = gen docs swhsSymMap