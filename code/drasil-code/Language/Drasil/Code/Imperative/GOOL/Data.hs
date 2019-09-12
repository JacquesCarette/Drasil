module Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, emptyAux, 
  PackData(..), packD, emptyPack
) where

import Text.PrettyPrint.HughesPJ (Doc, empty)

data AuxData = AD {auxFilePath :: FilePath, auxDoc :: Doc}

ad :: String -> Doc -> AuxData
ad = AD

emptyAux :: AuxData
emptyAux = ad "" empty

data PackData = PackD {packProg :: ProgData, packAux :: [AuxData]}

packD :: ProgData -> [AuxData] -> PackData
packD = PackD

emptyPack :: PackData
emptyPack = packD emptyProg []