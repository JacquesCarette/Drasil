module Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, 
  PackData(..), packD
) where

import GOOL.Drasil (ProgData)

import Text.PrettyPrint.HughesPJ (Doc)

data AuxData = AD {auxFilePath :: FilePath, auxDoc :: Doc}

ad :: FilePath -> Doc -> AuxData
ad = AD

data PackData = PackD {packProg :: ProgData, packAux :: [AuxData]}

packD :: ProgData -> [AuxData] -> PackData
packD = PackD
