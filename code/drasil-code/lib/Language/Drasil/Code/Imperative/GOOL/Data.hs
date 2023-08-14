-- | Defines the underlying data types used in the package extension.
module Language.Drasil.Code.Imperative.GOOL.Data (AuxData(auxFilePath, auxDoc),
  ad, PackData(packProg, packAux), packD
) where

import GOOL.Drasil (ProgData)

import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

-- | The underlying data type for auxiliary files in all renderers.
data AuxData = AD {auxFilePath :: FilePath, auxDoc :: Doc}

-- | Constructor for auxiliary files.
ad :: FilePath -> Doc -> AuxData
ad = AD

-- | The underlying data type for packages in all renderers.
data PackData = PackD {packProg :: ProgData, packAux :: [AuxData]}

-- | Constructor for package data.
packD :: ProgData -> [AuxData] -> PackData
packD p as = PackD p (filter (not . isEmpty . auxDoc) as)
