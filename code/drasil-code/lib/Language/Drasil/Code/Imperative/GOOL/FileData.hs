-- | Defines the underlying data types used in the package extension.
module Language.Drasil.Code.Imperative.GOOL.Data (FileAndContents(filePath,
  fileDoc), fileAndContents, PackData(packProg, packAux), packD
) where

import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

import Drasil.GOOL (ProgData)

-- | The underlying data type for auxiliary files in all renderers.
data FileAndContents = FileAndContents {filePath :: FilePath, fileDoc :: Doc}

-- | Constructor for auxiliary files.
fileAndContents :: FilePath -> Doc -> FileAndContents
fileAndContents = FileAndContents

-- | The underlying data type for packages in all renderers.
data PackData = PackD {packProg :: ProgData, packAux :: [FileAndContents]}

-- | Constructor for package data.
packD :: ProgData -> [FileAndContents] -> PackData
packD p as = PackD p (filter (not . isEmpty . fileDoc) as)
