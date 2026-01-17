-- | Defines the underlying data types used in the package extension.
module Language.Drasil.Code.FileData (FileAndContents(filePath,
  fileDoc), fileAndContents, PackData(packProg, packAux), packD
) where

import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

-- | The underlying data type for auxiliary files in all renderers.
data FileAndContents = FileAndContents {filePath :: FilePath, fileDoc :: Doc}

-- | Constructor for auxiliary files.
fileAndContents :: FilePath -> Doc -> FileAndContents
fileAndContents = FileAndContents

-- | The underlying data type for packages in all renderers.
data PackData a = PackD {packProg :: a, packAux :: [FileAndContents]}

-- | Constructor for package data.
packD :: a -> [FileAndContents] -> PackData a
packD p as = PackD p (filter (not . isEmpty . fileDoc) as)
