-- | Defines the underlying data types used in the package extension.
module Language.Drasil.Code.FileData (FileAndContents(filePath,
  fileDoc), fileAndContents, PackageData(packageProg, packageAux), packageData
) where

import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

-- | The underlying data type for auxiliary files in all renderers.
data FileAndContents = FileAndContents {filePath :: FilePath, fileDoc :: Doc}

-- | Constructor for auxiliary files.
fileAndContents :: FilePath -> Doc -> FileAndContents
fileAndContents = FileAndContents

-- | The underlying data type for packages in all renderers.
data PackageData a = PackD {packageProg :: a, packageAux :: [FileAndContents]}

-- | Constructor for package data.
packageData :: a -> [FileAndContents] -> PackageData a
packageData p as = PackD p (filter (not . isEmpty . fileDoc) as)
