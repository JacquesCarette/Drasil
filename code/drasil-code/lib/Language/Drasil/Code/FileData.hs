{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Defines the underlying data types used in the package extension.
module Language.Drasil.Code.FileData (FileAndContents(filePath, fileDoc),
  fileAndContents, hasPathAndDocToFileAndContents, PackageData(packageProg,
  packageAux), packageData
) where

import Text.PrettyPrint.HughesPJ (Doc, isEmpty)
import Utils.Drasil (HasPathAndDoc(..))

-- | The underlying data type for auxiliary files in all renderers.
data FileAndContents = FileAndContents {filePath :: FilePath, fileDoc :: Doc}

-- | Constructor for auxiliary files.
fileAndContents :: FilePath -> Doc -> FileAndContents
fileAndContents = FileAndContents

instance HasPathAndDoc FileAndContents Doc where
  getPath = filePath
  getDoc = fileDoc

hasPathAndDocToFileAndContents :: (HasPathAndDoc a Doc) => a -> FileAndContents
hasPathAndDocToFileAndContents file = fileAndContents (getPath file) (getDoc file)

-- | The underlying data type for packages in all renderers.
data PackageData a = PackD {packageProg :: a, packageAux :: [FileAndContents]}

-- | Constructor for package data.
packageData :: a -> [FileAndContents] -> PackageData a
packageData p as = PackD p (filter (not . isEmpty . fileDoc) as)
