{-# LANGUAGE  PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Defines the underlying data types used in the package extension.
module Language.Drasil.Code.FileData (FileAndContents(filePath, fileDoc),
  fileAndContents, hasPathAndDocToFileAndContents,
  PackageData(packageProg, packageAux), pattern PackageData
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

pattern PackageData :: a -> [FileAndContents] -> PackageData a
pattern PackageData prog aux <- PackD prog aux
  where
    PackageData prog aux = PackD prog (filter (not . isEmpty . fileDoc) aux)
{-# COMPLETE PackageData #-}
