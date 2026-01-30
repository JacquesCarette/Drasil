{-# LANGUAGE  PatternSynonyms #-}

-- | Defines the underlying data types used in the package extension.
module Language.Drasil.Code.FileData (FileAndContents(filePath, fileDoc),
  fileAndContents, fileDataToFileAndContents,
  PackageData(packageProg, packageAux), pattern PackageData
  ) where

import Text.PrettyPrint.HughesPJ (Doc, isEmpty)
import qualified Drasil.GOOL as G (FileData(..), ModData(..))

-- | The underlying data type for auxiliary files in all renderers.
data FileAndContents = FileAndContents {filePath :: FilePath, fileDoc :: Doc}

-- | Constructor for auxiliary files.
fileAndContents :: FilePath -> Doc -> FileAndContents
fileAndContents = FileAndContents

fileDataToFileAndContents :: G.FileData -> FileAndContents
fileDataToFileAndContents file = fileAndContents (G.filePath file) ((G.modDoc . G.fileMod) file)

-- | The underlying data type for packages in all renderers.
data PackageData a = PackD {packageProg :: a, packageAux :: [FileAndContents]}

pattern PackageData :: a -> [FileAndContents] -> PackageData a
pattern PackageData prog aux <- PackD prog aux
  where
    PackageData prog aux = PackD prog (filter (not . isEmpty . fileDoc) aux)
{-# COMPLETE PackageData #-}
