{-# LANGUAGE PatternSynonyms #-}
module Language.Drasil.Code.PackageData (PackageData(packageProg, packageAux),
  pattern PackageData, package
) where

import Language.Drasil.Code.FileData (FileAndContents(..))
import Text.PrettyPrint.HughesPJ (isEmpty)
import Drasil.GOOL (ProgData, onCodeList)

-- | The underlying data type for packages in all renderers.
data PackageData = PackD {packageProg :: ProgData, packageAux :: [FileAndContents]}

pattern PackageData :: ProgData -> [FileAndContents] -> PackageData
pattern PackageData prog aux <- PackD prog aux
  where
    PackageData prog aux = PackD prog (filter (not . isEmpty . fileDoc) aux)
{-# COMPLETE PackageData #-}

package :: (Monad r) => ProgData -> [r FileAndContents] -> r PackageData
package p = onCodeList (PackageData p)
