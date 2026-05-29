{-# LANGUAGE PatternSynonyms #-}
module Language.Drasil.Code.PackageData (PackageData(packageProg, packageAux),
  pattern PackageData, package
) where

import Drasil.Build.Artifacts (FileLayout)
import Drasil.GOOL (ProgData, onCodeList)

-- | The underlying data type for packages in all renderers.
data PackageData = PackD {packageProg :: ProgData, packageAux :: [FileLayout]}

pattern PackageData :: ProgData -> [FileLayout] -> PackageData
pattern PackageData prog aux <- PackD prog aux
  where
    PackageData prog aux = PackD prog aux
{-# COMPLETE PackageData #-}

package :: (Monad r) => ProgData -> [r FileLayout] -> r PackageData
package p = onCodeList (PackageData p)
