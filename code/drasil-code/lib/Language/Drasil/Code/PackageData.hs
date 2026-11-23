module Language.Drasil.Code.PackageData (PackageData(..), package) where

import Drasil.FileHandling (FileLayout)
import Drasil.GOOL (ProgData, onCodeList)

-- | The underlying data type for packages in all renderers.
data PackageData = PackageData {
  packageProg :: ProgData,
  packageAux :: [FileLayout]
}

package :: (Monad r) => ProgData -> [r FileLayout] -> r PackageData
package p = onCodeList (PackageData p)
