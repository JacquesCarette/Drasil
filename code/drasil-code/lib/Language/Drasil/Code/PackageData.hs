module Language.Drasil.Code.PackageData (PackageData(packageProg, packageAux),
  package
) where

import Language.Drasil.Code.FileData (FileAndContents(..))
import Text.PrettyPrint.HughesPJ (isEmpty)
import Drasil.GOOL (ProgData, onCodeList)

-- | The underlying data type for packages in all renderers.
data PackageData a = PackD {packageProg :: a, packageAux :: [FileAndContents]}

-- | Constructor for package data.
packageData :: a -> [FileAndContents] -> PackageData a
packageData p as = PackD p (filter (not . isEmpty . fileDoc) as)

package :: (Monad r) => ProgData -> [r FileAndContents] -> r (PackageData ProgData)
package p = onCodeList (packageData p)
