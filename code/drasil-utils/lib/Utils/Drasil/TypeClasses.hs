module Utils.Drasil.TypeClasses (
  HasPathAndDoc(..)
) where

import Text.PrettyPrint.HughesPJ (Doc)

class HasPathAndDoc a where
  getPath :: a -> FilePath
  getDoc :: a -> Doc
