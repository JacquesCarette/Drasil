-- | Defines the 'Code' data type
module Language.Drasil.Code.Code (
    Code(..)
    ) where

import Text.PrettyPrint.HughesPJ (Doc)

-- | Represents the generated code as a list of file names and rendered code pairs
newtype Code = Code [(FilePath, Doc)]
