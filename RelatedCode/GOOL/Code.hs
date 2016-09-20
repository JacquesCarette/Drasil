-- | Defines the 'Code' data type
module GOOL.Code (
    Code(..)
    ) where

import Text.PrettyPrint.HughesPJ (Doc)

-- | Represents the generated code as a list of file names and rendered code pairs
data Code = Code [(FilePath, Doc)]
