-- | Defines the 'Code' data type
module Language.Drasil.Code.Code (
    Code(..),
    CodeType(..)
    ) where

import Text.PrettyPrint.HughesPJ (Doc)

-- | Represents the generated code as a list of file names and rendered code pairs
newtype Code = Code { unCode :: [(FilePath, Doc)]}

data CodeType = Boolean
              | Integer
              | Float
              | Char
              | String
              | File
              | List CodeType
              | Object String
