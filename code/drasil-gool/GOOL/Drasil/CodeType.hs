-- | Defines the 'CodeType' data type
module GOOL.Drasil.CodeType (
    CodeType(..),
    ) where

type ClassName = String
type EnumName = String

data CodeType = Boolean
              | Integer -- Maps to 32-bit signed integer in all languages but
                        -- Python, where integers have unlimited precision
              | Float
              | Double
              | Char
              | String
              | File
              | List CodeType
              | Array CodeType
              | Iterator CodeType
              | Object ClassName
              -- | Enum EnumName
              | Func [CodeType] CodeType
              | Void deriving Eq
