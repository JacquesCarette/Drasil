-- | Defines the 'CodeType' data type
module GOOL.Drasil.CodeType (
    ClassName, CodeType(..)
    ) where

type ClassName = String
-- type EnumName = String

data CodeType = Boolean
              | Integer
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
