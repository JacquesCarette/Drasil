-- | Defines the 'CodeType' data type
module GOOL.Drasil.CodeType (
    ClassName, CodeType(..)
    ) where

type ClassName = String

data CodeType = Boolean
              | Integer -- Maps to 32-bit signed integer in all languages but
                        -- Python, where integers have unlimited precision
              | Float
              | Double
              | Char
              | String
              | InFile
              | OutFile
              | List CodeType
              | Array CodeType
              | Object ClassName
              | Func [CodeType] CodeType
              | Void deriving (Eq, Show)
