-- | Defines the 'CodeType' data type
module GOOL.Drasil.CodeType (
    CodeType(..),
    isObject
    ) where

data CodeType = Boolean
              | Integer
              | Float
              | Char
              | String
              | File
              | List CodeType
              | Iterator CodeType
              | Object String
              | Enum String
              | Func [CodeType] CodeType
              | Void deriving Eq

isObject :: CodeType -> Bool
isObject (Object _) = True
isObject _ = False
