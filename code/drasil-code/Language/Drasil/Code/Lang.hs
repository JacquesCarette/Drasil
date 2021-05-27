-- | Defines an enumerated type of target languages for code generation.
module Language.Drasil.Code.Lang (Lang(..)) where

data Lang = Cpp
          | CSharp
          | Java
          | Python
          | Swift
          deriving (Eq, Show)