-- | Defines an enumerated type of target languages for code generation.
module Language.Drasil.Code.Lang (Lang(..)) where

-- FIXME: Move to 'Language.Drasil.Choices'.
--
-- | Various OO languages where code may be generated.
data Lang = Cpp
          | CSharp
          | Java
          | Python
          | Swift
          | Julia
          deriving (Eq, Show)
