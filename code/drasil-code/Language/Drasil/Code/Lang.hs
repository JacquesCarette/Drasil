module Language.Drasil.Code.Lang (Lang(..)) where

data Lang = Cpp
          | CSharp
          | Java
          | Python
          deriving (Eq, Show)