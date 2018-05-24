{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Attribute.ShortName where

data ShortNm = ShortName String

-- Hack 
snToS :: ShortNm -> String
snToS (ShortName x) = x