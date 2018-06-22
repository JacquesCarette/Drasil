{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.ShortName where

import Control.Lens (Lens')

--It is a hack to think of ShortName as a String
newtype ShortName = ShortNm String

class HasShortName  s where
  shortname :: Lens' s ShortName-- String; The text to be displayed for the link.
                            -- A short name used for referencing within a document that can 
                            -- include symbols and whatnot if required.
                            -- Visible in the typeset documents (pdf)

shortname' :: String -> ShortName
shortname' = ShortNm
