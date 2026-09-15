{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Extras (
  wrap,
  paren,
  brak,
  brace
) where

import Data.Text (Text)

wrap :: Text -> Text -> Text -> Text
wrap l r e = l <> e <> r

paren :: Text -> Text
paren = wrap "(" ")"

brak :: Text -> Text
brak = wrap "[" "]"

brace :: Text -> Text
brace = wrap "{" "}"
