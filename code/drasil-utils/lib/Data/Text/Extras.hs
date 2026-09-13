{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Data.Text.Extras (
  num2Text,
  wrap,
  paren,
  brak,
  brace
) where

import Data.Text (Text)
import TextShow

num2Text :: (TextShow i, Num i) => i -> Text
num2Text = showt

wrap :: Text -> Text -> Text -> Text
wrap l r e = l <> e <> r

paren :: Text -> Text
paren = wrap "(" ")"

brak :: Text -> Text
brak = wrap "[" "]"

brace :: Text -> Text
brace = wrap "{" "}"
