{-# LANGUAGE OverloadedStrings #-}
module Language.Drasil.HTML2.MathJax (
  mathJax3Url,
  inlineEqn, blockEqn
) where

import Data.Text (Text)
import Data.Text.Extras (wrap)

mathJax3Url :: String
mathJax3Url = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"

inlineEqn :: Text -> Text
inlineEqn = wrap "\\(" "\\)"

blockEqn :: Text -> Text
blockEqn = wrap "\\[" "\\]"
