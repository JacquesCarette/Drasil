{-# LANGUAGE OverloadedStrings #-}
module Language.Drasil.HTML2.MathJax (
  mathJax3Url, mathJaxScript,
  inlineEqn, blockEqn
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Extras (wrap)

import Drasil.Data.Formats.JSON (renderJSON, jsonRenderOpts, JSONStyle(..), JSON(..))

mathJax3Url :: Text
mathJax3Url = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"

-- | Variable to include MathJax in our HTML files so we can render equations in LaTeX.
mathJaxScript :: Text
mathJaxScript = "MathJax = " <> configJSON <> ";"
  where
    configJSON = T.pack $ show $ renderJSON
      (jsonRenderOpts (Pretty 2))
      ( JObject [
        ("loader",
        JObject [("load", JArray ["[tex]/textmacros", "output/chtml"])]),
        ("tex", JObject [("packages", JObject [("[+]", JArray ["textmacros"])])]),
        ("svg", JObject [("fontCache", "global")])]
      )

inlineEqn :: Text -> Text
inlineEqn = wrap "\\(" "\\)"

blockEqn :: Text -> Text
blockEqn = wrap "\\[" "\\]"
