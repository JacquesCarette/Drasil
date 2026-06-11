{-# LANGUAGE OverloadedStrings #-}

module Drasil.Data.Formats.JSON.Render
  ( -- ** Rendering
    JSONRenderOptions,
    JSONStyle(..),
    jsonRenderOpts,
    renderJSON,
  )
where

import Drasil.Data.Formats.JSON.Core (JSON(..))

import Data.Char (ord, isControl)
import Data.Scientific (formatScientific, isInteger, FPFormat(..))
import Data.Text (Text)
import qualified Data.Text as T (concatMap, pack, singleton)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, braces, brackets, colon, comma, dquotes, pretty,
  hcat, indent, lbrace, lbracket, punctuate, rbrace, rbracket, space, vcat)
import Text.Printf (printf)

-- | Options for rendering 'JSON'.
newtype JSONRenderOptions = JSONRO { style :: JSONStyle }

-- | JSON Style: How the overall JSON document is formatted.
data JSONStyle =
    -- | With as few characters as possible and no newlines.
    Minified
    -- | In a "pretty" human readable way, with given indent size.
  | Pretty Natural

-- | Create 'JSONRenderOptions'.
jsonRenderOpts :: JSONStyle -> JSONRenderOptions
jsonRenderOpts = JSONRO

-- | Render 'JSON' to a 'Doc' with the given options.
renderJSON :: JSONRenderOptions -> JSON -> Doc ann
renderJSON _ (JObject []) = lbrace <> rbrace
renderJSON opts (JObject m) =
  case sty of
    Minified -> braces (hcat content)
    Pretty i -> vcat [lbrace, indent (fromIntegral i) (vcat content), rbrace]
  where
    sty = style opts
    sep Minified = colon
    sep (Pretty _) = colon <> space
    contents = map (\(k, v) -> renderString k <> sep sty <> renderJSON opts v) m
    content = punctuate comma contents
renderJSON _ (JArray []) = lbracket <> rbracket
renderJSON opts (JArray a) =
  case style opts of
    Minified -> brackets (hcat content)
    Pretty i -> vcat [lbracket, indent (fromIntegral i) (vcat content), rbracket]
  where
    contents = map (renderJSON opts) a
    content = punctuate comma contents
renderJSON _ (JString s) = renderString s
renderJSON _ (JNumber n) =
  pretty $
    if isInteger n
      then formatScientific Fixed (Just 0) n -- Avoid inclding .0 for integers
      else show n
renderJSON _ (JBool True) = pretty ("true" :: Text)
renderJSON _ (JBool False) = pretty ("false" :: Text)
renderJSON _ JNull = pretty ("null" :: Text)

-- | Internal: Renders Text as a JSON string, with appropriate escaping.
renderString :: Text -> Doc ann
renderString s = dquotes $ pretty (T.concatMap escapeChar s)

-- | Internal: Escapes a character for encoding in JSON.
escapeChar :: Char -> Text
escapeChar '\"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '\b' = "\\b"
escapeChar '\f' = "\\f"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar c =
  if isControl c
    then T.pack $ printf "\\u%04X" (ord c)
    else T.singleton c
