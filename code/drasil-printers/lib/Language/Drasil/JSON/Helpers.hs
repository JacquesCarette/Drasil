{-# LANGUAGE OverloadedStrings #-}

-- | Defines helper functions for creating jupyter notebooks.
module Language.Drasil.JSON.Helpers (
  -- * Types
  Variation(..),
  -- * Jupyter-related
  markdownCell, codeCell, makeMetadata,
  -- * HTML Tag Wrappers
  tr, td, figure, li, pa, ba, ol, ul, table,
  wrap, wrap', refwrap, refID, reflink, reflinkURI, image, h, br, mkDiv,
  stripnewLine,
) where

import Prelude hiding ((<>))
import qualified Prelude
import Text.PrettyPrint (Doc, text, empty, (<>), vcat, hcat, render)
import Data.Text (Text)
import qualified Data.Text as T (lines, pack)
import Data.List (intersperse)
import Data.List.Split (splitOn)

import Drasil.Data.Formats.JSON (JSON(..))
import Language.Drasil.Document (MaxWidthPercent)
import Language.Drasil.HTML.Helpers (img)
import Language.Drasil.Printing.Helpers (bslash)

data Variation = Class | Id

tr, td, figure, li, pa, ba :: Doc -> Doc
-- | Table row tag wrapper
tr         = wrap "tr" []
-- | Table cell tag wrapper
td         = wrap "td" []
-- | Figure tag wrapper
figure     = wrap "figure" []
-- | List tag wrapper
li         = wrap' "li" []
-- | Paragraph in list tag wrapper
pa         = wrap "p" []
-- | Bring attention to element wrapper.
ba         = wrap "b" []

ol, ul, table :: [String] -> Doc -> Doc
-- | Ordered list tag wrapper
ol       = wrap "ol"
-- | Unordered list tag wrapper
ul       = wrap "ul"
-- | Table tag wrapper
table    = wrap "table"

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen' vcat Class a empty

wrap' :: String -> [String] -> Doc -> Doc
wrap' a = wrapGen' hcat Class a empty

wrapGen' :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen' sepf _ s _ [] = \x ->
  let tb c = text $ "<" ++ c ++ ">"
  --in sepf [quote(tb s), x, quote(tb $ '/':s)]
  in sepf [tb s, x, tb $ '/':s]
wrapGen' sepf Class s _ ts = \x ->
  let tb c = text $ "<" ++ c ++ " class=\\\"" ++ foldr1 (++) (intersperse " " ts) ++ "\\\">"
  in let te c = text $ "</" ++ c ++ ">"
  in sepf [tb s, x, te s]
wrapGen' sepf Id s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " id=\\\"") <> ti <> text "\\\">"
      te c = text $ "</" ++ c ++ ">"
  in  sepf [tb s, x, te s]

refwrap :: Doc -> Doc -> Doc
refwrap = flip (wrapGen' vcat Id "div") [""]

refID :: Doc -> Doc
refID i = text "<a id=\"" <> i <> text "\"></a>"

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text "[" <> txt <> text ("](#" ++ ref ++ ")")
--reflink ref txt = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>"

-- | Helper for setting up links to external URIs
reflinkURI :: String -> Doc -> Doc
reflinkURI ref txt = text ("<a href=\\\"" ++ ref ++ "\\\">") <> txt <> text "</a>"

-- | Helper for setting up figures.
image :: Doc -> Maybe Doc -> MaxWidthPercent -> Doc
image f Nothing wp =
  figure $ vcat [
  img $ [("src", f), ("alt", text "")] ++ [("width", text $ show wp ++ "%") | wp /= 100]]
image f (Just c) wp =
  figure $ vcat [
  img $ [("src", f), ("alt", c)] ++ [("width", text $ show wp ++ "%") | wp /= 100]]

h :: Int -> Doc
h n | n < 1 = error "Illegal header (too small)"
    | n > 6 = error "Illegal header (too large)"
    | otherwise = text (replicate n '#' ++ " ")

-- | Curly braces.
br :: Doc -> Doc
br x = text "{" <> x <> text "}"

mkDiv :: String -> Doc -> Doc -> Doc
mkDiv s a0 a1 = (bslash <> text s) <> br a0 <> br a1

-- Maybe use "lines" instead (Data.List @lines :: String -> [String])
stripnewLine :: String -> Doc
stripnewLine s = hcat (map text (splitOn "\n" s))

-- | Construct a Jupyter markdown cell with the given content.
markdownCell :: Doc -> JSON
markdownCell d =
  JObject
  [ ("cell_type", "markdown"),
    ("metadata", JObject []),
    ("source", formatSource d)
  ]

-- | Construct a Jupyter code cell with the given content.
codeCell :: Doc -> JSON
codeCell d =
  JObject
  [ ("cell_type", "code"),
    ("execution_count", JNull),
    ("metadata", JObject []),
    ("outputs", JArray []),
    ("source", formatSource d)
  ]

-- | Renders a Doc to a JSON array for using in a Juptyer cell's
-- 'source' attribute.
formatSource :: Doc -> JSON
formatSource d =
  let
    d' = render d
    t = T.pack d'
    t' = T.lines t
  in JArray $ map (JString . (Prelude.<> "\n")) t'

-- | Generate the metadata necessary for a notebook document.
makeMetadata :: [(Text, JSON)]
makeMetadata =
  [ ("metadata",
      JObject
      [ ("kernelspec",
          JObject
          [ ("display_name", "Python 3"),
            ("language", "python"),
            ("name", "python3")
          ]
        ),
        ("language_info",
          JObject
          [ ("codemirror_mode",
              JObject [("name", "ipython"), ("version", JNumber 3)]),
            ("file_extension", ".py"),
            ("mimetype", "text/x-python"),
            ("name", "python"),
            ("nbconvert_exporter", "python"),
            ("pygments_lexer", "ipython3"),
            ("version", "3.9.1")
          ]
        )
      ]
    ),
    ("nbformat", JNumber 4),
    ("nbformat_minor", JNumber 4)
  ]
