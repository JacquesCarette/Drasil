{-# LANGUAGE OverloadedStrings #-}

-- | Defines helper functions for creating jupyter notebooks.
module Language.Drasil.JSON.Helpers (
  -- * Jupyter-related
  markdownCell, codeCell, makeMetadata,
  -- * HTML Tag Wrappers
  tr, td, th, bold, em, li, pa, ba, table,
  refwrap, refID, reflink, reflinkInfo, reflinkURI, image, h, mkDiv,
  stripnewLine
) where

import Prelude hiding ((<>))
import qualified Prelude as P ((<>))
import qualified Prelude
import Text.PrettyPrint (Doc, text, empty, (<>), (<+>), vcat, hcat, render)
import Data.Text (Text)
import qualified Data.Text as T (lines, pack)
import Data.List (intersperse)
import Data.List.Split (splitOn)

import Drasil.Data.Formats.JSON (JSON(..))
import Language.Drasil.Document (MaxWidthPercent)

import Drasil.Printers.Common hiding (wrap)
import Language.Drasil.Printing.Helpers (bslash)

data Variation = Class | Id

tr, td, th, bold, em, figure, li, pa, ba :: Doc -> Doc
-- | Table row tag wrapper
tr         = wrap "tr" []
-- | Table cell tag wrapper
td         = wrap "td" []
-- | Table header tag wrapper
th         = wrap' "th" []
-- | Bold tag wrapper
bold       = wrap' "b" []
-- | Emphasis (italics) tag wrapper
em         = wrap' "em" []
-- | Figure tag wrapper
figure     = wrap "figure" []
-- | List tag wrapper
li         = wrap' "li" []
-- | Paragraph in list tag wrapper
pa         = wrap "p" []
-- | Bring attention to element wrapper.
ba         = wrap "b" []

-- | Table tag wrapper
table :: [String] -> Doc -> Doc
table = wrap "table"

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen' vcat Class a empty

wrap' :: String -> [String] -> Doc -> Doc
wrap' a = wrapGen' hcat Class a empty

wrapGen' :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen' sepf _ s _ [] = \x ->
  let tb = angbrac . text
  in sepf [tb s, x, tb $ '/':s]
wrapGen' sepf Class s _ ts = \x ->
  let tb c = text $ "<" P.<> c P.<> " class=\\\"" P.<> foldr1 (++) (intersperse " " ts) P.<> "\\\">"
  in let te c = text $ "</" P.<> c P.<> ">"
  in sepf [tb s, x, te s]
wrapGen' sepf Id s ti _ = \x ->
  let tb c = text ("<" P.<> c P.<> " id=\\\"") <> ti <> text "\\\">"
      te c = text $ "</" P.<> c P.<> ">"
  in  sepf [tb s, x, te s]

refwrap :: Doc -> Doc -> Doc
refwrap = flip (wrapGen' vcat Id "div") [""]

refID :: Doc -> Doc
refID i = text "<a id=\"" <> i <> text "\"></a>"

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text "[" <> txt <> text ("](#" P.<> ref P.<> ")")

-- | Helper for setting up links to references with additional information.
reflinkInfo :: String -> Doc -> Doc -> Doc
reflinkInfo rf txt info = text ("<a href=\"#" P.<> rf P.<> "\">") <> txt <> text "</a>" <+> info

-- | Helper for setting up links to external URIs
reflinkURI :: String -> Doc -> Doc
reflinkURI ref txt = text ("<a href=\\\"" P.<> ref P.<> "\\\">") <> txt <> text "</a>"

-- | Helper for wrapping attributes in a tag.
--
--     * The first argument is tag name.
--     * The 'String' in the pair is the attribute name,
--     * The 'Doc' is the value for different attributes.
wrapInside :: String -> [(String, Doc)] -> Doc
wrapInside t p = text ("<" P.<> t P.<> " ") <> foldl1 (<>) (fmap foldStr p) <> text ">"
  where foldStr (attr, val) = text (attr P.<> "=\"") <> val <> text "\" "

-- | Image tag wrapper.
img :: [(String, Doc)] -> Doc
img = wrapInside "img"

-- | Helper for setting up figures.
image :: Doc -> Maybe Doc -> MaxWidthPercent -> Doc
image f Nothing wp =
  figure $ vcat [
  img $ [("src", f), ("alt", text "")] P.<> [("width", text $ show wp P.<> "%") | wp /= 100]]
image f (Just c) wp =
  figure $ vcat [
  img $ [("src", f), ("alt", c)] P.<> [("width", text $ show wp P.<> "%") | wp /= 100]]

h :: Int -> Doc
h n | n < 1 = error "Illegal header (too small)"
    | n > 6 = error "Illegal header (too large)"
    | otherwise = text (replicate n '#' P.<> " ")

mkDiv :: String -> Doc -> Doc -> Doc
mkDiv s a0 a1 = (bslash <> text s) <> brace a0 <> brace a1

-- Maybe use "lines" instead (Data.List @lines :: String -> [String])
stripnewLine :: String -> Doc
stripnewLine s = hcat (fmap text (splitOn "\n" s))

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
  in JArray $ fmap (JString . (Prelude.<> "\n")) t'

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
