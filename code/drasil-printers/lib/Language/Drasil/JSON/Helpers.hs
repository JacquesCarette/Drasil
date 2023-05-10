-- | Defines helper functions for creating jupyter notebooks.
module Language.Drasil.JSON.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, empty, (<>), vcat, hcat)
import Data.List (intersperse)
import Data.List.Split (splitOn)

import Language.Drasil (MaxWidthPercent)
import qualified Language.Drasil.Printing.Helpers as H
import Language.Drasil.HTML.Helpers (img)
import Numeric (showHex)

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

ba         = wrap "b" []

ol, ul, table :: [String] -> Doc -> Doc
-- | Ordered list tag wrapper
ol       = wrap "ol"
-- | Unordered list tag wrapper
ul       = wrap "ul"
-- | Table tag wrapper
table    = wrap "table"

-- FIXME: Why are we using a Doc if we use 'show'?
nbformat :: Doc -> Doc
nbformat s = text ("    \"" ++ escapeStringForJson (show s) ++ "\\n\",")

-- TODO: This replaces the `json` library import, and is modelled after it:
-- https://www.stackage.org/haddock/lts-20.20/json-0.10/src/Text.JSON.String.html#encJSString
-- However, we should ultimately replace this with our own JSON encoding with
-- more printing options and using `Doc`s appropriately. Alternatively, if we
-- prefer to use a premade library, Aeson has more features, is actively
-- developed, and is already compiled by our other dependencies.
escapeStringForJson :: String -> String
escapeStringForJson = concatMap (either id ('\\' :) . special)
  where
    special :: Char -> Either String String
    special c
      | c `elem` "\"\\"       = Right [c]
      | c `elem` "\b\f\n\r\t" = Right (show [c])
      -- note: the below double quotes disregard the point of ShowS, but that
      -- shouldn't be an issue if we switch to using `Doc`s.
      | c < '\x20'            = Right (showHex (fromEnum c) "")
      | otherwise             = Left [c]

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen' vcat Class a empty

wrap' :: String -> [String] -> Doc -> Doc
wrap' a = wrapGen' hcat Class a empty

wrapGen' :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen' sepf _ s _ [] = \x ->
  let tb c = text $ "<" ++ c ++ ">"
  --in sepf [quote(tb s), x, quote(tb $ '/':s)]
  in if s == "li" then sepf [tb s, x, tb $ '/':s] else sepf [nbformat (tb s), x, nbformat (tb $ '/':s)]
wrapGen' sepf Class s _ ts = \x ->
  let tb c = text $ "<" ++ c ++ " class=\\\"" ++ foldr1 (++) (intersperse " " ts) ++ "\\\">"
  in let te c = text $ "</" ++ c ++ ">"
  in sepf [nbformat (tb s), x, nbformat (te s)]
wrapGen' sepf Id s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " id=\\\"") <> ti <> text "\\\">"
      te c = text $ "</" ++ c ++ ">"
  in  sepf [nbformat (tb s), x, nbformat (te s)]

refwrap :: Doc -> Doc -> Doc
refwrap = flip (wrapGen' vcat Id "div") [""]

refID :: Doc -> Doc
refID i = nbformat $ text "<a id=\"" <> i <> text "\"></a>"

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text "[" <> txt <> text ("](#" ++ ref ++ ")")
--reflink ref txt = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>"

-- | Helper for setting up links to external URIs
reflinkURI :: String -> Doc -> Doc
reflinkURI ref txt = text ("<a href=\\\"" ++ ref ++ "\\\">") <> txt <> text "</a>"

-- | Helper for setting up figures
image :: Doc -> Doc -> MaxWidthPercent -> Doc
image f c 100 =
  figure $ vcat [
  nbformat $ img [("src", f), ("alt", c)]]
image f c wp =
  figure $ vcat [
  nbformat $ img [("src", f), ("alt", c), ("width", text $ show wp ++ "%")]]

h :: Int -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 4 = error "Illegal header (too large)"
          | otherwise = text (hash n)
              where hash 1 = "# "
                    hash 2 = "## "
                    hash 3 = "### "
                    hash 4 = "#### "
                    hash _ = "Illegal header"

{- Will delete it after checking encode works well
-- JSON formatter
formatter :: String -> String
formatter ('"':xs) = '\\' : '"' : formatter xs
formatter ('\\':xs) = '\\' : '\\' : formatter xs
formatter (x:xs) = x: formatter xs
formatter [] = []

jf :: String -> Doc
jf s = text $ replace "`" "\'" (formatter s)
-}

br :: Doc -> Doc
-- | Curly braces.
br x = text "{" <> x <> text "}"

mkDiv :: String -> Doc -> Doc -> Doc
mkDiv s a0 a1 = (H.bslash <> text s) <> br a0 <> br a1

-- Maybe use "lines" instead (Data.List @lines :: String -> [String])
stripnewLine :: String -> Doc
stripnewLine s = hcat (map text (splitOn "\n" s))
--filter (`notElem` "\n" )

makeMetadata :: Doc
makeMetadata = vcat [
  text " \"metadata\": {",
  vcat [
    text "  \"kernelspec\": {",
    text "   \"display_name\": \"Python 3\",",
    text "   \"language\": \"python\",",
    text "   \"name\": \"python3\"",
    text "  },"],
  vcat [
    text "  \"language_info\": {",
    text "   \"codemirror_mode\": {",
    text "    \"name\": \"ipython\",",
    text "    \"version\": 3",
    text "   },"],
  text "   \"file_extension\": \".py\",",
  text "   \"mimetype\": \"text/x-python\",",
  text "   \"name\": \"python\",",
  text "   \"nbconvert_exporter\": \"python\",",
  text "   \"pygments_lexer\": \"ipython3\",",
  text "   \"version\": \"3.9.1\"",
  text "  }",
  text " },",
  text " \"nbformat\": 4,",
  text " \"nbformat_minor\": 4"
  ]
