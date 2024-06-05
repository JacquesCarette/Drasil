-- | Defines helper functions for creating jupyter notebooks.
module Language.Drasil.Markdown.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, empty, (<>), ($$), hcat)
import Data.List.Split (splitOn)

data Variation =  Id | Align

li, pa, ba, ul :: Doc -> Doc
-- | List tag wrapper
li         = wrap "li" []
-- | Paragraph in list tag wrapper
pa         = wrap "p" []
-- | Bring attention to element wrapper.
ba         = wrap "b" []
-- | Unordered list tag wrapper.
ul         = wrap "ul" []

bold :: Doc -> Doc
bold t = text "**" <> t <> text "**"

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen' hcat Id a empty

wrapGen' :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen' sepf _ s _ [] = \x ->
  let tb c = text $ "<" ++ c ++ ">"
  --in sepf [quote(tb s), x, quote(tb $ '/':s)]
  in sepf [tb s, x, tb $ '/':s]
wrapGen' sepf Align s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " align=\"") <> ti <> text "\">"
      te c = text $ "</" ++ c ++ ">"
  in  sepf [tb s, x, te s]
wrapGen' sepf Id s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " id=\"") <> ti <> text "\">"
      te c = text $ "</" ++ c ++ ">\n"
  in  sepf [tb s, x, te s]

refwrap :: Doc -> Doc -> Doc
refwrap = flip (wrapGen' hcat Id "div") [""]

sq :: Doc -> Doc
sq t = text "[" <> t <> text "]" 

paren :: Doc -> Doc
paren t = text "(" <> t <> text ")" 

-- | Helper for setting up links to references
reflink :: Doc -> Doc -> Doc
reflink ref txt = sq txt <> paren (text "#" <> ref)

-- | Helper for setting up links to references with additional information.
reflinkInfo :: Doc -> Doc -> Doc -> Doc
reflinkInfo rf txt info = reflink rf txt <> info

-- | Helper for setting up links to external URIs
reflinkURI :: Doc -> Doc -> Doc
reflinkURI ref txt = sq txt <> paren ref

-- | Helper for setting up figures
image :: Doc -> Doc -> Doc
image f c =  text "!" <> (reflinkURI f c) $$ bold (caption c) <> text "\n"

caption :: Doc -> Doc
caption = wrapGen' hcat Align "p" (text "center") [""]

h :: Int -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 4 = error "Illegal header (too large)"
          | n < 4 = text "# "
          | n == 4 = text "#### "
          | otherwise = text "Illegal header"

-- | Curly braces.
br :: Doc -> Doc
br x = text "{" <> x <> text "}"

-- Maybe use "lines" instead (Data.List @lines :: String -> [String])
stripnewLine :: String -> Doc
stripnewLine s = hcat (map text (splitOn "\n" s))

stripTabs :: Doc -> Doc
stripTabs d = hcat (map text (splitOn "\t" (show d)))

docLength :: Doc -> Int
docLength d = length $ show d
