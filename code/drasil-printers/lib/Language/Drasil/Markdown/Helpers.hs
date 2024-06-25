-- | Defines helper functions for creating Markdown files.
module Language.Drasil.Markdown.Helpers where

import Prelude hiding ((<>), lookup)
import Text.PrettyPrint (Doc, text, empty, (<>), (<+>), ($$), hcat, vcat)
import Data.List.Split (splitOn)
import Data.Map (lookup)
import Language.Drasil.Printing.Helpers (ast)
import Language.Drasil.Printing.LayoutObj (RefMap)

data Variation =  Id | Align

-- | Curly braces.
br :: Doc -> Doc
br x = text "{" <> x <> text "}"

-- | Square brackets
sq :: Doc -> Doc
sq t = text "[" <> t <> text "]" 

-- | Parenthesis
paren :: Doc -> Doc
paren t = text "(" <> t <> text ")" 

-- | Angled brackets
ang :: Doc -> Doc
ang t = text "<" <> t <> text ">"

-- | Bold text
bold :: Doc -> Doc
bold t = ast <> ast <> t <> ast <> ast

-- | Italicized text
em :: Doc -> Doc
em t = ast <> t <> ast

li, ul :: Doc -> Doc
-- | List tag wrapper
li = wrap "li" []
-- | Unordered list tag wrapper.
ul = wrap "ul" []

-- | helper for wrapping HTML lists
wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen hcat Id a empty

-- | Helper for setting up HTML tags
wrapGen :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen sepf _ s _ [] = \x ->
  let tb c = text $ "<" ++ c ++ ">"
  in sepf [tb s, x, tb $ '/':s]
wrapGen sepf Align s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " align=\"") <> ti <> text "\">"
      te c = text $ "</" ++ c ++ ">"
  in  sepf [tb s, x, te s]
wrapGen sepf Id s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " id=\"") <> ti <> text "\">"
      te c = text $ "</" ++ c ++ ">\n"
  in  sepf [tb s, x, te s]

-- | Helper for setting up section div
divTag :: Doc -> Doc
divTag l = wrapGen hcat Id "div" l [""] empty

-- | Helper for setting up div for defn heading
defnHTag :: Doc -> Doc
defnHTag = wrapGen vcat Align "div" (text "center") [""]

-- | Helper for setting up links to references
reflink :: RefMap -> String -> Doc -> Doc
reflink rm ref txt = sq txt <> paren rp
  where
    fn = maybe empty fp (lookup ref rm)
    fp s = text $ "./" ++ s ++ ".md"
    rp = fn <> (text $ "#" ++ ref)

-- | Helper for setting up links to references with additional information.
reflinkInfo :: RefMap -> String -> Doc -> Doc -> Doc
reflinkInfo rm rf txt info = reflink rm rf txt <+> info

-- | Helper for setting up links to external URIs
reflinkURI :: Doc -> Doc -> Doc
reflinkURI ref txt = if ref==txt then ang ref
  else sq txt <> paren ref

-- | Helper for setting up figures
image :: Doc -> Doc -> Doc
image f c =  text "!" <> reflinkURI f c $$ bold (caption c) <> text "\n"

-- | Helper for setting up captions
caption :: Doc -> Doc
caption = wrapGen hcat Align "p" (text "center") [""]

-- | Helper for setting up headings
heading :: Int -> Doc -> Doc -> Doc
heading d t l = h d <+> t <+> (br $ text "#" <> l)
  where 
    h n
      | n < 1     = error "Illegal header (too small)"
      | n > 4     = error "Illegal header (too large)"
      | n < 4     = text "#"
      | otherwise = text "####"

-- | Helper for stripping Docs
stripStr :: Doc -> Doc -> Doc
stripStr d s = hcat (map text (splitOn (show s) (show d)))

-- | Helper for getting length of a Doc
docLength :: Doc -> Int
docLength d = length $ show d
