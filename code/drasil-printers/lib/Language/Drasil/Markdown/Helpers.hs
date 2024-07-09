-- | Defines helper functions for creating Markdown files.
module Language.Drasil.Markdown.Helpers where

import Prelude hiding ((<>), lookup)
import Text.PrettyPrint (Doc, text, empty, (<>), (<+>), ($$), hcat)
import Data.List (foldl')
import Data.Map (lookup)
import Language.Drasil.Printing.Helpers (ast)
import Language.Drasil.Printing.LayoutObj (RefMap)

data Variation =  Id | Align | None

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

-- | Custom infix operator for concatenating 
-- two Docs with an empty line in between
infixl 5 $^$
($^$) :: Doc -> Doc -> Doc
($^$) a b = a $$ text "" $$ b

-- | Concatenate a list of 'Doc's with an 
-- empty line in between
vcatnl :: [Doc] -> Doc
vcatnl []     = empty
vcatnl (l:ls) = foldl' ($^$) l ls

li, ul :: Doc -> Doc
-- | List tag wrapper
li = wrap "li"
-- | Unordered list tag wrapper.
ul = wrap "ul"

-- | Helper for wrapping HTML lists
wrap :: String -> Doc -> Doc
wrap a = wrapGen hcat None a empty

-- | Helper for setting up HTML tags
-- ([Doc] -> Doc): Doc concatenation function.
-- Variation: HTML tag attribute.
-- String: HTML tag.
-- Doc: HTML tag attribute value.
-- Doc: Content being wrapped.
wrapGen :: ([Doc] -> Doc) -> Variation -> String -> Doc -> Doc -> Doc
wrapGen sepf None s _ = \x ->
  let tb c = text $ "<" ++ c ++ ">"
  in sepf [tb s, x, tb $ '/':s]
wrapGen sepf Align s ti = \x ->
  let tb c = text ("<" ++ c ++ " align=\"") <> ti <> text "\">"
      te c = text $ "</" ++ c ++ ">"
  in  sepf [tb s, x, te s]
wrapGen sepf Id s ti = \x ->
  let tb c = text ("<" ++ c ++ " id=\"") <> ti <> text "\">"
      te c = text $ "</" ++ c ++ ">"
  in  sepf [tb s, x, te s]

-- | Helper for setting up section div
divTag :: Doc -> Doc
divTag l = wrapGen hcat Id "div" l empty

-- | Helper for setting up div for defn heading
defnHTag :: Doc -> Doc
defnHTag = wrapGen vcatnl Align "div" (text "center")

-- | Helper for setting up links to references
reflink :: RefMap -> String -> Doc -> Doc
reflink rm ref txt = sq txt <> paren rp
  where
    fn = maybe empty fp (lookup ref rm)
    fp s = text $ "./" ++ s ++ ".md"
    rp = fn <> text ("#" ++ ref)

-- | Helper for setting up links to references with additional information.
reflinkInfo :: RefMap -> String -> Doc -> Doc -> Doc
reflinkInfo rm rf txt info = reflink rm rf txt <+> info

-- | Helper for setting up links to external URIs
reflinkURI :: Doc -> Doc -> Doc
reflinkURI ref txt = if ref == txt then ang ref
  else sq txt <> paren ref

-- | Helper for setting up figures
image :: Doc -> Doc -> Doc
image f c =  text "!" <> reflinkURI f c $^$ bold (caption c)

-- | Helper for setting up captions
caption :: Doc -> Doc
caption = wrapGen hcat Align "p" (text "center")

-- | Helper for setting up headings with an id attribute.
-- id attribute will only work for mdBook.
heading ::  Doc -> Doc -> Doc
heading t l = t <+> br (text "#" <> l)

-- | Helper for setting up heading weights in mdBook.
h :: Int -> Doc
h n
  | n < 1     = error "Illegal header (header weight must be > 0)."
  | n > 7     = error "Illegal header (header weight must be < 8)"
  | n < 4     = h' 1
  | otherwise = h' n

-- | Helper for setting up heading weights in normal Markdown.
h' :: Int -> Doc
h' n
  | n < 1 = error "Illegal header (header weight must be > 0)."
  | n > 7 = error "Illegal header (header weight must be < 8)."
  | otherwise = text $ replicate n '#'

-- | Helper for getting length of a Doc
docLength :: Doc -> Int
docLength d = length $ show d
