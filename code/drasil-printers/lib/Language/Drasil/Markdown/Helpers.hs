-- | Defines helper functions for creating Markdown files.
module Language.Drasil.Markdown.Helpers where

import Prelude hiding ((<>), lookup)
import System.FilePath (takeFileName)
import Text.PrettyPrint (Doc, text, empty, (<>), (<+>), hcat,
  brackets, parens, braces)
import Data.Map (lookup)
import Language.Drasil.Printing.Helpers (ast, ($^$), vsep)
import Language.Drasil.Printing.LayoutObj (RefMap)
import Language.Drasil.HTML.Helpers (wrap', wrapGen', Variation(Id, Align))

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
li = wrap' "li" []
-- | Unordered list tag wrapper.
ul = wrap' "ul" []

-- | Helper for setting up section div
divTag :: Doc -> Doc
divTag l = wrapGen' hcat Id "div" l [""] empty

-- | Helper for setting up div for defn heading
defnHTag :: Doc -> Doc
defnHTag = wrapGen' vsep Align "div" (text "center") [""]

-- | Helper for setting up links to references
reflink :: RefMap -> String -> Doc -> Doc
reflink rm ref txt = brackets txt <> parens rp
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
  else brackets txt <> parens ref

-- | Helper for setting up figures
image :: Doc -> Doc -> Doc
image f c =  text "!" <> reflinkURI fp c $^$ bold (caption c)
  where
    fp = text $ "/assets/" ++ takeFileName (show f)

-- | Helper for setting up captions
caption :: Doc -> Doc
caption = wrapGen' hcat Align "p" (text "center") [""]

-- | Helper for setting up headings with an id attribute.
-- id attribute will only work for mdBook.
heading ::  Doc -> Doc -> Doc
heading t l = t <+> braces (text "#" <> l)

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
