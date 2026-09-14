-- | Helper functions for HTML and Markdown printers (specifically, HTML tag wrappers).
module Language.Drasil.HTML.Helpers (
  -- * Types
  BibFormatter(..), Variation(..),
  -- * Tag Wrappers
  th, bold, em, sub, sup, spanTag', img,
  -- * Wrapping Combinators
  wrap', wrapGen', wrapInside, tagL, tagR,
  -- * References
  reflink, reflinkInfo, reflinkURI
) where
import Prelude hiding ((<>))
import Data.List (intersperse)
import Text.PrettyPrint (Doc, text, empty, (<>), (<+>), hcat, nest)

import Language.Drasil.Printing.AST (Spec)

-- | Data type that carries functions that vary
-- for bib printing
data BibFormatter = BibFormatter {
  -- | Emphasis (italics) rendering
  emph :: Doc -> Doc,
  -- | Spec rendering
  spec :: Spec -> Doc
}

-- | Table header tag wrapper.
th :: Doc -> Doc
th = wrap' "th" []

-- | Image tag wrapper.
img :: [(String, Doc)] -> Doc
img = wrapInside "img"

-- | HTML attribute selector.
data Variation = Class | Id | Align | Title deriving Eq

instance Show Variation where
  show Class = "class"
  show Id    = "id"
  show Align = "align"
  show Title = "title"

-- | General wrapper function and formats the document space with 'hcat'.
wrap' :: String -> [String] -> Doc -> Doc
wrap' a = wrapGen' hcat Class a empty

-- | Helper for wrapping HTML tags.
-- The fourth argument provides class names for the CSS.
wrapGen' :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen' sepf _ s _ [] = \x ->
  sepf [text $ "<" ++ s ++ ">", indent x, tagR s]
wrapGen' sepf Class s _ ts = \x ->
  let val = text $ foldr1 (++) (intersperse " " ts)
  in sepf [tagL s Class val, indent x, tagR s]
wrapGen' sepf v s ti _ = \x ->
  let con = if v == Align then x else indent x
  in sepf [tagL s v ti, con, tagR s]

-- | Helper for creating a left HTML tag with a single attribute.
tagL :: String -> Variation -> Doc -> Doc
tagL t a v = text ("<" ++ t ++ " " ++ show a ++ "=\"") <> v <> text "\">"

-- | Helper for creating a right HTML closing tag.
tagR :: String -> Doc
tagR t = text $ "</" ++ t ++ ">"

-- | Helper for wrapping attributes in a tag.
--
--     * The first argument is tag name.
--     * The 'String' in the pair is the attribute name,
--     * The 'Doc' is the value for different attributes.
wrapInside :: String -> [(String, Doc)] -> Doc
wrapInside t p = text ("<" ++ t ++ " ") <> foldl1 (<>) (map foldStr p) <> text ">"
  where foldStr (attr, val) = text (attr ++ "=\"") <> val <> text "\" "


-- | Helper for setting up links to references.
reflink :: String -> Doc -> Doc
reflink rf txt = text ("<a href=\"#" ++ rf ++ "\">") <> txt <> text "</a>"

-- | Helper for setting up links to references with additional information.
reflinkInfo :: String -> Doc -> Doc -> Doc
reflinkInfo rf txt info = text ("<a href=\"#" ++ rf ++ "\">") <> txt <> text "</a>" <+> info

-- | Helper for setting up links to external URIs.
reflinkURI :: String -> Doc -> Doc
reflinkURI rf txt = text ("<a href=\"" ++ rf ++ "\">") <> txt <> text "</a>"


em, sup, sub, bold :: Doc -> Doc
-- | Emphasis (italics) tag.
em = wrap' "em" []
-- | Superscript tag.
sup = wrap' "sup" []
-- | Subscript tag.
sub = wrap' "sub" []
-- | Bold tag.
bold = wrap' "b" []


-- | Span tag wrapper with a title attribute.
spanTag' :: Doc -> Doc -> Doc
spanTag' t = wrapGen' hcat Title "span" t [""]

-- | Indent the Document by 2 positions.
indent :: Doc -> Doc
indent = nest 2
