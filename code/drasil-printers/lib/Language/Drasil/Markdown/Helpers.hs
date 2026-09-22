-- | Defines helper functions for creating Markdown files.
module Language.Drasil.Markdown.Helpers (
  bold, em, li, ul, divTag, centeredDiv, centeredDivId,
  reflink, reflinkInfo, reflinkURI, image, caption, heading, h, h',
  docLength
) where

import Prelude hiding ((<>), lookup)
import Data.List (intersperse)
import Data.Map (lookup)
import System.FilePath (takeFileName)
import Text.PrettyPrint (Doc, text, empty, (<>), (<+>), hcat, nest)

import Language.Drasil.Printing.Helpers (ast, ($^$), vsep)
import Language.Drasil.Printing.LayoutObj (RefMap)
import Drasil.Printers.Common

-- | HTML attribute selector.
data Variation = Class | Id | Align deriving Eq

instance Show Variation where
  show Class = "class"
  show Id    = "id"
  show Align = "align"

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

-- | Indent the Document by 2 positions.
indent :: Doc -> Doc
indent = nest 2

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

-- | Helper for setting up centered div tags
centeredDiv :: Doc -> Doc
centeredDiv = wrapGen' vsep Align "div" (text "center") [""]

-- | Helper for setting up centered div tags with an Id
centeredDivId :: Doc -> Doc -> Doc
centeredDivId l con = vsep [wrapInside "div" atrs, con, tagR "div"]
  where
    atrs = [(show Id, l), (show Align, text "center")]

-- | Helper for setting up links to references
reflink :: RefMap -> String -> Doc -> Doc
reflink rm ref txt = brak txt <> paren rp
  where
    fn = maybe empty fp (lookup ref rm)
    fp s = text $ "./" ++ s ++ ".md"
    rp = fn <> text ("#" ++ ref)

-- | Helper for setting up links to references with additional information.
reflinkInfo :: RefMap -> String -> Doc -> Doc -> Doc
reflinkInfo rm rf txt info = reflink rm rf txt <+> info

-- | Create clickable URIs with a link and a displayed text. If displayed text
-- is the same as the link, it will return @<link>@ instead of @[text](link)@.
reflinkURI :: Doc -> Doc -> Doc
reflinkURI ref txt
  | ref == txt = angbrac ref
  | otherwise  = brak txt <> paren ref

-- | Helper for setting up figures
image :: Doc -> Maybe Doc -> Doc
image f Nothing = text "!" <> reflinkURI (text $ "./assets/" ++ takeFileName (show f)) (text "")
image f (Just c) = text "!" <> reflinkURI (text $ "./assets/" ++ takeFileName (show f)) c $^$ bold (text "Figure: " <> c)

-- | Helper for setting up captions
caption :: Doc -> Doc
caption = wrapGen' hcat Align "p" (text "center") [""]

-- | Helper for setting up headings with an id attribute.
-- id attribute will only work for mdBook.
heading ::  Doc -> Doc -> Doc
heading t l = t <+> brace (text "#" <> l)

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
