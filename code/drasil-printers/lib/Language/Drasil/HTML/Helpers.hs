-- | Helper functions for creating HTML printers (specifically, HTML tag wrappers).
module Language.Drasil.HTML.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, empty, ($$), (<>), (<+>), vcat, hcat, nest,
  cat, hcat)
import Data.List (intersperse)

import Language.Drasil hiding (Expr)

--import Language.Drasil.Document (Document, MaxWidthPercent)
import Language.Drasil.Printing.AST (Expr, Spec)

-- | Data type that carries functions that vary
-- for bib printing
data BibFormatter = BibFormatter {
  -- | Emphasis (italics) rendering
  emph :: Doc -> Doc,
  -- | Spec rendering
  spec :: Spec -> Doc
}

html, headTag, body, title, paragraph, code, tr, th, td, figure,
  figcaption, li, pa, ba :: Doc -> Doc
-- | HTML tag wrapper.
html       = wrap "html" []
-- | Head tag wrapper.
headTag   = wrap "head" []
-- | Body tag wrapper.
body       = wrap "body" []
-- | Title tag wrapper.
title      = wrap "title" []
-- | Paragraph tag wrapper.
paragraph  = wrap "p" ["paragraph"]
-- | Code tag wrapper.
code       = wrap "code" ["code"]
-- | Table row tag wrapper.
tr         = wrap "tr" []
-- | Table header tag wrapper.
th         = wrap "th" []
-- | Table cell tag wrapper.
td         = wrap "td" []
-- | Figure tag wrapper.
figure     = wrap "figure" []
-- | Figcaption tag wrapper.
figcaption = wrap "figcaption" []
-- | List tag wrapper.
li         = wrap "li" []
-- | Paragraph in list tag wrapper.
pa         = wrap "p" []
-- | Bring attention to element wrapper.
ba         = wrap "b" []

ol, ul, table :: [String] -> Doc -> Doc
-- | Ordered list tag wrapper.
ol       = wrap "ol"
-- | Unordered list tag wrapper.
ul       = wrap "ul"
-- | Table tag wrapper.
table    = wrap "table"

img :: [(String, Doc)] -> Doc
-- | Image tag wrapper.
img        = wrapInside "img"

-- | Helper for HTML headers.
h :: Int -> Doc -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 7 = error "Illegal header (too large)"
          | otherwise = wrap ("h" ++ show n) []

-- | HTML attribute selector.
data Variation = Class | Id | Align deriving Eq

instance Show Variation where
  show Class = "class"
  show Id    = "id"
  show Align = "align"

-- | General 'Class' wrapper function and formats the document space with 'cat'.
wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen Class a empty

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

-- | General wrapper that formats the document space nicely.
wrapGen :: Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen = wrapGen' cat

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

-- | Helper for setting up captions. 
caption :: Doc -> Doc
caption = wrap "p" ["caption"]

-- | Helper for wrapping divisions or sections.
refwrap :: Doc -> Doc -> Doc
refwrap = flip (wrapGen Id "div") [""]

-- | Helper for setting up links to references.
reflink :: String -> Doc -> Doc
reflink rf txt = text ("<a href=#" ++ rf ++ ">") <> txt <> text "</a>"

-- | Helper for setting up links to references with additional information.
reflinkInfo :: String -> Doc -> Doc -> Doc
reflinkInfo rf txt info = text ("<a href=#" ++ rf ++ ">") <> txt <> text "</a>" <+> info

-- | Helper for setting up links to external URIs.
reflinkURI :: String -> Doc -> Doc
reflinkURI rf txt = text ("<a href=\"" ++ rf ++ "\">") <> txt <> text "</a>"

-- | Helper for setting up figures.
image :: Doc -> Maybe Doc -> MaxWidthPercent -> Doc
image f Nothing wp = 
  figure $ vcat [img $ [("src", f), ("alt", text "")] ++ [("width", text $ show wp ++ "%") | wp /= 100]]
image f (Just c) wp =
  figure $ vcat [img $ [("src", f), ("alt", c)] ++ [("width", text $ show wp ++ "%") | wp /= 100], figcaption $ text "Figure: " <> c]

em, sup, sub, bold :: Doc -> Doc
-- | Emphasis (italics) tag.
em = wrap' "em" []
-- | Superscript tag.
sup = wrap' "sup" []
-- | Subscript tag.
sub = wrap' "sub" []
-- | Bold tag.
bold = wrap' "b" []

articleTitle, author :: Doc -> Doc
-- | Title header.
articleTitle t = divTag ["title"]  (h 1 t)
-- | Author header.
author a        = divTag ["author"] (h 2 a)

-- | Div tag wrapper.
divTag :: [String] -> Doc -> Doc
divTag = wrap "div"

-- | Span tag wrapper.
spanTag :: [String] -> Doc -> Doc
spanTag = wrap "span"

-- | Indent the Document by 2 positions.
indent :: Doc -> Doc
indent = nest 2

-- Not used since we use MathJax handles this
-- | Create and markup fractions
-- fraction :: Doc -> Doc -> Doc
-- fraction a b =
--   divTag ["fraction"] (spanTag ["fup"] a $$ spanTag ["fdn"] b)

-- Not used since we use MathJax handles this
-- -- | Build cases for case expressions
-- cases :: [(Expr,Expr)] -> (Expr -> Doc) -> Doc
-- cases ps pExpr = spanTag ["casebr"] (text "{") $$ divTag ["cases"] 
--                   (makeCases ps pExpr)

-- | Build case expressions.
makeCases :: [(Expr,Expr)] -> (Expr -> Doc) -> Doc                 
makeCases [] _ = empty
makeCases (p:ps) pExpr = spanTag [] (pExpr (fst p) <> text " , " <>
                          spanTag ["case"] (pExpr (snd p))) $$
                          makeCases ps pExpr
