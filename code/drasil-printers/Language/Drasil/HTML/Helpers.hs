module Language.Drasil.HTML.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, empty, ($$), (<>), (<+>), vcat, hcat, nest,
  cat, hcat)
import Data.List (intersperse, foldl1)

import Language.Drasil hiding (Expr)

--import Language.Drasil.Document (Document, MaxWidthPercent)
import Language.Drasil.Printing.AST (Expr)

html, headTag, body, title, paragraph, code, tr, th, td, figure,
  figcaption, li, pa, ba :: Doc -> Doc
-- | HTML tag wrapper
html       = wrap "html" []
-- | Head tag wrapper
headTag   = wrap "head" []
-- | Body tag wrapper
body       = wrap "body" []
-- | Title tag wrapper
title      = wrap "title" []
-- | Paragraph tag wrapper
paragraph  = wrap "p" ["paragraph"]
-- | Code tag wrapper
code       = wrap "code" ["code"]
-- | Table row tag wrapper
tr         = wrap "tr" []
-- | Table header tag wrapper
th         = wrap "th" []
-- | Table cell tag wrapper
td         = wrap "td" []
-- | Figure tag wrapper
figure     = wrap "figure" []
-- | Figcaption tag wrapper
figcaption = wrap "figcaption" []
-- | List tag wrapper
li         = wrap "li" []
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

img :: [(String, Doc)] -> Doc
-- | Image tag wrapper
img        = wrapInside "img"

-- | Helper for HTML headers
h :: Int -> Doc -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 7 = error "Illegal header (too large)"
          | otherwise = wrap ("h" ++ show n) []

data Variation = Class | Id

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrapGen Class a empty

wrap' :: String -> [String] -> Doc -> Doc
wrap' a = wrapGen' hcat Class a empty

-- | Helper for wrapping HTML tags.
-- The forth argument provides class names for the CSS.
wrapGen' :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen' sepf _ s _ [] = \x -> 
  let tb c = text $ "<" ++ c ++ ">"
  in sepf [tb s, indent x, tb $ '/':s]
wrapGen' sepf Class s _ ts = \x ->
  let tb c = text $ "<" ++ c ++ " class=\"" ++ foldr1 (++) (intersperse " " ts) ++ "\">"
  in let te c = text $ "</" ++ c ++ ">"
  in sepf [tb s, indent x, te s]
wrapGen' sepf Id s ti _ = \x ->
  let tb c = text ("<" ++ c ++ " id=\"") <> ti <> text "\">"
      te c = text $ "</" ++ c ++ ">"
  in sepf [tb s, indent x, te s] 

wrapGen :: Variation -> String -> Doc -> [String] -> Doc -> Doc
wrapGen = wrapGen' cat


-- | Helper for wrapping attributes in a tag.
-- | The first argument is tag name.
-- | The String in the pair is the attribute name,
-- | The Doc is the value for different attributes.
wrapInside :: String -> [(String, Doc)] -> Doc
wrapInside t p = text ("<" ++ t ++ " ") <> foldl1 (<>) (map foldStr p) <> text ">"
  where foldStr (attr, val) = text (attr ++ "=\"") <> val <> text "\" "

-- | Helper for setting up captions  
caption :: Doc -> Doc
caption = wrap "p" ["caption"]

refwrap :: Doc -> Doc -> Doc
refwrap = flip (wrapGen Id "div") [""]

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>"

-- | Helper for setting up links to references with additional information
reflinkInfo :: String -> Doc -> Doc -> Doc
reflinkInfo ref txt info = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>" <+> info

-- | Helper for setting up links to external URIs
reflinkURI :: String -> Doc -> Doc
reflinkURI ref txt = text ("<a href=\"" ++ ref ++ "\">") <> txt <> text "</a>"

-- | Helper for setting up figures
image :: Doc -> Doc -> MaxWidthPercent -> Doc
image f c 100 = 
  figure $ vcat [
  img [("src", f), ("alt", c)],
  figcaption c]
image f c wp =
  figure $ vcat [
  img [("src", f), ("alt", c), ("width", text $ show wp ++ "%")],
  figcaption c]

em, sup, sub, bold :: Doc -> Doc
-- | Emphasis (italics) tag
em = wrap' "em" []
-- | Superscript tag
sup = wrap' "sup" []
-- | Subscript tag
sub = wrap' "sub" []
-- | Bold tag
bold = wrap' "b" []

articleTitle, author :: Doc -> Doc
-- | Title header
articleTitle t = divTag ["title"]  (h 1 t)
-- | Author header
author a        = divTag ["author"] (h 2 a)

-- | Div tag wrapper
divTag :: [String] -> Doc -> Doc
divTag = wrap "div"

spanTag :: [String] -> Doc -> Doc
spanTag = wrap "span"

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

-- | Build case expressions              
makeCases :: [(Expr,Expr)] -> (Expr -> Doc) -> Doc                 
makeCases [] _ = empty
makeCases (p:ps) pExpr = spanTag [] (pExpr (fst p) <> text " , " <>
                          spanTag ["case"] (pExpr (snd p))) $$
                          makeCases ps pExpr
