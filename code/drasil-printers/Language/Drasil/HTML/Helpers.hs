module Language.Drasil.HTML.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, empty, ($$), (<>), vcat, hcat, nest,
  cat, hcat)
import Data.List (intersperse, foldl1)

import Language.Drasil hiding (Expr)

--import Language.Drasil.Document (Document, MaxWidthPercent)
import Language.Drasil.Printing.AST (Expr)

html, head_tag, body, title, paragraph, code, tr, th, td, figure,
  figcaption, li, pa, ba :: Doc -> Doc
-- | HTML tag wrapper
html       = wrap "html" []
-- | Head tag wrapper
head_tag   = wrap "head" []
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
          | otherwise = wrap ("h"++show n) []

data Variation = Class | Id

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrap_gen Class a empty

wrapAux :: String -> [String] -> Doc -> Doc
wrapAux a = wrap_genAux hcat Class a empty

-- | Helper for wrapping HTML tags.
-- The forth argument provides class names for the CSS.
wrap_genAux :: ([Doc] -> Doc) -> Variation -> String -> Doc -> [String] -> Doc -> Doc
wrap_genAux sepf _ s _ [] = \x -> 
  let tb c = text $ "<" ++ c ++ ">"
  in sepf [tb s, indent x, tb $ '/':s]
wrap_genAux sepf Class s _ ts = \x ->
  let tb c = text $ "<" ++c++ " class=\""++(foldr1 (++) (intersperse " " ts))++"\">"
  in let te c = text $ "</" ++ c ++ ">"
  in sepf [tb s, indent x, te s]
wrap_genAux sepf Id s ti _ = \x ->
  let tb c = text ("<" ++c++ " id=\"") <> ti <> text ("\">")
      te c = text $ "</" ++ c ++ ">"
  in sepf [tb s, indent x, te s] 

wrap_gen :: Variation -> String -> Doc -> [String] -> Doc -> Doc
wrap_gen = wrap_genAux cat


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
refwrap = flip (wrap_gen Id "div") [""]

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>"

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
  img [("src", f), ("alt", c), ("width", text $ show (wp) ++ "%")],
  figcaption c]

em, sup, sub, bold :: Doc -> Doc
-- | Emphasis (italics) tag
em = wrapAux "em" []
-- | Superscript tag
sup = wrapAux "sup" []
-- | Subscript tag
sub = wrapAux "sub" []
-- | Bold tag
bold = wrapAux "b" []

article_title, author :: Doc -> Doc
-- | Title header
article_title t = div_tag ["title"]  (h 1 t)
-- | Author header
author a        = div_tag ["author"] (h 2 a)

-- | Div tag wrapper
div_tag :: [String] -> Doc -> Doc
div_tag = wrap "div"

span_tag :: [String] -> Doc -> Doc
span_tag = wrap "span"

indent :: Doc -> Doc
indent = nest 0

-- | Create and markup fractions
fraction :: Doc -> Doc -> Doc
fraction a b =
  div_tag ["fraction"] (span_tag ["fup"] a $$ span_tag ["fdn"] b)

-- | Build cases for case expressions
cases :: [(Expr,Expr)] -> (Expr -> Doc) -> Doc
cases ps p_expr = span_tag ["casebr"] (text "{") $$ div_tag ["cases"] 
                  (makeCases ps p_expr)

-- | Build case expressions              
makeCases :: [(Expr,Expr)] -> (Expr -> Doc) -> Doc                 
makeCases [] _ = empty
makeCases (p:ps) p_expr = ((span_tag [] (p_expr (fst p) <> text " , " <>
                            (span_tag ["case"] (p_expr (snd p))))) $$
                            makeCases ps p_expr)
