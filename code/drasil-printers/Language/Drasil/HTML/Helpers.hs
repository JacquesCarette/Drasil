module Language.Drasil.HTML.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, render, empty, ($$), (<>), vcat, hcat)
import Data.List (intersperse, foldl1)

import Language.Drasil hiding (Expr)

--import Language.Drasil.Document (Document, MaxWidthPercent)
import Language.Drasil.Printing.AST (Expr)

html, head_tag, body, title, paragraph, code, tr, th, td, figure,
  figcaption :: Doc -> Doc
-- | HTML tag wrapper
html       = oldWrap "html" []
-- | Head tag wrapper
head_tag   = oldWrap "head" []
-- | Body tag wrapper
body       = oldWrap "body" []
-- | Title tag wrapper
title      = oldWrap "title" []
-- | Paragraph tag wrapper
paragraph  = oldWrap "p" ["paragraph"]
-- | Code tag wrapper
code       = oldWrap "code" ["code"]
-- | Table row tag wrapper
tr         = oldWrap "tr" []
-- | Table header tag wrapper
th         = oldWrap "th" []
-- | Table cell tag wrapper
td         = oldWrap "td" []
-- | Figure tag wrapper
figure     = oldWrap "figure" []
-- | Figcaption tag wrapper
figcaption = oldWrap "figcaption" []

img :: [(String, Doc)] -> Doc
-- | Image tag wrapper
img        = wrapInside "img"

-- | Helper for HTML headers
h :: Int -> Doc -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 7 = error "Illegal header (too large)"
          | otherwise = oldWrap ("h"++show n) []

-- | Helper for wrapping HTML tags.
-- The second argument provides class names for the CSS.
oldWrap :: String -> [String] -> Doc -> Doc
oldWrap s [] = \x -> 
  let tb c = text $ "<" ++ c ++ ">"
  in vcat [tb s, x, tb $ '/':s]
oldWrap s ts = \x ->
  let tb c = text $ "<" ++c++ " class=\""++(foldr1 (++) (intersperse " " ts))++"\">"
  in let te c = text $ "</" ++ c ++ ">"
  in vcat [tb s, x, te s]

-- | Helper for wrapping attributes in a tag.
-- | The first argument is tag name.
-- | The String in the pair is the attribute name,
-- | The Doc is the value for different attributes.
wrapInside :: String -> [(String, Doc)] -> Doc
wrapInside t p = text ("<" ++ t ++ " ") <> foldl1 (<>) (map foldStr p) <> text ">"
 where foldStr (attr, val) = text (attr ++ "=\"") <> val <> text "\" "

-- | Helper for setting up captions  
caption :: Doc -> Doc
caption = oldWrap "p" ["caption"]

-- | Helper for setting up references
refwrap :: Doc -> Doc -> Doc
refwrap r x = vcat [hcat [text "<div id=\"", r, text  "\">"], x, text "</div>"]

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>"

-- | Helper for setting up links to external URIs
reflinkURI :: String -> Doc -> Doc
reflinkURI ref txt = text ("<a href=\"" ++ ref ++ "\">") <> txt <> text "</a>"

-- | Helper for setting up figures
image :: Doc -> Doc -> MaxWidthPercent -> Doc
image f c 100 = 
  figure $ vcat[
  img $ [("src", f), ("alt", c)],
  figcaption c]
image f c wp =
  figure $ vcat[
  img $ [("src", f), ("alt", c), ("width", text $ show (wp) ++ "%")],
  figcaption c
  ]

em :: Doc -> Doc
-- | Emphasis (italics) tag
em x = text "<em>"  <> x <> text "</em>"

sub,sup,bold :: String -> String  
-- | Subscript tag
sub x = "<sub>" ++ x ++ "</sub>"
-- | Superscript tag
sup x = "<sup>" ++ x ++ "</sup>"
-- | Bold tag
bold x  = "<b>"  ++ x ++ "</b>"

article_title, author :: Doc -> Doc
-- | Title header
article_title t = div_tag ["title"]  (h 1 t)
-- | Author header
author a        = div_tag ["author"] (h 2 a)

-- | Div tag wrapper
div_tag :: [String] -> Doc -> Doc
div_tag = oldWrap "div"
  
-- | Span tag wrapper
span_tag :: [String] -> String -> Doc
span_tag t = oldWrap "span" t . text


-- | Create and markup fractions
fraction :: String -> String -> String  
fraction a b =
  render $ div_tag ["fraction"] (span_tag ["fup"] a $$ span_tag ["fdn"] b)

-- | Build cases for case expressions
cases :: [(Expr,Expr)] -> (Expr -> String) -> String
cases ps p_expr = render $ (span_tag ["casebr"] "{" $$ div_tag ["cases"] 
                  (makeCases ps p_expr))

-- | Build case expressions
makeCases :: [(Expr,Expr)] -> (Expr -> String) -> Doc                 
makeCases [] _ = empty
makeCases (p:ps) p_expr = ((span_tag [] (p_expr (fst p) ++ " , " ++
                            (render $ span_tag ["case"] (p_expr (snd p))))) $$
                            makeCases ps p_expr)
