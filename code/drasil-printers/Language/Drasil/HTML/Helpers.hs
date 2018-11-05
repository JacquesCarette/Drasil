module Language.Drasil.HTML.Helpers where

import Text.PrettyPrint (Doc, text, render, empty, ($$), (<>), vcat, hcat)
import Data.List (intersperse, length)
import Data.String (unwords)

import Language.Drasil hiding (Expr)

--import Language.Drasil.Document (Document, MaxWidthPercent)
import Language.Drasil.Printing.AST (Expr)

html, head_tag, body, title, paragraph, code, tr, th, td, figure,
  figcaption :: Doc -> Doc
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
-- | Figcaption tag wra
figcaption = wrap "figcaption" []

-- | Helper for HTML headers
h :: Int -> Doc -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 7 = error "Illegal header (too large)"
          | otherwise = wrap ("h"++show n) []

-- | Helper for wrapping HTML tags.
-- The second argument provides class names for the CSS.
wrap :: String -> [String] -> Doc -> Doc
wrap s [] = \x -> 
  let tb c = text $ "<" ++ c ++ ">"
  in vcat [tb s, x, tb $ '/':s]
wrap s ts = \x ->
  let tb c = text $ "<" ++c++ " class=\""++(foldr1 (++) (intersperse " " ts))++"\">"
  in let te c = text $ "</" ++ c ++ ">"
  in vcat [tb s, x, te s]

-- | Helper for wrapping attributes in a tag.
-- | The first argument is tag name.
-- | The second argument contains different attribute names.
-- | The third argument contains the values for different attributes.
wrapInside :: String -> [String] -> [String] -> Doc
wrapInside t att va = if length att /= length va then
    error ("The number of attributes doesn't match the number of values.")
 else text $ "<" ++ t ++ (unwords $ zipWith foldStr att va) ++ ">"
 where foldStr s1 s2 = s1 ++ "\"" ++ s2 ++ "\""

-- | Helper for setting up captions  
caption :: Doc -> Doc
caption = wrap "p" ["caption"]

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
  text "<img src=\"" <> f <> text "\" alt=\"" <> c <> text "\">",
  figcaption c]
image f c wp =
  figure $ vcat[
  text "<img src=\"" <> f <> text "\" alt=\"" <> c <>
  text ("\" width=\" " ++ show (wp) ++ "%\">"),
  figcaption c
  ]

em :: Doc -> Doc
-- | Emphasis (italics) tag
em  = \x -> text "<em>"  <> x <> text "</em>"

sub,sup,bold :: String -> String  
-- | Subscript tag
sub = \x -> "<sub>" ++ x ++ "</sub>"
-- | Superscript tag
sup = \x -> "<sup>" ++ x ++ "</sup>"
-- | Bold tag
bold  = \x -> "<b>"  ++ x ++ "</b>"

article_title, author :: Doc -> Doc
-- | Title header
article_title t = div_tag ["title"]  (h 1 t)
-- | Author header
author a        = div_tag ["author"] (h 2 a)

-- | Div tag wrapper
div_tag :: [String] -> Doc -> Doc
div_tag = wrap "div"
  
-- | Span tag wrapper
span_tag :: [String] -> String -> Doc
span_tag = \t -> wrap "span" t . text


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
