module Language.Drasil.HTML.Helpers where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, text, render, empty, ($$), (<>), vcat, hcat, nest,
  ($+$), cat, sep, fcat)
import Data.List (intersperse, foldl1)

import Language.Drasil hiding (Expr)

--import Language.Drasil.Document (Document, MaxWidthPercent)
import Language.Drasil.Printing.AST (Expr)

html, head_tag, body, title, paragraph, code, tr, th, td, figure,
  figcaption, li, pa, ba :: Doc -> Doc
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
-- | List tag wrapper
li         = oldWrap "li" []
-- | Paragraph in list tag wrapper
pa         = oldWrap "p" []

ba         = oldWrap "b" []

ol, ul, table :: [String] -> Doc -> Doc
-- | Ordered list tag wrapper
ol a b       = wrap "ol" a b
-- | Unordered list tag wrapper
ul a b       = wrap "ul" a b
-- | Table tag wrapper
table a b    = wrap "table" a b

img :: [(String, Doc)] -> Doc
-- | Image tag wrapper
img        = wrapInside "img"

-- | Helper for HTML headers
h :: Int -> Doc -> Doc
h n       | n < 1 = error "Illegal header (too small)"
          | n > 7 = error "Illegal header (too large)"
          | otherwise = oldWrap ("h"++show n) []

-- OLD FUNCTIONS
--------------------------------------------------
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
--------------------------------------------------

data Variation = Class | Id

wrap :: String -> [String] -> Doc -> Doc
wrap a = wrap_gen Class a empty
-- | Helper for wrapping HTML tags.
-- The forth argument provides class names for the CSS.
wrap_gen :: Variation -> String -> Doc -> [String] -> Doc -> Doc
wrap_gen _ s _ [] = \x -> 
  let tb c = text $ "<" ++ c ++ ">"
  in sep [tb s, indent x, tb $ '/':s]
wrap_gen Class s _ ts = \x ->
  let tb c = text $ "<" ++c++ " class=\""++(foldr1 (++) (intersperse " " ts))++"\">"
  in let te c = text $ "</" ++ c ++ ">"
  in sep [tb s, indent x, te s]
wrap_gen Id s ti _ = \x ->
  let tb c = text ("<" ++c++ " id=\"") <> ti <> text ("\">")
      te c = text $ "</" ++ c ++ ">"
  in sep [tb s, indent x, te s] 


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

-- OLD FUNCTIONS
--------------------------------------------------
-- | Helper for setting up references
oldRefwrap :: Doc -> Doc -> Doc
oldRefwrap r x = vcat [hcat [text "<div id=\"", r, text  "\">"], x, text "</div>"]
--------------------------------------------------

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
  figure $ vcat[
  img $ [("src", f), ("alt", c)],
  figcaption c]
image f c wp =
  figure $ vcat[
  img $ [("src", f), ("alt", c), ("width", text $ show (wp) ++ "%")],
  figcaption c
  ]

-- OLD FUNCTIONS
--------------------------------------------------
oldEm :: Doc -> Doc
-- | Emphasis (italics) tag
oldEm x = text "<em>"  <> x <> text "</em>"

oldSub, oldSup, oldBold :: String -> String  
-- | Subscript tag
oldSub x = "<sub>" ++ x ++ "</sub>"
-- | Superscript tag
oldSup x = "<sup>" ++ x ++ "</sup>"
-- | Bold tag
oldBold x  = "<b>"  ++ x ++ "</b>"
--------------------------------------------------

em, sup, sub, bold :: Doc -> Doc
-- | Emphasis (italics) tag
em = wrap "em" []
-- | Superscript tag
sup = wrap "sup" []
-- | Subscript tag
sub = wrap "sub" []
-- | Bold tag
bold = wrap "b" []

article_title, author :: Doc -> Doc
-- | Title header
article_title t = div_tag ["title"]  (h 1 t)
-- | Author header
author a        = div_tag ["author"] (h 2 a)

-- | Div tag wrapper
div_tag :: [String] -> Doc -> Doc
div_tag = oldWrap "div"
  
-- OLD FUNCTIONS
--------------------------------------------------
-- | Span tag wrapper
oldSpan_tag :: [String] -> String -> Doc
oldSpan_tag t = oldWrap "span" t . text

-- | Create and markup fractions
oldFraction :: String -> String -> String  
oldFraction a b =
  render $ div_tag ["fraction"] (oldSpan_tag ["fup"] a $$ oldSpan_tag ["fdn"] b)

-- | Build oldCases for case expressions
oldCases :: [(Expr,Expr)] -> (Expr -> String) -> String
oldCases ps oldP_expr = render $ (oldSpan_tag ["casebr"] "{" $$ div_tag ["cases"] 
                  (oldMakeCases ps oldP_expr))

-- | Build case expressions
oldMakeCases :: [(Expr,Expr)] -> (Expr -> String) -> Doc                 
oldMakeCases [] _ = empty
oldMakeCases (p:ps) oldP_expr = ((oldSpan_tag [] (oldP_expr (fst p) ++ " , " ++
                            (render $ oldSpan_tag ["case"] (oldP_expr (snd p))))) $$
                            oldMakeCases ps oldP_expr)
--------------------------------------------------

span_tag :: [String] -> Doc -> Doc
span_tag t = wrap "span" t

indent :: Doc -> Doc
indent = nest 4

indentl :: [Doc] -> [Doc]
indentl = map $ nest 4

vvcat :: [Doc] -> Doc
vvcat = foldr ($+$) empty

-- | Create and markup fractions
fraction :: Doc -> Doc -> Doc
fraction a b =
  div_tag ["fraction"] (span_tag ["fup"] a $$ span_tag ["fdn"] b)

-- | Build cases for case expressions
cases :: [(Expr,Expr)] -> (Expr -> Doc) -> Doc
cases ps oldP_expr = (span_tag ["casebr"] (text "{") $$ div_tag ["cases"] 
                  (makeCases ps oldP_expr))

-- | Build case expressions              
makeCases :: [(Expr,Expr)] -> (Expr -> Doc) -> Doc                 
makeCases [] _ = empty
makeCases (p:ps) oldP_expr = ((span_tag [] (oldP_expr (fst p) <> text " , " <>
                            (span_tag ["case"] (oldP_expr (snd p))))) $$
                            makeCases ps oldP_expr)
