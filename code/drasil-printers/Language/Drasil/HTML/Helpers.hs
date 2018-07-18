module Language.Drasil.HTML.Helpers where

import Text.PrettyPrint (Doc, text, render, empty, ($$), (<>), vcat, hcat)
import Data.List (intersperse)

import Language.Drasil hiding (Expr)

--import Language.Drasil.Document (Document, MaxWidthPercent)
import Language.Drasil.Printing.AST (Expr)

html, head_tag, body, title, paragraph, code, tr, th, td :: Doc -> Doc
-- | HTML tag wrapper
html      = wrap "html" []
-- | Head tag wrapper
head_tag  = wrap "head" []
-- | Body tag wrapper
body      = wrap "body" []
-- | Title tag wrapper
title     = wrap "title" []
-- | Paragraph tag wrapper
paragraph = wrap "p" ["paragraph"]
-- | Code tag wrapper
code      = wrap "code" ["code"]
-- | Table row tag wrapper
tr        = wrap "tr" []
-- | Table header tag wrapper
th        = wrap "th" []
-- | Table cell tag wrapper
td        = wrap "td" []

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

-- | Helper for setting up captions  
caption :: Doc -> Doc
caption = wrap "p" ["caption"]

-- | Helper for setting up references
refwrap :: Doc -> Doc -> Doc
refwrap r x = vcat [hcat [text "<div id=\"", r, text  "\">"], x, text "</div>"]

-- | Helper for setting up links to references
reflink :: String -> Doc -> Doc
reflink ref txt = text ("<a href=#" ++ ref ++ ">") <> txt <> text "</a>"

-- | Helper for setting up figures
image :: Doc -> Doc -> MaxWidthPercent -> Doc
image f c 100 = 
  text "<img class=\"figure\" src=\"" <> f <> text "\" alt=\"" <> c <> text "\"></img>"
image f c wp =
  text "<img class=\"figure\" src=\"" <> f <> text "\" alt=\"" <> c <> 
  text ("\"style=\"max-width: " ++ show (wp / 100) ++ "%;\"></img>")

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

-- | Generates the CSS selectors necessary for a document
makeCSS :: Document -> Doc  
makeCSS _ = vcat [
-- TODO: Autogenerate necessary css selectors only, make CSS configurable
  text "body {min-width: 400px; max-width: 1400px;}",
  text ".title {text-align: center;}",
  text ".author {text-align: center;}",
  text ".paragraph {text-align: justify;}",
  vcat [
    text ".cases {",
    text "  display: inline-block;",
    text "  vertical-align: middle;}"],
  vcat [
    text ".case {",
    text "  float: right;",
    text "  padding-left: 1em;}"],
  vcat [
    text ".cases > span {",
    text "  display: block;",
    text "  padding-top: 0.1em;",
    text "  padding-left: 0em;}"],
  vcat [
    text ".casebr {",
    text "  display: inline-block;",
    text "  vertical-align: middle;",
    text "  margin: 0 0.2em 0.4ex;",
    text "  text-align: center;",
    text "  font-size: 500%;}"],
  vcat [
    text ".caption {",
    text "  text-align: center;",
    text "  font-weight: bold;",
    text "  padding-bottom: 1%;",
    text "  line-height: 0;}"
    ],
  vcat [
    text ".fraction {",
    text "  display: inline-block;",
    text "  vertical-align: middle;",
    text "  margin: 0 0.2em 0.4ex;",
    text "  text-align: center;}"
    ],
  vcat [
    text ".fraction > span {",
    text "  display: block;",
    text "  padding-top: 0.15em;}"
    ],
  text ".fdn {border-top: thin solid black;}",
  vcat [
    text ".table {",
    text "  text-align: left;",
    text "  padding-left: 1%;",
    text "  width: 90%;",
    text "  margin-bottom: 2%;",
    text "  margin-top: 2%}"],
  vcat [
    text "table, th, td {",
    text "  border-collapse: collapse;",
    text "  margin-left: auto;",
    text "  margin-right: auto;}"],
  text "th, td {border: 1px solid black; padding: 0.5em;}",
  text ".tdefn, .ddefn {width: 75%; margin-top: 1%; margin-bottom: 1%;}",
  text ".tdefn th {width: 15%;}",
  text ".ddefn th {width: 15%;}",
  text ".section {width: 80%; margin: 0 auto; text-align: left;}",
  vcat [
    text ".code {",
    text "  display: inline-block;",
    text "  text-align: left;",
    text ("  font-family: Monaco, Consolas, \"Andale Mono\"," ++
      "\"DejaVu Sans Mono\", monospace;"),
    text "  font-size: 95%;",
    text "  line-height: 140%;",
    text "  white-space: pre;",
    text "  white-space: pre-wrap;",
    text "  white-space: -moz-pre-wrap;",
    text "  white-space: -o-pre-wrap;",
    text "  background: #faf8f0;}"],
  text ".list {text-align: left;}",
  text ".figure {max-width: 100%;}",
  vcat [
    text ".matrix {",
    text "  position: relative;",
    text "  display: inline-table;",
    text "  margin: 10px;",
    text "  vertical-align: middle;}",
    text ".matrix:before, .matrix:after {",
    text "  content: \"\";",
    text "  position: absolute;",
    text "  top: 0;",
    text "  border: 1px solid #000;",
    text "  width: 5px;",
    text "  height: 100%;}",
    text ".matrix:before {",
    text "  left: -5px;",
    text "  border-right: 0px;}",
    text ".matrix:after {",
    text "  right: -5px;",
    text "  border-left: 0px;}",
    text ".matrix td {",
    text "  padding: 5px;",
    text "  text-align: center;",
    text "  border: 0px;}"],
  vcat [
    text "ul.hide-list-style {",
    text "  list-style-type: none;}"
    ],
  vcat [
    text "ul.hide-list-style-no-indent {",
    text "  list-style-type: none;",
    text "padding: 0;}"
    ] 
  ]

-- | Create the link to the necessary CSS file
linkCSS :: String -> Doc  
linkCSS fn = 
  text $ "<link rel=\"stylesheet\" type=\"text/css\" href=\""++fn++".css\">"

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
