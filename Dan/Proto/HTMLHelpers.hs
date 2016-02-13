{-# OPTIONS -Wall #-} 
module HTMLHelpers where

import Text.PrettyPrint
import Data.List (intersperse)
import Spec (Document)
import Helpers (quotes)

html, head_tag, body, title :: Doc -> Doc
html      = wrap "html"
head_tag  = wrap "head"
body      = wrap "body"
title     = wrap "title"

h :: Int -> Doc -> Doc
h n       | n < 0 = error "Illegal header (too small)"
          | n > 7 = error "Illegal header (too large)"
          | otherwise = wrap ("h"++show n)

wrap :: String -> Doc -> Doc
wrap s = \x -> 
  vcat [tb s, x, tb $ "/"++s]
  where tb c = text $ "<" ++ c ++ ">"

sub :: String -> String  
sub = \x -> "<sub>" ++ x ++ "</sub>"

article_title, author :: Doc -> Doc
article_title t = div_tag ["\"title\""] (h 1 t)
author a = div_tag ["\"author\""] (h 3 a)

div_tag :: [String] -> Doc -> Doc
div_tag [] = wrap "div"
div_tag x = \s -> vcat [
  text ("<div class="++(foldr1 (++) (intersperse " " x))++">"),
  s,
  text "</div>"]
  
span_tag :: [String] -> Doc -> Doc
span_tag [] = wrap "span"
span_tag x = \s -> vcat [
  text ("<span class="++(foldr1 (++) (intersperse " " x))++">"),
  s,
  text "</span>"]

makeCSS :: Document -> Doc  
makeCSS _ = vcat [
-- TODO: Autogenerate necessary css selectors only, make CSS configurable
  text ".title {text-align:center;}",
  text ".author {text-align:center;}",
  text ".paragraph {text-align:justify;}",
  vcat [
    text ".fraction {",
    text "    display: inline-block;",
    text "    vertical-align: middle;",
    text "    margin: 0 0.2em 0.4ex;",
    text "    text-align: center;}"
    ],
  vcat [
    text ".fraction > span {",
    text "    display: block;",
    text "    padding-top: 0.15em;}"
    ],
  text ".fdn {border-top: thin solid black;}"
  text ".bar {display: none;}"
  ]

linkCSS :: String -> Doc  
linkCSS fn = 
  text $ "<link rel=\"stylesheet\" type=\"text/css\" href=\""++fn++".css\">"
  
fraction a b =
  div_tag ["fraction"] (span ["fup"] a $$ span ["bar"] "/" $$ span ["fdn"] b)