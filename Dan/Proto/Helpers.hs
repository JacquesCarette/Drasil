module Helpers where

import Text.PrettyPrint
import Data.Char

--basic
bslash = text "\\"
sq t = text $ "[" ++ t ++ "]"
br t = text $ "{" ++ t ++ "}"

--TeX
docclass [] braces = bslash <> text "documentclass" <> br braces
docclass sqbrack braces = bslash <> text "documentclass" <> sq sqbrack <> br braces

usepackage pkg = bslash <> text "usepackage" <> br pkg

exdoc [] d = bslash <> text "externaldocument" <> br d
exdoc sqbrack d = bslash <> text "externaldocument" <> sq sqbrack <> br d

title t = bslash <> text "title" <> br t

author a = bslash <> text "author" <> br a

begin = bslash <> text "begin" <> br "document" $$ bslash <> text "maketitle"


--format strings
upcase, lowcase :: [Char] -> Doc
upcase [] = text []
upcase (c:cs) = text $ toUpper c:cs --capitalize first letter of string
lowcase [] = text []
lowcase (c:cs) = text $ toLower c:cs --make first letter lowercase