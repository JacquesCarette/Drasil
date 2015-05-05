module Helpers where

import Text.PrettyPrint
import Data.Char

--basic
bslash = text "\\"
dbs = bslash <> bslash
sq t = text $ "[" ++ t ++ "]"
br t = text $ "{" ++ t ++ "}"
eq = text "="
dlr = text "$"

--TeX
docclass [] braces = bslash <> text "documentclass" <> br braces
docclass sqbrack braces = bslash <> text "documentclass" <> sq sqbrack <> br braces

usepackage pkg = bslash <> text "usepackage" <> br pkg

exdoc [] d = bslash <> text "externaldocument" <> br d
exdoc sqbrack d = bslash <> text "externaldocument" <> sq sqbrack <> br d

title t = bslash <> text "title" <> br t

author a = bslash <> text "author" <> br a

begin = bslash <> text "begin" <> br "document" $$ bslash <> text "maketitle"
end = bslash <> text "enddocument"
endL = bslash <> text "end" <> br "document"

command = bslash <> text "newcommand"
comm b [] []= (command) <> br ("\\" ++ b)
comm b1 b2 [] = (command) <> br ("\\" ++ b1) <> br b2
comm b1 b2 s1 = (command) <> br ("\\" ++ b1) <> sq s1 <> br b2

count b = bslash <> text "newcounter" <> br b

renewcomm b1 b2 = bslash <> text "renewcommand" <> br ("\\" ++ b1) <> br b2

sec b= bslash <> text "section*" <> br b

--format strings
upcase, lowcase :: [Char] -> Doc
upcase [] = text []
upcase (c:cs) = text $ toUpper c:cs --capitalize first letter of string
lowcase [] = text []
lowcase (c:cs) = text $ toLower c:cs --make first letter lowercase