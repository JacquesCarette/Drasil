{-# OPTIONS -Wall #-} 
module Helpers where

import Text.PrettyPrint
import Data.Char

data OutFormat = TeX
               | Plain
--basic
bslash,dbs,eq,dlr,ast,pls :: Doc
bslash = text "\\"
dbs = bslash <> bslash
eq = text "="
dlr = text "$"
ast = text "*"
pls = text "+"

sq,br :: String -> Doc
sq t = text $ "[" ++ t ++ "]"
br t = text $ "{" ++ t ++ "}"

--TeX
docclass :: String -> String -> Doc
docclass [] brac = bslash <> text "documentclass" <> br brac
docclass sqbrack brac = bslash <> text "documentclass" <> sq sqbrack <> br brac

usepackage :: String -> Doc
usepackage pkg = bslash <> text "usepackage" <> br pkg

exdoc :: String -> String -> Doc
exdoc [] d = bslash <> text "externaldocument" <> br d
exdoc sqbrack d = bslash <> text "externaldocument" <> sq sqbrack <> br d

title :: String -> Doc
title t = bslash <> text "title" <> br t

author :: String -> Doc
author a = bslash <> text "author" <> br a

begin, end, endL, command :: Doc
begin = bslash <> text "begin" <> br "document" $$ bslash <> text "maketitle"
end = bslash <> text "enddocument"
endL = bslash <> text "end" <> br "document"
command = bslash <> text "newcommand"

comm :: String -> String -> String -> Doc
comm b [] []= (command) <> br ("\\" ++ b)
comm b1 b2 [] = (command) <> br ("\\" ++ b1) <> br b2
comm b1 b2 s1 = (command) <> br ("\\" ++ b1) <> sq s1 <> br b2

count :: String -> Doc
count b = bslash <> text "newcounter" <> br b

renewcomm :: String -> String -> Doc
renewcomm b1 b2 = bslash <> text "renewcommand" <> br ("\\" ++ b1) <> br b2

sec :: String -> Doc
sec b= bslash <> text "section*" <> br b

--format strings
upcase, lowcase :: [Char] -> Doc
upcase [] = text []
upcase (c:cs) = text $ toUpper c:cs --capitalize first letter of string
lowcase [] = text []
lowcase (c:cs) = text $ toLower c:cs --make first letter lowercase