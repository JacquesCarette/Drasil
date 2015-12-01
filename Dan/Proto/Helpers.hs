{-# OPTIONS -Wall #-} 
module Helpers where

import Text.PrettyPrint
import Data.Char
import Config (tableWidth)

--Table making help
lAndDim :: [[a]] -> String
lAndDim [] = error "No fields provided"
lAndDim [f]  = concat (replicate ((length f)-1) "l ") ++ "p" ++ 
  brace (show tableWidth ++ "cm")
lAndDim _ = error "Unimplemented use of lAndDim in Helpers."
  
--basic docs
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

--basic plaintext manipulation
paren,brace,dollar :: String -> String
paren  = \x -> "(" ++ x ++ ")"
brace  = \x -> "{" ++ x ++ "}"
dollar = \x -> "$" ++ x ++ "$"


--format strings
upcase, lowcase :: [Char] -> Doc
upcase [] = text []
upcase (c:cs) = text $ toUpper c:cs --capitalize first letter of string
lowcase [] = text []
lowcase (c:cs) = text $ toLower c:cs --make first letter lowercase

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

begin, endS, endL, command :: Doc
begin = bslash <> text "begin" <> br "document" $$ bslash <> text "maketitle"
endS = bslash <> text "enddocument"
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

subsec :: String -> Doc
subsec b = bslash <> text "subsection*" <> br b

newline :: Doc
newline = bslash <> text "newline"

-- Macro / Command def'n --
--TeX--
srsComms, lpmComms, bullet, counter, ddefnum, ddref, colAw, colBw, arrayS :: Doc
srsComms = bullet $$ counter $$ ddefnum $$ ddref $$ colAw $$ colBw $$ arrayS
lpmComms = text ""

bullet = comm "blt" "- " []
counter = count "datadefnum"
ddefnum = comm "ddthedatadefnum" "DD\\thedatadefnum" []
ddref = comm "ddref" "DD\\ref{#1}" "1"
colAw = comm "colAwidth" "0.2\\textwidth" []
colBw = comm "colBwidth" "0.73\\textwidth" []
arrayS = renewcomm "arraystretch" "1.2"

fraction :: String -> String -> String
fraction a b = "\\frac{" ++ a ++ "}{" ++ b ++ "}"
