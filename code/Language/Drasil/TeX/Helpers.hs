module Language.Drasil.TeX.Helpers where

import Text.PrettyPrint

import Language.Drasil.Config (tableWidth, numberedSections)
import Language.Drasil.Printing.Helpers

caption, label :: String -> Doc
caption c = text "\\caption" <> br c
label l = text "\\label" <> br l

--Table making help
lAndDim :: [[a]] -> String
lAndDim []  = error "No fields provided"
lAndDim [f] = concat (replicate ((length f)-1) "l ") ++ "p" ++ 
  brace (show tableWidth ++ "cm")
lAndDim _   = error "Unimplemented use of lAndDim in Helpers."
  
docclass :: String -> String -> Doc
docclass [] brac      = bslash <> text "documentclass" <> br brac
docclass sqbrack brac = bslash <> text "documentclass" <> sq sqbrack <> br brac

usepackage :: String -> Doc
usepackage pkg = bslash <> text "usepackage" <> br pkg

exdoc :: String -> String -> Doc
exdoc [] d      = bslash <> text "externaldocument" <> br d
exdoc sqbrack d = bslash <> text "externaldocument" <> sq sqbrack <> br d

title :: String -> Doc
title t = bslash <> text "title" <> br t

author :: String -> Doc
author a = bslash <> text "author" <> br a

begin, endL, command :: Doc
begin   = bslash <> text "begin" <> br "document" $$ bslash <> text "maketitle"
endL    = bslash <> text "end" <> br "document"
command = bslash <> text "newcommand"

comm :: String -> String -> String -> Doc
comm b1 [] [] = (command) <> br ("\\" ++ b1)
comm b1 b2 [] = (command) <> br ("\\" ++ b1) <> br b2
comm b1 b2 s1 = (command) <> br ("\\" ++ b1) <> sq s1 <> br b2

count :: String -> Doc
count b1 = bslash <> text "newcounter" <> br b1

renewcomm :: String -> String -> Doc
renewcomm b1 b2 = bslash <> text "renewcommand" <> br ("\\" ++ b1) <> br b2

sec :: Int -> String -> Doc
sec d b1 
  | d < 0 = error "Cannot have section with negative depth"
  | d > 2 = error "Section depth must be from 0-2"
  | otherwise = bslash <> text (concat $ replicate d "sub") <> text "section" 
      <> (if (not numberedSections) then text "*" else empty) <> br b1

newline :: Doc
newline = bslash <> text "newline"

-- Macro / Command def'n --
--TeX--
srsComms, lpmComms, bullet, counter, ddefnum, ddref, colAw, colBw, arrayS :: Doc
srsComms = bullet $$ counter $$ ddefnum $$ ddref $$ colAw $$ colBw $$ arrayS
lpmComms = text ""

bullet  = comm "blt" "- " []
counter = count "datadefnum"
ddefnum = comm "ddthedatadefnum" "DD\\thedatadefnum" []
ddref   = comm "ddref" "DD\\ref{#1}" "1"
colAw   = comm "colAwidth" "0.2\\textwidth" []
colBw   = comm "colBwidth" "0.73\\textwidth" []
arrayS  = renewcomm "arraystretch" "1.2"

fraction :: String -> String -> String
fraction n d = "\\frac{" ++ n ++ "}{" ++ d ++ "}"

b,e :: String -> Doc
b s = bslash <> text ("begin" ++ brace s)
e s = bslash <> text ("end" ++ brace s)
