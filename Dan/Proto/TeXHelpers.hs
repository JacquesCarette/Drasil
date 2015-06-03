{-# OPTIONS -Wall #-} 
module TeXHelpers where
import Text.PrettyPrint.HughesPJ
import Helpers

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

subsec :: String -> Doc
subsec b = bslash <> text "subsection*" <> br b

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
