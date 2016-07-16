module Language.Drasil.TeX.Helpers where

import Text.PrettyPrint (text)
import qualified Text.PrettyPrint as TP
import Control.Applicative (pure)

import Language.Drasil.Config (numberedSections)
import qualified Language.Drasil.Printing.Helpers as H
import Language.Drasil.TeX.Monad

-- Encapsulate some commands
command :: String -> (String -> D)
command s c = pure $ (H.bslash TP.<> text s) TP.<> H.br c

caption, label, usepackage, title, author, count, includegraphics :: String -> D
caption         = command "caption"
label           = command "label"
usepackage      = command "usepackage"
title           = command "title"
author          = command "author"
count           = command "count"
includegraphics = command "includegraphics"

command0 :: String -> D
command0 s = pure $ H.bslash TP.<> text s

maketitle :: D
maketitle = command0 "maketitle"

-- Encapsulate environments
mkEnv :: String -> D -> D
mkEnv nm d =
  (pure $ text ("\\begin" ++ H.brace nm)) $+$ 
  d $+$
  (pure $ text ("\\end" ++ H.brace nm))

code, itemize, enumerate, figure, center, document :: D -> D
code      = mkEnv "lstlisting"
itemize   = mkEnv "itemize"
enumerate = mkEnv "enumerate"
figure    = mkEnv "figure"
center    = mkEnv "center"
document  = mkEnv "document"

docclass :: String -> String -> D
docclass [] brac      = pure $ text "\\documentclass" TP.<> H.br brac
docclass sqbrack brac = pure $ text "\\documentclass" TP.<> H.sq sqbrack TP.<> H.br brac

exdoc :: String -> String -> D
exdoc [] d      = pure $ text "\\externaldocument" TP.<> H.br d
exdoc sqbrack d = pure $ text "\\externaldocument" TP.<> H.sq sqbrack TP.<> H.br d

newcommand :: D
newcommand = pure $ text "\\newcommand"

comm :: String -> String -> String -> D
comm b1 [] [] = newcommand <> (pure $ H.br ("\\" ++ b1))
comm b1 b2 [] = newcommand <> (pure $ H.br ("\\" ++ b1) TP.<> H.br b2)
comm b1 b2 s1 = newcommand <> (pure $ H.br ("\\" ++ b1) TP.<> H.sq s1 TP.<> H.br b2)

renewcomm :: String -> String -> D
renewcomm b1 b2 = pure $ text "\\renewcommand" TP.<> H.br ("\\" ++ b1) TP.<> H.br b2

sec :: Int -> String -> D
sec d b1 
  | d < 0 = error "Cannot have section with negative depth"
  | d > 2 = error "Section depth must be from 0-2"
  | otherwise = pure $ 
      H.bslash TP.<> text (concat $ replicate d "sub") TP.<> text "section" 
      TP.<> (if (not numberedSections) then text "*" else TP.empty) TP.<> H.br b1

newline :: D
newline = pure $ text "\\newline"

-- Macro / Command def'n --
--TeX--
srsComms, lpmComms, bullet, counter, ddefnum, ddref, colAw, colBw, arrayS :: D
srsComms = bullet %% counter %% ddefnum %% ddref %% colAw %% colBw %% arrayS
lpmComms = pure $ text ""

bullet  = comm "blt" "- " []
counter = count "datadefnum"
ddefnum = comm "ddthedatadefnum" "DD\\thedatadefnum" []
ddref   = comm "ddref" "DD\\ref{#1}" "1"
colAw   = comm "colAwidth" "0.2\\textwidth" []
colBw   = comm "colBwidth" "0.73\\textwidth" []
arrayS  = renewcomm "arraystretch" "1.2"

fraction :: String -> String -> String
fraction n d = "\\frac{" ++ n ++ "}{" ++ d ++ "}"
