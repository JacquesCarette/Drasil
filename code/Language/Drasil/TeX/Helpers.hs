module Language.Drasil.TeX.Helpers where

import Text.PrettyPrint (text)
import qualified Text.PrettyPrint as TP
import Control.Applicative (pure)

import Language.Drasil.Config (numberedSections)
import qualified Language.Drasil.Printing.Helpers as H
import Language.Drasil.TeX.Monad

-----------------------------------------------------------------------------
-- Infrastructre for defining commands, environments, etc.
--   (calls to TP should only occur in this section)

-- Make 1-argument command
command :: String -> (String -> D)
command s c = pure $ (H.bslash TP.<> text s) TP.<> H.br c

-- 1-argument command, with optional argument
command1o :: String -> Maybe String -> String -> D
command1o s o c = pure $ (H.bslash TP.<> text s) TP.<>
  (maybe TP.empty H.sq o) TP.<> H.br c

-- 0-argument command
command0 :: String -> D
command0 s = pure $ H.bslash TP.<> text s

-- 2-argument command
command2 :: String -> String -> String -> D
command2 s a0 a1 = pure $ (H.bslash TP.<> text s) TP.<> H.br a0 TP.<> H.br a1

-- Encapsulate environments
mkEnv :: String -> D -> D
mkEnv nm d =
  (pure $ text ("\\begin" ++ H.brace nm)) $+$ 
  d $+$
  (pure $ text ("\\end" ++ H.brace nm))

-- for defining (LaTeX) macros
comm :: String -> String -> Maybe String -> D
comm b1 b2 s1 = command0 "newcommand" <> (pure $ H.br ("\\" ++ b1) TP.<> 
  maybe TP.empty H.sq s1 TP.<> H.br b2)

-- this one is special enough, let this sub-optimal implementation stand
renewcomm :: String -> String -> D
renewcomm b1 b2 = pure $ text "\\renewcommand" TP.<> H.br ("\\" ++ b1) TP.<> H.br b2

-- Useful to have empty 
empty :: D
empty = pure TP.empty

-----------------------------------------------------------------------------
-- Now create standard LaTeX stuff

caption, label, usepackage, title, author, count, includegraphics :: String -> D
caption         = command "caption"
label           = command "label"
usepackage      = command "usepackage"
title           = command "title"
author          = command "author"
count           = command "count"
includegraphics = command "includegraphics"

maketitle, newline :: D
maketitle = command0 "maketitle"
newline = command0 "newline"

code, itemize, enumerate, figure, center, document :: D -> D
code      = mkEnv "lstlisting"
itemize   = mkEnv "itemize"
enumerate = mkEnv "enumerate"
figure    = mkEnv "figure"
center    = mkEnv "center"
document  = mkEnv "document"

docclass, exdoc :: Maybe String -> String -> D
docclass = command1o "documentclass"
exdoc = command1o "externaldocument"

sec :: Int -> String -> D
sec d b1 
  | d < 0 = error "Cannot have section with negative depth"
  | d > 2 = error "Section depth must be from 0-2"
  | otherwise = pure $ 
      H.bslash TP.<> text (concat $ replicate d "sub") TP.<> text "section" 
      TP.<> (if (not numberedSections) then text "*" else TP.empty) TP.<> H.br b1

-- Macro / Command def'n --
--TeX--
srsComms, lpmComms, bullet, counter, ddefnum, ddref, colAw, colBw, arrayS :: D
srsComms = bullet %% counter %% ddefnum %% ddref %% colAw %% colBw %% arrayS
lpmComms = pure $ text ""

counter = count "datadefnum"

bullet  = comm "blt"             "- "                Nothing
ddefnum = comm "ddthedatadefnum" "DD\\thedatadefnum" Nothing
ddref   = comm "ddref"           "DD\\ref{#1}"       (Just "1")
colAw   = comm "colAwidth"       "0.2\\textwidth"    Nothing
colBw   = comm "colBwidth"       "0.73\\textwidth"   Nothing

arrayS  = renewcomm "arraystretch" "1.2"

-- TODO
fraction :: String -> String -> String
fraction n d = "\\frac{" ++ n ++ "}{" ++ d ++ "}"
-- fraction = command2 "frac"
