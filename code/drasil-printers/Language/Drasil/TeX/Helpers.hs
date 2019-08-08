module Language.Drasil.TeX.Helpers where

import Text.PrettyPrint (text)
import qualified Text.PrettyPrint as TP
import Control.Applicative (pure)

import Language.Drasil (MaxWidthPercent)

import Language.Drasil.Config (numberedSections, hyperSettings)
import qualified Language.Drasil.Printing.Helpers as H
import Language.Drasil.TeX.Monad (PrintLaTeX(PL), D, MathContext(Math), ($+$))

--import Language.Drasil.Config (numberedSections, hyperSettings)
--import Language.Drasil.Document (MaxWidthPercent)

-----------------------------------------------------------------------------
-- Infrastructre for defining commands, environments, etc.
--   (calls to TP should only occur in this section)

br, sq, parens, quote :: D -> D
br x = lb <> x <> rb
  where
  lb = pure $ text "{"
  rb = pure $ text "}"
sq x = ls <> x <> rs
  where
  ls = pure $ text "["
  rs = pure $ text "]"
parens x = lp <> x <> rp
  where
  lp = pure $ text "("
  rp = pure $ text ")"
quote x = lq <> x <> rq
  where
  lq = pure $ text "``"
  rq = pure $ text "''"

-- 0-argument command
command0 :: String -> D
command0 s = pure $ H.bslash TP.<> text s

-- Make 1-argument command
command :: String -> String -> D
command s c = pure $ (H.bslash TP.<> text s) TP.<> H.br c

commandD :: String -> D -> D
commandD s c = pure (H.bslash TP.<> text s) <> br c

-- 1-argument command, with optional argument
command1o :: String -> Maybe String -> String -> D
command1o s = maybe (command s) (command1p s)

command1oD :: String -> Maybe D -> D -> D
command1oD s = maybe (commandD s) (command1pD s)

-- 1-argument command with parameter in square brackets
command1p :: String -> String -> String -> D
command1p s p c = pure $ (H.bslash TP.<> text s) TP.<> H.sq p TP.<> H.br c

command1pD :: String -> D -> D -> D
command1pD s p c = pure (H.bslash TP.<> text s) <> sq p <> br c

-- Make LaTeX symbol
texSym :: String -> D
texSym s = pure $ H.bslash TP.<> text s

-- 2-argument command
command2 :: String -> String -> String -> D
command2 s a0 a1 = pure $ (H.bslash TP.<> text s) TP.<> H.br a0 TP.<> H.br a1

command2D :: String -> D -> D -> D
command2D s a0 a1 = pure (H.bslash TP.<> text s) <> br a0 <> br a1

-- 3-argument command
command3 :: String -> String -> String -> String -> D
command3 s a0 a1 a2 = pure $ (H.bslash TP.<> text s) TP.<> H.br a0 TP.<> H.br a1 TP.<> H.br a2

-- Encapsulate environments
mkEnv :: String -> D -> D
mkEnv nm d =
  pure (text ("\\begin" ++ H.brace nm)) $+$ 
  d $+$
  pure (text ("\\end" ++ H.brace nm))

-- Encapsulate environments with argument with braces
mkEnvArgBr :: String -> String -> D -> D
mkEnvArgBr nm args d =
  pure (text ("\\begin" ++ H.brace nm ++ H.brace args)) $+$ 
  d $+$
  pure (text ("\\end" ++ H.brace nm))

-- Encapsulate environments with argument with brackets
mkEnvArgSq :: String -> String -> D -> D
mkEnvArgSq nm args d =
  pure (text ("\\begin" ++ H.brace nm ++ H.sqbrac args)) $+$ 
  d $+$
  pure (text ("\\end" ++ H.brace nm))

-- Makes minipage environment
mkMinipage :: D -> D
mkMinipage d = commandD "vspace" (command0 "baselineskip") $+$
  command0 "noindent" $+$ mkEnvArgBr "minipage" "\\textwidth" d

-- for defining (LaTeX) macros
comm :: String -> String -> Maybe String -> D
comm b1 b2 s1 = command0 "newcommand" <> pure (H.br ("\\" ++ b1) TP.<> 
  maybe TP.empty H.sq s1 TP.<> H.br b2)

-- this one is special enough, let this sub-optimal implementation stand
renewcomm :: String -> String -> D
renewcomm b1 = command2 "renewcommand" ("\\" ++ b1)

-- Useful to have empty 
empty :: D
empty = pure TP.empty

-- For sections
genSec :: Int -> D
genSec d
  | d < 0 = error "Cannot have section with negative depth"
  | d > 3 = error "Section depth must be from 0-2"
  | d == 3 = pure $ H.bslash TP.<> text "paragraph"
  | otherwise = pure $ 
     H.bslash TP.<> text (concat $ replicate d "sub") TP.<> text "section" 
      TP.<> (if not numberedSections then text "*" else TP.empty) 

-- For references
ref, sref, hyperref, externalref, snref :: String -> D -> D
sref            = if numberedSections then ref else hyperref
ref         t x = pure (text $ t ++ "~") <> commandD "ref" x
hyperref    t x = command1pD "hyperref" x (pure (text (t ++ "~")) <> x)
externalref t x = command0 "hyperref" <> br (pure $ text t) <> br empty <>
  br empty <> br x
snref       r   = command1pD "hyperref" (pure (text r))

href :: String -> String -> D
href = command2 "href"

cite :: String -> Maybe D -> D
cite c i = command1oD "cite" i (pure $ text c)

-----------------------------------------------------------------------------
-- Now create standard LaTeX stuff

count, mathbb, usepackage :: String -> D
count      = command "newcounter"
-- changed to command "newcounter" from command "count" (I assume this was
-- what was intended?)
mathbb     = command "mathbb"
usepackage = command "usepackage"

includegraphics :: MaxWidthPercent -> String -> D
includegraphics n = command1p "includegraphics" ("width=" ++ per n ++ "\\textwidth")
  where
    per 100 = ""
    per wp  = show (wp / 100)

author, caption, item, label, title, bold :: D -> D
author  = commandD "author"
caption = commandD "caption"
item    = commandD "item"
label   = commandD "label"
title   = commandD "title"
bold    = commandD "textbf"

item' :: D -> D -> D
item' = command1pD "item"

maketitle, maketoc, newpage, centering :: D
maketitle = command0 "maketitle"
maketoc   = command0 "tableofcontents"
newpage   = command0 "newpage"
centering = command0 "centering"

code, itemize, enumerate, description, figure, center, document, 
  equation, symbDescription :: D -> D
code        = mkEnv "lstlisting"
itemize     = mkEnv "itemize"
enumerate   = mkEnv "enumerate"
description = mkEnv "description"
figure      = mkEnv "figure"
center      = mkEnv "center"
document    = mkEnv "document"
equation    = mkEnv "displaymath" --displays math
symbDescription = mkEnv "symbDescription"

docclass :: String -> String -> D
docclass = command1p "documentclass"

sec :: Int -> D -> D
sec d b1 = genSec d <> br b1

subscript, superscript :: D -> D -> D
subscript   a b = a <> pure H.unders <> br b
superscript a b = a <> pure H.hat    <> br b

-- grave, acute :: Char -> D
-- grave c = (pure $ text "\\`{") <> pure (TP.char c) <> (pure $ text "}")
-- acute c = (pure $ text "\\'{") <> pure (TP.char c) <> (pure $ text "}")

-- Macro / Command def'n --
--TeX--
bullet, counter, ddefnum, ddref, colAw, colBw, arrayS, modcounter, modnum :: D

counter    = count "datadefnum"
modcounter = count "modnum"

bullet  = comm "blt"             "- "                Nothing
ddefnum = comm "ddthedatadefnum" "MG\\thedatadefnum" Nothing
ddref   = comm "ddref"           "MG\\ref{#1}"       (Just "1")
colAw   = comm "colAwidth"       "0.2\\textwidth"    Nothing
colBw   = comm "colBwidth"       "0.73\\textwidth"   Nothing
modnum  = comm "mthemodnum"      "M\\themodnum"      Nothing

arrayS  = renewcomm "arraystretch" "1.2"

-- add newline
newline :: D -> D
newline s = s $+$ pure (text "")

fraction :: D -> D -> D
fraction = command2D "frac"

hyperConfig :: D
hyperConfig = command "hypersetup" hyperSettings

useTikz :: D
useTikz = usepackage "luatex85" $+$ command0 "def" <>
  command "pgfsysdriver" "pgfsys-pdftex.def" $+$
  -- the above is a workaround..  temporary until TeX packages have been fixed
  usepackage "tikz" $+$ command "usetikzlibrary" "arrows.meta" $+$
  command "usetikzlibrary" "graphs" $+$ command "usetikzlibrary" "graphdrawing" $+$
  command "usegdlibrary" "layered"
  
-----------------------------------------------------------------------------
-- This 'belongs' in Monad, but it would make Monad depend on Helpers, which depends
-- on Monad...

-- toEqn is special; it switches to Math, but inserts an equation environment
toEqn :: D -> D
toEqn (PL g) = equation $ PL (\_ -> g Math)

-----------------------------------------------------------------------------
-- Helper(s) for String-Printing in TeX where it varies from HTML/Plaintext

paren, sqbrac :: String -> String
paren x = "\\left(" ++ x ++ "\\right)"
sqbrac x = "\\left[" ++ x ++ "\\right]"
