module Language.Drasil.TeX.Preamble (genPreamble) where

import Data.List (nub)

import Language.Drasil.Printing.LayoutObj (LayoutObj(..))
import Language.Drasil.TeX.Monad (D, vcat, (%%))
import Language.Drasil.TeX.Helpers (docclass, command, command0, command1o, command2, command3, 
  usepackage)

import Language.Drasil.Config (hyperSettings, fontSize, bibFname)

-- FIXME: this really shouldn't be in code, it should be data!
data Package = AMSMath
             | BookTabs
             | Caption
             | FullPage
             | Graphics
             | HyperRef
             | Listings
             | LongTable
             | Tikz
             | Dot2Tex
             | AdjustBox
             | AMSsymb --displays bold math sets (reals, naturals, etc.)
--           | Breqn --line breaks long equations automatically
             | FileContents --creates .bib file within .tex file
             | BibLaTeX
             | Tabu --adds auto column width feature for tables 
             | Mathtools --line breaks for long fractions and cases
             | URL
             | FontSpec -- for utf-8 encoding in lualatex
             | Unicode -- for unicode-math in lualatex
             | EnumItem
             deriving Eq

addPackage :: Package -> D
addPackage AMSMath   = usepackage "amsmath"
addPackage BookTabs  = usepackage "booktabs"
addPackage Caption   = usepackage "caption"
addPackage FullPage  = usepackage "fullpage"
addPackage Graphics  = usepackage "graphics"
addPackage HyperRef  = usepackage "hyperref" %%
                       command "hypersetup" hyperSettings
addPackage Listings  = usepackage "listings"
addPackage LongTable = usepackage "longtable"
addPackage Tikz      = usepackage "tikz" %%
                       command "usetikzlibrary" "arrows.meta, shapes"
addPackage Dot2Tex   = usepackage "dot2texi"
addPackage AdjustBox = usepackage "adjustbox"
addPackage AMSsymb   = usepackage "amssymb"
--addPackage Breqn     = usepackage "breqn"
addPackage FileContents = usepackage "filecontents"
addPackage BibLaTeX  = command1o "usepackage" (Just "backend=bibtex") "biblatex"
addPackage Tabu      = usepackage "tabu"
addPackage Mathtools = usepackage "mathtools"
addPackage URL       = usepackage "url"
-- Discussed in issue #1819
-- Because we are using LuaLatex, we use fontspec here instead of fontenc
addPackage FontSpec  = usepackage "fontspec"
addPackage Unicode   = usepackage "unicode-math"
addPackage EnumItem  = usepackage "enumitem"

data Def = Bibliography
         | TabuLine
         | GreaterThan
         | LessThan
         | SetMathFont
         | SymbDescriptionP1
         | SymbDescriptionP2
         deriving Eq

addDef :: Def -> D
addDef Bibliography  = command "bibliography" bibFname
addDef GreaterThan   = command2 "newcommand" "\\gt" "\\ensuremath >"
addDef LessThan      = command2 "newcommand" "\\lt" "\\ensuremath <"
addDef TabuLine      = command0 "global\\tabulinesep=1mm"
addDef SetMathFont   = command "setmathfont" "Latin Modern Math"
addDef SymbDescriptionP1 = command3 "newlist" "symbDescription" "description" "1"
addDef SymbDescriptionP2 = command1o "setlist" (Just "symbDescription") "noitemsep, topsep=0pt, parsep=0pt, partopsep=0pt"

genPreamble :: [LayoutObj] -> D
genPreamble los = let (pkgs, defs) = parseDoc los
  in docclass (show fontSize ++ "pt") "article" %%
     vcat (map addPackage pkgs) %% vcat (map addDef defs)

parseDoc :: [LayoutObj] -> ([Package], [Def])
parseDoc los' = 
  ([FontSpec, FullPage, HyperRef, AMSMath, AMSsymb, Mathtools, Unicode] ++ 
   nub (concatMap fst res)
  , [SetMathFont, GreaterThan, LessThan] ++ nub (concatMap snd res))
  where 
    res = map parseDoc' los'
    parseDoc' :: LayoutObj -> ([Package], [Def])
    parseDoc' Table{} = ([Tabu,LongTable,BookTabs,Caption], [TabuLine])
    parseDoc' (HDiv _ slos _) = 
      let res1 = map parseDoc' slos in
      let pp = concatMap fst res1 in
      let dd = concatMap snd res1 in
      (pp, dd)
    parseDoc' (Definition _ ps _) =
      let res1 = concatMap (map parseDoc' . snd) ps in
      let pp = concatMap fst res1 in
      let dd = concatMap snd res1 in
      (Tabu:LongTable:BookTabs:pp,SymbDescriptionP1:SymbDescriptionP2:TabuLine:dd)
    parseDoc' Figure{}     = ([Graphics,Caption],[])
    parseDoc' Graph{}      = ([Caption,Tikz,Dot2Tex,AdjustBox],[])
    parseDoc' Bib{}        = ([FileContents,BibLaTeX,URL],[Bibliography])
    parseDoc' Header{}     = ([], [])
    parseDoc' Paragraph{}  = ([], [])
    parseDoc' List{}       = ([EnumItem], [])
    parseDoc' EqnBlock{}   = ([], [])
