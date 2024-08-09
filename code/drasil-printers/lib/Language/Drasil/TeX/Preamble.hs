-- | Defines types and functions to help generate common LaTeX preamble.
module Language.Drasil.TeX.Preamble (genPreamble) where

import Data.List (nub)

import Text.PrettyPrint (text)
import Language.Drasil.Printing.LayoutObj (LayoutObj(..))
import Language.Drasil.TeX.Monad (D, vcat, (%%))
import Language.Drasil.TeX.Helpers (docclass, command, command1o, command2, command3,
  usepackage, command0, lbrace, rbrace)
import Language.Drasil.Printing.Helpers (sq, brace)

import Language.Drasil.Config (hyperSettings, fontSize, bibFname)

-- FIXME: this really shouldn't be in code, it should be data!
-- | LaTeX packages.
data Package = AMSMath      -- ^ Improves information structure for mathematical formulas.
             | BookTabs     -- ^ Enhances quality of tables in the document.
             | Caption      -- ^ Customize the captions in floating environments.
             | FullPage     -- ^ Sets margins and page style.
             | Graphics     -- ^ Manipulate graphical elements.
             | HyperRef     -- ^ Handles cross-referencing within the document.
             | Listings     -- ^ Source code printer for LaTeX.
             | Tikz         -- ^ Create graphical elements.
             | Dot2Tex      -- ^ Create better graphs.
             | AdjustBox    -- ^ Adjustable boxed content.
             | AMSsymb      -- ^ Displays bold math sets (reals, naturals, etc.).
--           | Breqn --line breaks long equations automatically
             | FileContents -- ^ Creates .bib file within .tex file.
             | BibLaTeX     -- ^ Reimplementation of bibliography elements.
             | Tabularray   -- ^ Adds auto column width feature for tables.
             | TabularX     -- ^ Adds \arraybackslash
             | Mathtools    -- ^ Line breaks for long fractions and cases.
             | URL          -- ^ Allows for hyperlinks.
             | FontSpec     -- ^ For utf-8 encoding in lualatex.
             | Unicode      -- ^ For unicode-math in lualatex.
             | EnumItem     -- ^ Contol basic list environments.
             | SVG          -- ^ For rendering svg diagrams.
             | Float        -- ^ For enhanced control over placement of figures and tables.
             | Graphicx     -- ^ Resizebox for large expressions.
             | Calc         -- ^ Perform arithmetic operation in LaTeX commands.
             deriving Eq

-- | Adds a 'Package' to the LaTeX document.
addPackage :: Package -> D
addPackage AMSMath   = usepackage "amsmath"
addPackage BookTabs  = usepackage "booktabs"
addPackage Caption   = usepackage "caption"
addPackage FullPage  = usepackage "fullpage"
addPackage Graphics  = usepackage "graphics"
addPackage HyperRef  = usepackage "hyperref" %%
                       command "hypersetup" hyperSettings
addPackage Listings  = usepackage "listings"
addPackage Tikz      = usepackage "tikz" %%
                       command "usetikzlibrary" "arrows.meta, shapes"
addPackage Dot2Tex   = usepackage "dot2texi"
addPackage AdjustBox = usepackage "adjustbox"
addPackage AMSsymb   = usepackage "amssymb"
--addPackage Breqn     = usepackage "breqn"
addPackage FileContents = usepackage "filecontents"
addPackage BibLaTeX  = command1o "usepackage" (Just "backend=bibtex") "biblatex"
addPackage Tabularray = usepackage "tabularray"
addPackage TabularX  = usepackage "tabularx"
addPackage Mathtools = usepackage "mathtools"
addPackage URL       = usepackage "url"
-- Discussed in issue #1819
-- Because we are using LuaLatex, we use fontspec here instead of fontenc
addPackage FontSpec  = usepackage "fontspec"
addPackage Unicode   = usepackage "unicode-math"
addPackage EnumItem  = usepackage "enumitem"
addPackage SVG       = usepackage "svg"
addPackage Float     = usepackage "float"
addPackage Graphicx  = usepackage "graphicx"
addPackage Calc      = usepackage "calc"

-- | Common LaTeX commands.
data Def = Bibliography
         | GreaterThan
         | LessThan
         | SetMathFont
         | SymbDescriptionP1
         | SymbDescriptionP2
         | SaveBox
         | ResizeExpr
         deriving Eq

-- | Define common LaTeX commands.
addDef :: Def -> D
addDef Bibliography  = command "bibliography" bibFname
addDef GreaterThan   = command2 "newcommand" "\\gt" "\\ensuremath >"
addDef LessThan      = command2 "newcommand" "\\lt" "\\ensuremath <"
addDef SetMathFont   = command "setmathfont" "Latin Modern Math"
addDef SymbDescriptionP1 = command3 "newlist" "symbDescription" "description" "1"
addDef SymbDescriptionP2 = command1o "setlist" (Just "symbDescription") "noitemsep, topsep=0pt, parsep=0pt, partopsep=0pt"
addDef SaveBox       = command "newsavebox" "\\mybox"
addDef ResizeExpr    = vcat [
    command "newcommand" "\\resizeExpression" <> pure (sq "2") <> lbrace,
    command2 "savebox" "\\mybox" "$#1$",
    command0 "ifdim" <> command0 "wd" <> command0 "mybox" <> pure (text ">#2") <> command0 "linewidth",
    command3 "resizebox" "#2\\textwidth" "!" ("\\usebox" ++ brace "\\mybox"),
    command0 "else",
    command "usebox" "\\mybox",
    command0 "fi",
    rbrace
  ]

-- | Generates LaTeX document preamble.
genPreamble :: [LayoutObj] -> D
genPreamble los = let (pkgs, defs) = parseDoc los
  in docclass (show fontSize ++ "pt") "article" %%
     vcat (map addPackage pkgs) %% vcat (map addDef defs)

-- | Helper to gather all preamble information.
parseDoc :: [LayoutObj] -> ([Package], [Def])
parseDoc los' = 
  ([FontSpec, FullPage, HyperRef, AMSMath, AMSsymb, Mathtools, Unicode] ++ 
   nub (concatMap fst res)
  , [SetMathFont, GreaterThan, LessThan] ++ nub (concatMap snd res))
  where 
    res = map parseDoc' los'
    parseDoc' :: LayoutObj -> ([Package], [Def])
    parseDoc' Table{} = ([Tabularray,TabularX,BookTabs,Caption], [])
    parseDoc' (HDiv _ slos _) = 
      let res1 = map parseDoc' slos in
      let pp = concatMap fst res1 in
      let dd = concatMap snd res1 in
      (pp, dd)
    parseDoc' (Definition _ ps _) =
      let res1 = concatMap (map parseDoc' . snd) ps in
      let pp = concatMap fst res1 in
      let dd = concatMap snd res1 in
      (Tabularray:TabularX:BookTabs:pp,SymbDescriptionP1:SymbDescriptionP2:dd)
    parseDoc' Figure{}     = ([Graphics,Caption, SVG, Float],[])
    parseDoc' Graph{}      = ([Caption,Tikz,Dot2Tex,AdjustBox],[])
    parseDoc' Bib{}        = ([FileContents,BibLaTeX,URL],[Bibliography])
    parseDoc' Header{}     = ([], [])
    parseDoc' Paragraph{}  = ([], [])
    parseDoc' List{}       = ([EnumItem], [])
    parseDoc' EqnBlock{}   = ([Graphicx, Calc], [SaveBox, ResizeExpr])
    parseDoc' Cell{}       = ([], [])
    parseDoc' CodeBlock{}  = ([], [])
